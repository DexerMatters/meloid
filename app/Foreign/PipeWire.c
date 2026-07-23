#include <math.h>
#include <pthread.h>
#include <stdint.h>
#include <stdatomic.h>
#include <stdio.h>
#include <string.h>

#include <spa/param/latency-utils.h>
#include <pipewire/filter.h>
#include <pipewire/pipewire.h>

#define BANDS 18
#define EQ_GAIN_LIMIT_DB 20.0
#define SPECTRUM_BANDS 64
#define SPECTRUM_SAMPLES 2048

struct biquad {
  float b0, b1, b2, a1, a2;
  float x1, x2, y1, y2;
};

struct spectrum_sample {
  _Atomic unsigned long sequence;
  _Atomic float channels[2];
};

struct bridge {
  struct pw_main_loop *loop;
  struct pw_filter *filter;
  void *inputs[2];
  void *outputs[2];
  pthread_t thread;
  _Atomic bool running;
  _Atomic unsigned gains_version;
  _Atomic double gains[BANDS];
  struct spectrum_sample samples[SPECTRUM_SAMPLES];
  _Atomic unsigned long sample_index;
  unsigned applied_version;
  _Atomic double rate;
  struct biquad filters[2][BANDS];
  double spectrum_window[SPECTRUM_SAMPLES];
  double spectrum_window_sum;
  char error[256];
};

static struct bridge bridge;
static const double frequencies[BANDS] = {
  55, 77, 110, 156, 220, 311, 440, 622, 880,
  1200, 1800, 2500, 3500, 5000, 7000, 10000, 14000, 20000
};

static void set_error(const char *message) {
  snprintf(bridge.error, sizeof(bridge.error), "%s", message);
}

const char *meloid_eq_error(void) {
  return bridge.error;
}

static void rebuild_filters(struct bridge *data, double rate) {
  unsigned version = atomic_load_explicit(&data->gains_version, memory_order_acquire);
  if (version == data->applied_version && rate == atomic_load_explicit(&data->rate, memory_order_relaxed))
    return;

  atomic_store_explicit(&data->rate, rate, memory_order_relaxed);
  data->applied_version = version;
  for (unsigned band = 0; band < BANDS; band++) {
    double gain = atomic_load_explicit(&data->gains[band], memory_order_relaxed);
    double omega = 2.0 * M_PI * fmin(frequencies[band], rate * 0.45) / rate;
    double alpha = sin(omega) / (2.0 * 2.871);
    double amplitude = pow(10.0, gain / 40.0);
    double a0 = 1.0 + alpha / amplitude;
    float b0 = (float) ((1.0 + alpha * amplitude) / a0);
    float b1 = (float) ((-2.0 * cos(omega)) / a0);
    float b2 = (float) ((1.0 - alpha * amplitude) / a0);
    float a1 = (float) ((-2.0 * cos(omega)) / a0);
    float a2 = (float) ((1.0 - alpha / amplitude) / a0);
    for (unsigned channel = 0; channel < 2; channel++) {
      struct biquad *filter = &data->filters[channel][band];
      filter->b0 = b0;
      filter->b1 = b1;
      filter->b2 = b2;
      filter->a1 = a1;
      filter->a2 = a2;
    }
  }
}

static inline float process_band(struct biquad *filter, float sample) {
  float output = filter->b0 * sample + filter->b1 * filter->x1 + filter->b2 * filter->x2
    - filter->a1 * filter->y1 - filter->a2 * filter->y2;
  filter->x2 = filter->x1;
  filter->x1 = sample;
  filter->y2 = filter->y1;
  filter->y1 = output;
  return output;
}

static void process(void *userdata, struct spa_io_position *position) {
  struct bridge *data = userdata;
  uint32_t frames =
    position->clock.duration > UINT32_MAX ? UINT32_MAX : (uint32_t) position->clock.duration;
  float *in_left = pw_filter_get_dsp_buffer(data->inputs[0], frames);
  float *in_right = pw_filter_get_dsp_buffer(data->inputs[1], frames);
  float *out_left = pw_filter_get_dsp_buffer(data->outputs[0], frames);
  float *out_right = pw_filter_get_dsp_buffer(data->outputs[1], frames);
  if (!out_left && !out_right)
    return;

  double rate = position->clock.rate.num ? (double) position->clock.rate.denom / position->clock.rate.num : 48000.0;
  rebuild_filters(data, rate);
  for (uint32_t frame = 0; frame < frames; frame++) {
    float left = in_left ? in_left[frame] : 0.0f;
    float right = in_right ? in_right[frame] : 0.0f;
    for (unsigned band = 0; band < BANDS; band++) {
      left = process_band(&data->filters[0][band], left);
      right = process_band(&data->filters[1][band], right);
    }
    left = isfinite(left) ? fmaxf(-1.0f, fminf(1.0f, left)) : 0.0f;
    right = isfinite(right) ? fmaxf(-1.0f, fminf(1.0f, right)) : 0.0f;
    if (out_left)
      out_left[frame] = left;
    if (out_right)
      out_right[frame] = right;

    unsigned long index = atomic_load_explicit(&data->sample_index, memory_order_relaxed);
    struct spectrum_sample *sample = &data->samples[index % SPECTRUM_SAMPLES];
    atomic_store_explicit(&sample->sequence, 0, memory_order_release);
    atomic_store_explicit(&sample->channels[0], left, memory_order_relaxed);
    atomic_store_explicit(&sample->channels[1], right, memory_order_relaxed);
    atomic_store_explicit(&sample->sequence, index + 1, memory_order_release);
    atomic_store_explicit(&data->sample_index, index + 1, memory_order_release);
  }
}

static const struct pw_filter_events filter_events = {
  PW_VERSION_FILTER_EVENTS,
  .process = process,
};

static void *run_loop(void *userdata) {
  pw_main_loop_run(((struct bridge *) userdata)->loop);
  return NULL;
}

int meloid_eq_start(void) {
  if (atomic_load_explicit(&bridge.running, memory_order_acquire))
    return 0;

  memset(&bridge.filters, 0, sizeof(bridge.filters));
  bridge.applied_version = atomic_load_explicit(&bridge.gains_version, memory_order_relaxed) - 1;
  atomic_store_explicit(&bridge.rate, 0.0, memory_order_relaxed);
  atomic_store_explicit(&bridge.sample_index, 0, memory_order_relaxed);
  bridge.spectrum_window_sum = 0.0;
  for (unsigned sample = 0; sample < SPECTRUM_SAMPLES; sample++) {
    atomic_store_explicit(&bridge.samples[sample].sequence, 0, memory_order_relaxed);
    atomic_store_explicit(&bridge.samples[sample].channels[0], 0.0f, memory_order_relaxed);
    atomic_store_explicit(&bridge.samples[sample].channels[1], 0.0f, memory_order_relaxed);
    bridge.spectrum_window[sample] =
      0.5 * (1.0 - cos(2.0 * M_PI * sample / (SPECTRUM_SAMPLES - 1)));
    bridge.spectrum_window_sum += bridge.spectrum_window[sample];
  }
  bridge.error[0] = '\0';
  pw_init(NULL, NULL);
  bridge.loop = pw_main_loop_new(NULL);
  if (!bridge.loop) {
    set_error("Failed to create the PipeWire main loop");
    return -1;
  }
  bridge.filter = pw_filter_new_simple(
    pw_main_loop_get_loop(bridge.loop),
    "meloid-eq-filter",
    pw_properties_new(
      PW_KEY_MEDIA_TYPE, "Audio",
      PW_KEY_MEDIA_CATEGORY, "Filter",
      PW_KEY_MEDIA_ROLE, "DSP",
      PW_KEY_NODE_NAME, "meloid_eq_filter",
      PW_KEY_NODE_DESCRIPTION, "Meloid EQ",
      PW_KEY_NODE_AUTOCONNECT, "false",
      PW_KEY_NODE_DRIVER, "false",
      PW_KEY_NODE_PAUSE_ON_IDLE, "true",
      PW_KEY_NODE_VIRTUAL, "false",
      PW_KEY_NODE_DONT_RECONNECT, "true",
      NULL),
    &filter_events,
    &bridge);
  if (!bridge.filter) {
    set_error("Failed to create the PipeWire EQ filter");
    pw_main_loop_destroy(bridge.loop);
    bridge.loop = NULL;
    return -1;
  }

  const char *channels[] = { "FL", "FR" };
  const char *inputs[] = { "input_FL", "input_FR" };
  const char *outputs[] = { "output_FL", "output_FR" };
  for (unsigned channel = 0; channel < 2; channel++) {
    bridge.inputs[channel] = pw_filter_add_port(
      bridge.filter, PW_DIRECTION_INPUT, PW_FILTER_PORT_FLAG_MAP_BUFFERS, 0,
      pw_properties_new(PW_KEY_FORMAT_DSP, "32 bit float mono audio", PW_KEY_PORT_NAME, inputs[channel], PW_KEY_AUDIO_CHANNEL, channels[channel], NULL), NULL, 0);
    bridge.outputs[channel] = pw_filter_add_port(
      bridge.filter, PW_DIRECTION_OUTPUT, PW_FILTER_PORT_FLAG_MAP_BUFFERS, 0,
      pw_properties_new(PW_KEY_FORMAT_DSP, "32 bit float mono audio", PW_KEY_PORT_NAME, outputs[channel], PW_KEY_AUDIO_CHANNEL, channels[channel], NULL), NULL, 0);
  }
  if (!bridge.inputs[0] || !bridge.inputs[1] || !bridge.outputs[0] || !bridge.outputs[1]) {
    set_error("Failed to create the PipeWire EQ ports");
    pw_filter_destroy(bridge.filter);
    pw_main_loop_destroy(bridge.loop);
    bridge.filter = NULL;
    bridge.loop = NULL;
    return -1;
  }

  uint8_t buffer[256];
  struct spa_pod_builder builder = SPA_POD_BUILDER_INIT(buffer, sizeof(buffer));
  const struct spa_pod *params[] = {
    spa_process_latency_build(&builder, SPA_PARAM_ProcessLatency, &SPA_PROCESS_LATENCY_INFO_INIT(.ns = 10 * SPA_NSEC_PER_MSEC)),
  };
  if (pw_filter_connect(bridge.filter, PW_FILTER_FLAG_NONE, params, 1) < 0) {
    set_error("Failed to connect the PipeWire EQ filter");
    pw_filter_destroy(bridge.filter);
    pw_main_loop_destroy(bridge.loop);
    bridge.filter = NULL;
    bridge.loop = NULL;
    return -1;
  }
  atomic_store_explicit(&bridge.running, true, memory_order_release);
  if (pthread_create(&bridge.thread, NULL, run_loop, &bridge) != 0) {
    atomic_store_explicit(&bridge.running, false, memory_order_release);
    set_error("Failed to start the PipeWire EQ thread");
    pw_filter_destroy(bridge.filter);
    pw_main_loop_destroy(bridge.loop);
    bridge.filter = NULL;
    bridge.loop = NULL;
    return -1;
  }
  return 0;
}

void meloid_eq_stop(void) {
  if (!atomic_exchange_explicit(&bridge.running, false, memory_order_acq_rel))
    return;
  pw_main_loop_quit(bridge.loop);
  pthread_join(bridge.thread, NULL);
  pw_filter_destroy(bridge.filter);
  pw_main_loop_destroy(bridge.loop);
  bridge.filter = NULL;
  bridge.loop = NULL;
}

int meloid_eq_set_gains(const double *gains, size_t count) {
  if (count != BANDS) {
    set_error("EQ gain count does not match the filter bands");
    return -1;
  }
  for (unsigned band = 0; band < BANDS; band++) {
    if (!isfinite(gains[band]) || fabs(gains[band]) > EQ_GAIN_LIMIT_DB) {
      set_error("EQ gains must be finite and between -20 and +20 dB");
      return -1;
    }
  }
  for (unsigned band = 0; band < BANDS; band++)
    atomic_store_explicit(&bridge.gains[band], gains[band], memory_order_relaxed);
  atomic_fetch_add_explicit(&bridge.gains_version, 1, memory_order_release);
  return 0;
}

int meloid_eq_spectrum(double *levels, size_t count) {
  if (!atomic_load_explicit(&bridge.running, memory_order_acquire) || count < SPECTRUM_BANDS)
    return 0;
  float samples[2][SPECTRUM_SAMPLES];
  bool copied = false;
  for (unsigned attempt = 0; attempt < 3 && !copied; attempt++) {
    unsigned long end = atomic_load_explicit(&bridge.sample_index, memory_order_acquire);
    if (end < SPECTRUM_SAMPLES)
      return 0;
    copied = true;
    for (unsigned offset = 0; offset < SPECTRUM_SAMPLES; offset++) {
      unsigned long absolute = end - SPECTRUM_SAMPLES + offset;
      struct spectrum_sample *sample = &bridge.samples[absolute % SPECTRUM_SAMPLES];
      unsigned long before = atomic_load_explicit(&sample->sequence, memory_order_acquire);
      if (before != absolute + 1) {
        copied = false;
        break;
      }
      samples[0][offset] = atomic_load_explicit(&sample->channels[0], memory_order_relaxed);
      samples[1][offset] = atomic_load_explicit(&sample->channels[1], memory_order_relaxed);
      if (atomic_load_explicit(&sample->sequence, memory_order_acquire) != before) {
        copied = false;
        break;
      }
    }
  }
  if (!copied)
    return 0;

  double rate = atomic_load_explicit(&bridge.rate, memory_order_relaxed);
  rate = isfinite(rate) && rate > 1000.0 ? rate : 48000.0;

  for (unsigned band = 0; band < SPECTRUM_BANDS; band++) {
    double frequency = 30.0 * pow(18000.0 / 30.0, ((double) band + 0.5) / SPECTRUM_BANDS);
    double phase_step = 2.0 * M_PI * frequency / rate;
    double step_real = cos(phase_step);
    double step_imaginary = sin(phase_step);
    double oscillator_real = 1.0;
    double oscillator_imaginary = 0.0;
    double real[2] = { 0.0, 0.0 };
    double imaginary[2] = { 0.0, 0.0 };
    for (unsigned sample = 0; sample < SPECTRUM_SAMPLES; sample++) {
      for (unsigned channel = 0; channel < 2; channel++) {
        double value = samples[channel][sample] * bridge.spectrum_window[sample];
        real[channel] += value * oscillator_real;
        imaginary[channel] += value * oscillator_imaginary;
      }
      double next_real =
        oscillator_real * step_real - oscillator_imaginary * step_imaginary;
      oscillator_imaginary =
        oscillator_real * step_imaginary + oscillator_imaginary * step_real;
      oscillator_real = next_real;
    }
    double power =
      0.5
      * (real[0] * real[0] + imaginary[0] * imaginary[0]
        + real[1] * real[1] + imaginary[1] * imaginary[1]);
    double amplitude = 2.0 * sqrt(power) / bridge.spectrum_window_sum;
    levels[band] = fmax(-90.0, 20.0 * log10(fmax(1.0e-9, amplitude)));
  }
  return 1;
}
