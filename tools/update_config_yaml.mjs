#!/usr/bin/env node

import fs from "node:fs/promises";
import process from "node:process";
import { constants as fsConstants } from "node:fs";

function fail(message) {
  process.stderr.write(`${message}\n`);
  process.exit(1);
}

async function readStdin() {
  const chunks = [];
  for await (const chunk of process.stdin) {
    chunks.push(chunk);
  }
  return Buffer.concat(chunks).toString("utf8");
}

async function loadPayload() {
  let payload;
  try {
    payload = JSON.parse(await readStdin());
  } catch (error) {
    fail(`Invalid config payload: ${error.message}`);
  }

  if (payload === null || Array.isArray(payload) || typeof payload !== "object") {
    fail("Config payload must be a JSON object.");
  }

  for (const key of Object.keys(payload)) {
    if (typeof key !== "string") {
      fail("Config payload keys must be strings.");
    }
  }

  return payload;
}

async function loadDocument(path, yamlApi) {
  let source;
  try {
    source = await fs.readFile(path, "utf8");
  } catch (error) {
    if (error && error.code === "ENOENT") {
      fail(`Config file not found: ${path}`);
    }
    fail(`Failed to read config.yaml: ${error.message}`);
  }

  let document;
  try {
    document = yamlApi.parseDocument(source, {
      keepSourceTokens: true,
      prettyErrors: false,
    });
  } catch (error) {
    fail(`Failed to parse config.yaml: ${error.message}`);
  }

  if (document.errors.length > 0) {
    fail(`Failed to parse config.yaml: ${document.errors[0].message}`);
  }

  if (!yamlApi.isMap(document.contents)) {
    fail("config.yaml must be a single-document top-level mapping.");
  }

  return document;
}

async function writeDocument(path, document) {
  const rendered = String(document);
  const originalMode = await fs
    .stat(path)
    .then((stat) => stat.mode)
    .catch(() => null);

  const tempPath = `${path}.${process.pid}.tmp`;
  try {
    await fs.writeFile(tempPath, rendered, { encoding: "utf8" });
    if (originalMode !== null) {
      await fs.chmod(tempPath, originalMode);
    }
    await fs.rename(tempPath, path);
  } catch (error) {
    await fs.rm(tempPath, { force: true }).catch(() => {});
    fail(`Failed to write config.yaml: ${error.message}`);
  }
}

async function loadYamlApi() {
  try {
    await fs.access(new URL("./node_modules/yaml", import.meta.url), fsConstants.F_OK);
  } catch {
    fail(
      "The Node 'yaml' package is required to update config.yaml. " +
        "Install it with: npm install --prefix tools"
    );
  }

  try {
    return await import("yaml");
  } catch (error) {
    fail(`Failed to load the Node 'yaml' package: ${error.message}`);
  }
}

async function main() {
  if (process.argv.length !== 3) {
    fail("Usage: update_config_yaml.mjs <config-path>");
  }

  const yamlApi = await loadYamlApi();
  const path = process.argv[2];
  const payload = await loadPayload();
  const document = await loadDocument(path, yamlApi);

  for (const [key, value] of Object.entries(payload)) {
    document.set(key, value);
  }

  await writeDocument(path, document);
}

await main();
