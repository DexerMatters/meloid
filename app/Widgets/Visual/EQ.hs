-- | This module provides widgets for the equalizer curve.
module Widgets.Visual.EQ (
  EQCurveVisualizer (..),
) where

import Brick qualified as B
import Data.List
import Data.Ord
import GHC.Bits
import Graphics.Vty.Attributes qualified as A
import Graphics.Vty.Image qualified as I
import Lens.Micro
import Types
import Utils (clampValue, eqGainBarNudgeLimitDb, formatFrequencyLabel)
import Widgets.Elements.Common (ElementNode (..), ElementPath, pathVariant)

{- | This widget is a visualizer for the equalizer curve.
Its state is associated with EQ config and current EQ ID.
-}
data EQCurveVisualizer = EQCurveVisualizer ElementPath

data EQPalette = EQPalette
  { eqDefaultAttr :: A.Attr
  , eqMutedAttr :: A.Attr
  , eqAccentAttr :: A.Attr
  , eqAccentBoldAttr :: A.Attr
  , eqPrimaryBoldAttr :: A.Attr
  }

instance Drawable St EQCurveVisualizer where
  draw _ st = B.Widget B.Greedy B.Greedy $ do
    ctx <- B.getContext
    palette <- lookupEQPalette
    let w = ctx ^. B.availWidthL
        h = ctx ^. B.availHeightL
        eq = st ^. stCurrentEQ
    B.render . B.raw $ renderEQCurveImage palette w h eq
  parent (EQCurveVisualizer path) = Just . ParentName . mName $ ElementNode path
  variant (EQCurveVisualizer path) = pathVariant path

lookupEQPalette :: B.RenderM n EQPalette
lookupEQPalette =
  EQPalette
    <$> B.lookupAttrName (B.attrName "eqDefault")
    <*> B.lookupAttrName (B.attrName "eqMuted")
    <*> B.lookupAttrName (B.attrName "eqAccent")
    <*> B.lookupAttrName (B.attrName "eqAccentBold")
    <*> B.lookupAttrName (B.attrName "eqPrimaryBold")

renderEQCurveImage :: EQPalette -> Int -> Int -> EQConfigValue -> I.Image
renderEQCurveImage palette width height config =
  case axisHeight of
    0 -> plotImage
    _ -> I.vertCat [plotImage, renderEQAxisImage palette w config]
 where
  w = max 1 width
  h = max 1 height
  axisHeight
    | h > 1 = 1
    | otherwise = 0
  plotImage = renderEQPlotImage palette w (h - axisHeight) config

renderEQPlotImage :: EQPalette -> Int -> Int -> EQConfigValue -> I.Image
renderEQPlotImage palette width height config =
  I.vertCat
    [ I.horizCat [renderBraillePlotCell palette cellX cellY curveYs zeroDbY | cellX <- [0 .. charWidth - 1]]
    | cellY <- [0 .. charHeight - 1]
    ]
 where
  bandFrequencies = map (^. eqBandFrequencyHz) (config ^. eqBands)
  charWidth = max 1 width
  charHeight = max 1 height
  plotWidth = charWidth * 2
  plotHeight = charHeight * 4
  responses = [eqResponseDb (frequencyAt bandFrequencies plotWidth x) | x <- [0 .. plotWidth - 1]]
  curveYs = map (dbToY) responses
  zeroDbY = dbToY 0

  rangeDb =
    max eqGainBarNudgeLimitDb . min 36 . max 9 $
      maximum (0 : map abs responses) + 3

  dbToY db =
    clampValue 0 (plotHeight - 1) . round $
      (rangeDb - clampValue (-rangeDb) rangeDb db)
        / (2 * rangeDb)
        * fromIntegral (plotHeight - 1)

  eqResponseDb frequency =
    finiteOrZero $
      config ^. eqPreampDb
        + sum (map (`bandResponseDb` frequency) (config ^. eqBands))

  bandResponseDb band frequency =
    finiteOrZero $
      biquadMagnitudeDb frequency (bandBiquadCoefficients band)

  biquadMagnitudeDb frequency coeffs =
    20 * logBase 10 magnitude
   where
    w = 2 * pi * clampValue 1 (sampleRate / 2 - 1) frequency / sampleRate
    numRe = bqB0 coeffs + bqB1 coeffs * cos w + bqB2 coeffs * cos (2 * w)
    numIm = -(bqB1 coeffs * sin w + bqB2 coeffs * sin (2 * w))
    denRe = bqA0 coeffs + bqA1 coeffs * cos w + bqA2 coeffs * cos (2 * w)
    denIm = -(bqA1 coeffs * sin w + bqA2 coeffs * sin (2 * w))
    num = sqrt (numRe * numRe + numIm * numIm)
    den = max 1.0e-12 (sqrt (denRe * denRe + denIm * denIm))
    magnitude = max 1.0e-12 (num / den)

  frequencyAt frequencies w x =
    case frequencies of
      [] -> 1000
      [frequency] -> frequency
      _ -> exp (log leftFrequency + t * (log rightFrequency - log leftFrequency))
   where
    segmentCount = length frequencies - 1
    position
      | w <= 1 = 0
      | otherwise =
          fromIntegral (clampValue 0 (w - 1) x)
            * fromIntegral segmentCount
            / fromIntegral (w - 1)
    leftIndex = min (length frequencies - 1) (floor position)
    rightIndex = min (length frequencies - 1) (leftIndex + 1)
    t = position - fromIntegral leftIndex
    leftFrequency = frequencies !! leftIndex
    rightFrequency = frequencies !! rightIndex

data BrailleDot
  = DotEmpty
  | DotFillSoft
  | DotFillMedium
  | DotFillStrong
  | DotZero
  | DotCurve
  deriving (Eq, Ord)

renderBraillePlotCell :: EQPalette -> Int -> Int -> [Int] -> Int -> I.Image
renderBraillePlotCell palette cellX cellY curveYs zeroDbY =
  case mask of
    0 -> I.string (eqDefaultAttr palette) " "
    _ -> I.string (dotAttr palette dominantDot) [toEnum (0x2800 + mask)]
 where
  rawDots =
    [ subPixelDot subX subY (curveYs !! subX) zeroDbY
    | localX <- [0 .. 1]
    , localY <- [0 .. 3]
    , let subX = cellX * 2 + localX
    , let subY = cellY * 4 + localY
    ]
  normalizedDots = normalizeBrailleDots rawDots
  mask =
    foldl'
      (.|.)
      0
      [ brailleBit localX localY
      | localX <- [0 .. 1]
      , localY <- [0 .. 3]
      , let dot = normalizedDots !! (localX * 4 + localY)
      , dot /= DotEmpty
      ]
  dominantDot = maximum normalizedDots

subPixelDot :: Int -> Int -> Int -> Int -> BrailleDot
subPixelDot subX subY curveY zeroDbY
  | subY == curveY = DotCurve
  | subY == zeroDbY = DotZero
  | subY > min curveY zeroDbY && subY < max curveY zeroDbY =
      fillDot subX subY (abs (curveY - zeroDbY)) (abs (subY - curveY))
  | otherwise = DotEmpty

fillDot :: Int -> Int -> Int -> Int -> BrailleDot
fillDot subX subY spanSize distance
  | distance * 3 <= visibleSpan = DotFillStrong
  | distance * 3 <= visibleSpan * 2 =
      if even (subX + subY)
        then DotFillMedium
        else DotEmpty
  | (subX + subY) `mod` 3 == 0 = DotFillSoft
  | otherwise = DotEmpty
 where
  visibleSpan = max 1 spanSize

normalizeBrailleDots :: [BrailleDot] -> [BrailleDot]
normalizeBrailleDots dots
  | DotCurve `elem` dots = map keepCurveCell dots
  | DotZero `elem` dots = map keepZeroCell dots
  | otherwise = dots
 where
  keepCurveCell DotFillSoft = DotEmpty
  keepCurveCell DotFillMedium = DotEmpty
  keepCurveCell DotFillStrong = DotEmpty
  keepCurveCell dot = dot

  keepZeroCell DotFillSoft = DotEmpty
  keepZeroCell DotFillMedium = DotEmpty
  keepZeroCell DotFillStrong = DotEmpty
  keepZeroCell dot = dot

brailleBit :: Int -> Int -> Int
brailleBit 0 0 = 0x01
brailleBit 0 1 = 0x02
brailleBit 0 2 = 0x04
brailleBit 0 3 = 0x40
brailleBit 1 0 = 0x08
brailleBit 1 1 = 0x10
brailleBit 1 2 = 0x20
brailleBit 1 3 = 0x80
brailleBit _ _ = 0

dotAttr :: EQPalette -> BrailleDot -> A.Attr
dotAttr palette DotCurve = eqPrimaryBoldAttr palette
dotAttr palette DotZero = eqMutedAttr palette
dotAttr palette DotFillStrong = eqAccentAttr palette
dotAttr palette DotFillMedium = eqAccentAttr palette
dotAttr palette DotFillSoft = eqMutedAttr palette
dotAttr palette DotEmpty = eqDefaultAttr palette

renderEQAxisImage :: EQPalette -> Int -> EQConfigValue -> I.Image
renderEQAxisImage palette width config =
  I.horizCat [axisCell x | x <- [0 .. w - 1]]
 where
  w = max 1 width
  markers = axisMarkers w config
  tickXs = map fst markers
  labelOverlays = placeAxisLabels w markers

  axisCell x =
    case overlayCharAt x labelOverlays of
      Just c -> I.string (eqAccentBoldAttr palette) [c]
      Nothing
        | x `elem` tickXs -> I.string (eqAccentAttr palette) "┴"
        | otherwise -> I.string (eqMutedAttr palette) "─"
  overlayCharAt x overlays =
    go (reverse overlays)
   where
    go [] = Nothing
    go (AxisOverlay start text : rest)
      | x >= start && x < start + length text =
          Just (text !! (x - start))
      | otherwise = go rest

data AxisOverlay = AxisOverlay Int String

axisMarkers :: Int -> EQConfigValue -> [(Int, String)]
axisMarkers width config =
  zipWith toMarker [0 :: Int ..] (config ^. eqBands)
 where
  bandCount = length (config ^. eqBands)
  toMarker index band =
    (bandIndexToX width bandCount index, formatFrequencyLabel (band ^. eqBandFrequencyHz))

bandIndexToX :: Int -> Int -> Int -> Int
bandIndexToX width bandCount index
  | width <= 1 = 0
  | bandCount <= 1 = 0
  | otherwise =
      clampValue 0 (width - 1) . round @Double $
        fromIntegral index
          * fromIntegral (width - 1)
          / fromIntegral (bandCount - 1)

placeAxisLabels :: Int -> [(Int, String)] -> [AxisOverlay]
placeAxisLabels width markers =
  snd $ mapAccumL placeOverlay (-1) markers
 where
  tickXs = map fst markers

  placeOverlay previousEnd (tickX, labelText) =
    let start = bestAxisLabelStart width tickXs previousEnd tickX labelText
        end = start + length labelText - 1
     in (max previousEnd end, AxisOverlay start labelText)

bestAxisLabelStart :: Int -> [Int] -> Int -> Int -> String -> Int
bestAxisLabelStart width tickXs previousEnd tickX labelText =
  minimumBy (comparing placementScore) [0 .. maxStart]
 where
  labelWidth = length labelText
  maxStart = max 0 (width - labelWidth)
  idealStart = clampValue 0 maxStart (tickX - labelWidth `div` 2)

  placementScore start =
    ( start <= previousEnd
    , coveredTickCount start
    , abs (labelCenter start - tickX)
    , abs (start - idealStart)
    )

  coveredTickCount start =
    length
      [ x
      | x <- tickXs
      , x >= start
      , x < start + labelWidth
      ]

  labelCenter start =
    start + (labelWidth - 1) `div` 2

data BiquadCoefficients = BiquadCoefficients
  { bqB0 :: Double
  , bqB1 :: Double
  , bqB2 :: Double
  , bqA0 :: Double
  , bqA1 :: Double
  , bqA2 :: Double
  }

bandBiquadCoefficients :: EQBand -> BiquadCoefficients
bandBiquadCoefficients band =
  case band ^. eqBandFilterType of
    EQPeak ->
      BiquadCoefficients
        { bqB0 = 1 + alpha * a
        , bqB1 = (-2) * cosW0
        , bqB2 = 1 - alpha * a
        , bqA0 = 1 + alpha / a
        , bqA1 = (-2) * cosW0
        , bqA2 = 1 - alpha / a
        }
    EQLowShelf ->
      BiquadCoefficients
        { bqB0 = a * ((a + 1) - (a - 1) * cosW0 + 2 * sqrtA * alpha)
        , bqB1 = 2 * a * ((a - 1) - (a + 1) * cosW0)
        , bqB2 = a * ((a + 1) - (a - 1) * cosW0 - 2 * sqrtA * alpha)
        , bqA0 = (a + 1) + (a - 1) * cosW0 + 2 * sqrtA * alpha
        , bqA1 = (-2) * ((a - 1) + (a + 1) * cosW0)
        , bqA2 = (a + 1) + (a - 1) * cosW0 - 2 * sqrtA * alpha
        }
    EQHighShelf ->
      BiquadCoefficients
        { bqB0 = a * ((a + 1) + (a - 1) * cosW0 + 2 * sqrtA * alpha)
        , bqB1 = (-2) * a * ((a - 1) + (a + 1) * cosW0)
        , bqB2 = a * ((a + 1) + (a - 1) * cosW0 - 2 * sqrtA * alpha)
        , bqA0 = (a + 1) - (a - 1) * cosW0 + 2 * sqrtA * alpha
        , bqA1 = 2 * ((a - 1) - (a + 1) * cosW0)
        , bqA2 = (a + 1) - (a - 1) * cosW0 - 2 * sqrtA * alpha
        }
 where
  a = 10 ** (band ^. eqBandGainDb / 40)
  sqrtA = sqrt a
  q = max 0.001 (band ^. eqBandQ)
  w0 = 2 * pi * clampValue 1 (sampleRate / 2 - 1) (band ^. eqBandFrequencyHz) / sampleRate
  cosW0 = cos w0
  alpha = sin w0 / (2 * q)

finiteOrZero :: Double -> Double
finiteOrZero value
  | isNaN value || isInfinite value = 0
  | otherwise = value

sampleRate :: Double
sampleRate = 48000
