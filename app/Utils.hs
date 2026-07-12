module Utils (
  ceilingDiv,
  clampValue,
  eqGainBarLimitDb,
  eqGainBarNudgeLimitDb,
  formatFrequencyLabel,
  formatGainDb,
  formatSecs,
  formatBytes,
  formatSampleRate,
  formatBitrate,
  gainBarThumbY,
  gainBarValue,
  snapToTenths,
  songProgressTarget,
  trimTrailingZeros,
  between,
  localToScreen,
  extentHorizontalBounds,
  extentVerticalBounds,
  resizeRatio,
  weightedSizes,
  validExtent,
  containsExtent,
  intersectsExtent,
) where

import Brick (Extent (..), Location (..))
import Numeric (showFFloat)
import Text.Printf (printf)

formatSecs :: Integer -> String
formatSecs totalSecs = show mins ++ ":" ++ ensureTwoDigits secs
 where
  (mins, secs) = totalSecs `divMod` 60
  ensureTwoDigits n = if n < 10 then "0" ++ show n else show n

formatBytes :: (Integral a) => a -> String
formatBytes value
  | bytes < 0 = '-' : formatBytes (abs bytes)
  | bytes < 1024 = show bytes <> " B"
  | otherwise = render (fromIntegral bytes / 1024 :: Double) units
 where
  bytes = toInteger value
  units = ["KiB", "MiB", "GiB", "TiB", "PiB", "EiB"]

  render amount [] = format amount "EiB"
  render amount (unit : rest)
    | amount < 1024 || null rest = format amount unit
    | otherwise = render (amount / 1024) rest

  format amount unit =
    trimTrailingZeros (showFFloat (Just precision) amount "") <> " " <> unit
   where
    precision
      | amount < 10 = 1
      | otherwise = 0

formatSampleRate :: Int -> String
formatSampleRate sampleRate = printf "%.2fkHz" (fromIntegral sampleRate / 1000 :: Double)

formatBitrate :: Int -> String
formatBitrate bitrate = printf "%.2fkbps" (fromIntegral bitrate / 1000 :: Double)

ceilingDiv :: Int -> Int -> Int
ceilingDiv numerator denominator = (numerator + denominator - 1) `div` denominator

clampValue :: (Ord a) => a -> a -> a -> a
clampValue low high = min high . max low

snapToTenths :: Double -> Double
snapToTenths value = fromIntegral (round (value * 10) :: Int) / 10

trimTrailingZeros :: String -> String
trimTrailingZeros s =
  case break (== '.') s of
    (_, "") -> s
    (whole, _ : fractional) ->
      case reverse (dropWhile (== '0') (reverse fractional)) of
        "" -> whole
        trimmed -> whole <> "." <> trimmed

formatGainDb :: Double -> String
formatGainDb gain
  | gain > 0 = "+" <> showFFloat (Just 1) gain ""
  | otherwise = showFFloat (Just 1) gain ""

formatFrequencyLabel :: Double -> String
formatFrequencyLabel frequency
  | frequency >= 1000 =
      trimTrailingZeros (showFFloat precision kiloHertz "") <> "K"
  | otherwise = show (round frequency :: Int)
 where
  kiloHertz = frequency / 1000
  precision
    | kiloHertz < 10 && not (isNearlyWhole kiloHertz) = Just 1
    | otherwise = Just 0
  isNearlyWhole value =
    abs (value - fromIntegral (round value :: Int)) < 0.05

songProgressTarget :: Int -> Int -> Double -> Double
songProgressTarget width x total =
  clampValue 0 total $
    if width <= 1
      then total
      else fromIntegral clampedX * total / fromIntegral (width - 1)
 where
  clampedX = clampValue 0 (max 0 (width - 1)) x

eqGainBarLimitDb :: Double
eqGainBarLimitDb = 12

eqGainBarNudgeLimitDb :: Double
eqGainBarNudgeLimitDb = 20

gainBarThumbY :: Int -> Double -> Int
gainBarThumbY sliderHeight gain =
  clampValue 0 (max 0 (sliderHeight - 1)) . round $
    (eqGainBarLimitDb - clampedGain)
      * fromIntegral (sliderHeight - 1)
      / (2 * eqGainBarLimitDb)
 where
  clampedGain = clampValue (-eqGainBarLimitDb) eqGainBarLimitDb gain

gainBarValue :: Int -> Int -> Double
gainBarValue sliderHeight y
  | sliderHeight <= 1 = 0
  | otherwise =
      eqGainBarLimitDb
        - fromIntegral y
          * (2 * eqGainBarLimitDb)
          / fromIntegral (sliderHeight - 1)

between :: Int -> Int -> Int -> Bool
between a b x = x > min a b && x < max a b

localToScreen :: Extent n -> Location -> Location
localToScreen extent (Location (x, y)) =
  Location (left extent + x, top extent + y)

extentHorizontalBounds :: Extent n -> (Int, Int)
extentHorizontalBounds extent = (left extent, right extent)

extentVerticalBounds :: Extent n -> (Int, Int)
extentVerticalBounds extent = (top extent, bottom extent)

-- | Convert a divider coordinate into a pair-weight ratio while retaining
-- one renderable terminal cell for each adjacent child.
resizeRatio :: Int -> (Int, Int) -> Int -> Double
resizeRatio activeCells (start, end) coordinate
  | activeCells <= 1 || spanLength <= 0 = 0.5
  | otherwise = clampValue minimumShare maximumShare requestedShare
 where
  spanLength = end - start
  requestedShare = fromIntegral (coordinate - start) / fromIntegral spanLength
  minimumShare = 1 / fromIntegral activeCells
  maximumShare = 1 - minimumShare

-- | Split a fixed number of cells among positive weights. Every cell is
-- assigned exactly once, and every child gets one cell when capacity permits.
weightedSizes :: Int -> [Double] -> [Int]
weightedSizes total weights
  | capacity >= length weights = fmap (+ 1) $ distribute (capacity - length weights) weights
  | otherwise = distribute capacity weights
 where
  capacity = max 0 total

  distribute _ [] = []
  distribute remaining [_] = [remaining]
  distribute remaining (weight : rest) =
    let exact = fromIntegral remaining * weight / sum (weight : rest)
        size = min remaining . floor $ exact + 1e-9
     in size : distribute (remaining - size) rest

validExtent :: Extent n -> Bool
validExtent extent =
  let (w, h) = extentSize extent
   in w > 0 && h > 0

containsExtent :: Extent n -> Extent n -> Bool
containsExtent outer inner =
  left outer <= left inner
    && top outer <= top inner
    && right inner <= right outer
    && bottom inner <= bottom outer

intersectsExtent :: Extent n -> Extent n -> Bool
intersectsExtent a b =
  left a < right b
    && left b < right a
    && top a < bottom b
    && top b < bottom a

left :: Extent n -> Int
left extent =
  let Location (x, _) = extentUpperLeft extent
   in x

top :: Extent n -> Int
top extent =
  let Location (_, y) = extentUpperLeft extent
   in y

right :: Extent n -> Int
right extent = left extent + fst (extentSize extent)

bottom :: Extent n -> Int
bottom extent = top extent + snd (extentSize extent)
