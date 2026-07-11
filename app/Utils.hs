module Utils (
  ceilingDiv,
  clampValue,
  eqGainBarLimitDb,
  eqGainBarNudgeLimitDb,
  formatFrequencyLabel,
  formatGainDb,
  formatSecs,
  gainBarThumbY,
  gainBarValue,
  snapToTenths,
  songProgressTarget,
  trimTrailingZeros,
  between,
) where

import Numeric (showFFloat)

formatSecs :: Integer -> String
formatSecs totalSecs = show mins ++ ":" ++ ensureTwoDigits secs
 where
  (mins, secs) = totalSecs `divMod` 60
  ensureTwoDigits n = if n < 10 then "0" ++ show n else show n

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
