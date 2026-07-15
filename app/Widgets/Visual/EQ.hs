-- | Braille preview of the fixed-band equalizer gains.
module Widgets.Visual.EQ (
  EQCurveVisualizer (..),
) where

import Brick qualified as B
import GHC.Bits ((.|.))
import Graphics.Vty.Attributes qualified as A
import Graphics.Vty.Image qualified as I
import Lens.Micro ((^.))
import Types
import Utils (clampValue, eqGainBarNudgeLimitDb, formatFrequencyLabel)
import Widgets.Elements.Common (ElementNode (..), ElementPath, pathVariant)
import Widgets.Lists (stCurrentEQ)

data EQCurveVisualizer = EQCurveVisualizer ElementPath

data EQPalette = EQPalette A.Attr A.Attr A.Attr A.Attr

instance Drawable St EQCurveVisualizer where
  draw _ st = B.Widget B.Greedy B.Greedy $ do
    context <- B.getContext
    palette <- EQPalette <$> B.lookupAttrName (B.attrName "eqDefault") <*> B.lookupAttrName (B.attrName "eqMuted") <*> B.lookupAttrName (B.attrName "eqAccent") <*> B.lookupAttrName (B.attrName "eqPrimaryBold")
    B.render . B.raw $ renderEQ palette (context ^. B.availWidthL) (context ^. B.availHeightL) (st ^. stCurrentEQ)
  parent (EQCurveVisualizer path) = Just . ParentName . mName $ ElementNode path
  variant (EQCurveVisualizer path) = pathVariant path

renderEQ :: EQPalette -> Int -> Int -> EQConfigSpecs -> I.Image
renderEQ palette width height (EQConfigSpecs gains) =
  I.vertCat $ plot : [axis | height > 1]
 where
  w = max 1 width
  h = max 1 (height - 1)
  plot =
    I.vertCat
      [ I.horizCat [cell x y | x <- [0 .. w - 1]]
      | y <- [0 .. h - 1]
      ]
  curve = [toY (interpolate gains x (w * 2 - 1)) | x <- [0 .. w * 2 - 1]]
  range = max eqGainBarNudgeLimitDb (maximum (0 : map abs gains) + 2)
  toY gain = clampValue 0 (h * 4 - 1) . round $ (range - gain) * fromIntegral (h * 4 - 1) / (2 * range)
  zero = toY 0
  cell x y =
    let dots = [dot (x * 2 + dx) (y * 4 + dy) | dx <- [0 .. 1], dy <- [0 .. 3]]
        mask = foldl' (.|.) 0 [brailleBit dx dy | dx <- [0 .. 1], dy <- [0 .. 3], dots !! (dx * 4 + dy)]
     in I.string (dotAttr palette dots) [toEnum (0x2800 + mask)]
  dot x y
    | y == curve !! x = True
    | y == zero = True
    | otherwise = y > min zero (curve !! x) && y < max zero (curve !! x)
  axis = I.string (axisAttr palette) $ take w $ concatMap ((<> " ") . formatFrequencyLabel) [eqFrequencies !! i | i <- [0, 3 .. length eqFrequencies - 1]] <> repeat ' '

interpolate :: [Double] -> Int -> Int -> Double
interpolate [] _ _ = 0
interpolate [gain] _ _ = gain
interpolate gains x width =
  left + (right - left) * fraction
 where
  position = fromIntegral x * fromIntegral (length gains - 1) / fromIntegral (max 1 width)
  index = min (length gains - 2) $ floor position
  fraction = position - fromIntegral index
  left = gains !! index
  right = gains !! (index + 1)

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

dotAttr :: EQPalette -> [Bool] -> A.Attr
dotAttr (EQPalette def _ _ curve) dots
  | not (or dots) = def
  | otherwise = curve

axisAttr :: EQPalette -> A.Attr
axisAttr (EQPalette _ _ accent _) = accent
