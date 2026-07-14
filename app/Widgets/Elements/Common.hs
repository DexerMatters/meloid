{-# LANGUAGE LambdaCase #-}

{- | This module provides common functions and types for elements and
scaffolds.
-}
module Widgets.Elements.Common (
  ElementPath,
  ElementNode (..),
  LayoutAxis (..),
  scaffoldBorderStyle,
  pathVariant,
  displayElementName,
  childPaths,
  parentPath,
  currentTabElement,
  layoutChildren,
  layoutWeights,
) where

import Brick hiding (Horizontal, Vertical)
import Brick qualified as W hiding (Horizontal, Vertical)
import Brick.Widgets.Border.Style qualified as Bd
import Data.List (mapAccumL, unsnoc, (!?))
import Data.Map qualified as Map
import Graphics.Vty qualified as V
import Lens.Micro
import Types
import Utils (weightedSizes)

-- | A zero-based path from the configured layout root to an element.
type ElementPath = [Int]

{- | A stable drawable identity for a layout node without drawing its body.

Headers and controls use this as their parent so their names remain stable
when normal rendering is replaced by edit-mode scaffolds.
-}
data ElementNode = ElementNode ElementPath
  deriving (Show, Eq)

-- | The primary axis used to distribute a container's children.
data LayoutAxis = Horizontal | Vertical

instance Drawable St ElementNode where
  draw _ _ = W.emptyWidget
  parent (ElementNode path) = case path of
    [] -> Nothing
    is -> ParentName . mName . ElementNode . fst <$> unsnoc is
  variant (ElementNode path) = pathVariant path

scaffoldBorderStyle :: Bd.BorderStyle
scaffoldBorderStyle =
  Bd.BorderStyle
    { bsCornerTL = '·'
    , bsCornerTR = '·'
    , bsCornerBR = '·'
    , bsCornerBL = '·'
    , bsIntersectFull = '+'
    , bsIntersectL = '+'
    , bsIntersectR = '+'
    , bsIntersectT = '+'
    , bsIntersectB = '+'
    , bsHorizontal = '-'
    , bsVertical = '¦'
    }

-- | Derive a reproducible name variant from an element path.
pathVariant :: ElementPath -> Int
pathVariant = foldl' (\acc i -> acc * 131 + i + 1) 0

-- | The user-facing name for leaf elements; containers have no title here.
displayElementName :: LayoutElement -> String
displayElementName (EHBox _ _) = ""
displayElementName (EVBox _ _) = ""
displayElementName (ETabs _) = ""
displayElementName EAlbumList = "ALBUMS"
displayElementName ETrackList = "TRACKS"
displayElementName ECurrentQueue = "QUEUE"
displayElementName EEqualizer = "EQUALIZER"
displayElementName ESongInfo = "INFO"
displayElementName EPlaceholder = "EMPTY"

-- | Construct the immediate child paths of a container.
childPaths :: [LayoutElement] -> ElementPath -> [ElementPath]
childPaths children path =
  fmap (\i -> path <> [i]) [0 .. length children - 1]

parentPath :: ElementPath -> Maybe ElementPath
parentPath = fmap fst . unsnoc

-- | Look up the selected tab and its path, if the selected index is valid.
currentTabElement :: St -> ElementPath -> [LayoutElement] -> Maybe (ElementPath, LayoutElement)
currentTabElement st path children =
  let currentTabIndex = Map.findWithDefault 0 path (st ^. stTabStates)
      childPath = childPaths children path !? currentTabIndex
   in childPath >>= \child -> (\element -> (child, element)) <$> (st ^. stLayoutElement child)

{- | Lay out children along one axis while giving collapsed children only their
rendered size. The remaining cells are distributed by the configured weights.
-}
layoutChildren :: LayoutAxis -> Maybe [Double] -> [Bool] -> [(Bool, Widget n)] -> Widget n
layoutChildren axis configuredWeights gaps children =
  W.Widget W.Greedy W.Greedy $ do
    context <- W.getContext
    collapsedSizes <-
      traverse (fmap (primarySize . image) . W.render . snd) $
        filter (not . fst) children
    let weights = layoutWeights configuredWeights (length children)
        activeWeights =
          [ weight
          | (weight, (expanded, _)) <- zip weights children
          , expanded
          ]
        available =
          max 0 $
            axisAvailable context - sum collapsedSizes - length (filter id gaps)
        activeSizes = weightedSizes available activeWeights
        slotted = snd $ mapAccumL constrain activeSizes children
    W.render . axisBox $ interleave gaps slotted
 where
  axisAvailable context =
    case axis of
      Horizontal -> context ^. W.availWidthL
      Vertical -> context ^. W.availHeightL

  primarySize =
    case axis of
      Horizontal -> V.imageWidth
      Vertical -> V.imageHeight

  constrain sizes (expanded, widget)
    | not expanded = (sizes, widget)
    | otherwise =
        case sizes of
          size : rest -> (rest, fitSlot size widget)
          [] -> ([], fitSlot 0 widget)

  fitSlot size widget =
    case axis of
      Horizontal -> W.hLimit size $ W.padRight W.Max widget
      Vertical -> W.vLimit size $ W.padBottom W.Max widget

  axisBox =
    case axis of
      Horizontal -> W.hBox
      Vertical -> W.vBox

  interleave _ [] = []
  interleave spaceAfter (first : rest) =
    first
      : concat
        [ [axisSpacer | addSpace] <> [widget]
        | (addSpace, widget) <- zip spaceAfter rest
        ]

  axisSpacer =
    case axis of
      Horizontal -> W.hLimit 1 $ W.fill ' '
      Vertical -> W.vLimit 1 $ W.fill ' '

layoutWeights :: Maybe [Double] -> Int -> [Double]
layoutWeights configuredWeights childCount =
  case configuredWeights of
    Just weights
      | length weights == childCount
      , all (> 0) weights ->
          weights
    _ ->
      replicate childCount 1
