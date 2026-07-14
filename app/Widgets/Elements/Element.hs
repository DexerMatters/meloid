{-# LANGUAGE LambdaCase #-}

{- | This module provides elements for the application.
Elements are the top-level widgets of the application which arrange the
other widgets. They are configurable, and can be nested. They can only be
displayed in `Command` or `Normal` mode.
-}
module Widgets.Elements.Element (
  ElementName (..),
) where

import Brick hiding (Horizontal, Vertical)
import Brick qualified as W hiding (Horizontal, Vertical)
import Data.Bool (bool)
import Data.Map qualified as Map
import Lens.Micro
import Types
import Widgets.Controls
import Widgets.Elements.Common
import Widgets.Elements.Header (HeaderName (..), drawCollapsedHeader)
import Widgets.Elements.Scaffold (ElementScaffoldName (..))
import Widgets.Lists
import Widgets.Visual.EQ

-- | The normal-mode drawable identity of an element at a layout path.
data ElementName = ElementName ElementPath
  deriving (Show, Eq)

instance Drawable St ElementName where
  draw (ElementName path) st
    | st ^. stMode == EditMode = drawNamed st (ElementScaffoldName path)
    | otherwise =
        case st ^. stLayoutElement path of
          Nothing -> W.emptyWidget
          Just element -> drawElement path st element
  parent (ElementName path) = Just . ParentName . mName $ ElementNode path
  variant (ElementName path) = pathVariant path

-- | Render a layout subtree, framing leaves with their element header.
drawElement :: ElementPath -> St -> LayoutElement -> Widget (MName St)
drawElement rootPath st = go True rootPath
 where
  go framed currentPath = \case
    EHBox weights children ->
      drawBox Horizontal currentPath weights children
    EVBox weights children ->
      drawBox Vertical currentPath weights children
    ETabs children ->
      frame framed currentPath $
        maybe W.emptyWidget (uncurry (go False)) (currentTabElement st currentPath children)
    EAlbumList ->
      frame framed currentPath $ drawNamed st (AllAlbumList currentPath)
    ETrackList ->
      frame framed currentPath $ drawNamed st (TrackList currentPath)
    ECurrentQueue ->
      frame framed currentPath $ drawNamed st (QueueSongList currentPath)
    EEqualizer ->
      frame framed currentPath $ drawEqualizerPanel currentPath st
    ESongInfo ->
      frame framed currentPath $ drawNamed st (SongInfoList currentPath)
    EPlaceholder ->
      W.emptyWidget

  drawBox axis currentPath weights children =
    let slots = zip (childPaths children currentPath) children
     in layoutChildren
          axis
          weights
          (zipWith needsSpacing slots $ drop 1 slots)
          [ (isExpanded childPath, drawNamed st $ ElementName childPath)
          | (childPath, _) <- slots
          ]

  frame framed currentPath body
    | not framed = body
    | otherwise =
        W.vBox
          [ if isExpanded currentPath
              then drawNamed st (HeaderName currentPath)
              else drawCollapsedHeader currentPath st
          , bool W.emptyWidget body (isExpanded currentPath)
          ]

  isExpanded path =
    case st ^. stLayoutElement path of
      Just element
        | isCollapsible element ->
            not $ st ^. stIsTriggered (mName $ ElementNode path)
      _ ->
        True

  isCollapsible = \case
    EHBox{} -> False
    EVBox{} -> False
    EPlaceholder -> False
    _ -> True

  hasScrollBar currentPath = \case
    EAlbumList -> True
    ETrackList -> True
    ECurrentQueue -> True
    EEqualizer -> True
    ESongInfo -> True
    EPlaceholder -> False
    EHBox _ children -> any (uncurry hasScrollBar) (zip (childPaths children currentPath) children)
    EVBox _ children -> any (uncurry hasScrollBar) (zip (childPaths children currentPath) children)
    ETabs children ->
      case currentTabElement st currentPath children of
        Nothing -> False
        Just (childPath, element) -> hasScrollBar childPath element

  needsSpacing (leftPath, left) (rightPath, right) =
    not $ hasScrollBar leftPath left || hasScrollBar rightPath right

-- | Draw the equalizer's list and active editor side by side.
drawEqualizerPanel :: ElementPath -> St -> Widget (MName St)
drawEqualizerPanel _ st
  | Map.null (st ^. stConfig . csEQConfigs) = W.emptyWidget
drawEqualizerPanel path st =
  W.hBox
    [ W.hLimit 11 $ drawNamed st (EQConfigList path)
    , case st ^. stIsTriggered (mName $ EQSwitch path) of
        False -> drawNamed st (EQCurveVisualizer path)
        True -> W.padLeft (W.Pad 1) $ drawNamed st (EQGainBarsViewport path)
    ]
