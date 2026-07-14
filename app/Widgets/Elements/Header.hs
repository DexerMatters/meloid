{-# LANGUAGE LambdaCase #-}

{- | This module provides headers for almost all elements.
The headers provide a title and a collapse button, if applicable. And
they also display tabs and buttons that are specific to the element.
-}
module Widgets.Elements.Header (
  HeaderName (..),
  drawCollapsedHeader,
  drawHeader,
) where

import Brick
import Brick qualified as W
import Data.Bool (bool)
import Data.List (unsnoc)
import Data.Map qualified as Map
import Data.Vector qualified as Vec
import Lens.Micro
import Lens.Micro.Mtl
import Types
import Widgets.Common (strClippedWithEllipsis)
import Widgets.Controls
import Widgets.Elements.Common

-- | The header paired with an element's normal-mode frame.
data HeaderName = HeaderName ElementPath
  deriving (Show, Eq)

-- | A collapse control for ordinary, named leaf elements.
data CollapsingSwitch = CollapsingSwitch ElementPath
  deriving (Show, Eq)

-- | A compact collapse control used by tab containers.
data CollapsingSwitch' = CollapsingSwitch' ElementPath
  deriving (Show, Eq)

-- | A tab selector identified by its child element path.
data TabButton = TabButton ElementPath
  deriving (Show, Eq)

instance Drawable St HeaderName where
  draw (HeaderName path) st =
    case st ^. stLayoutElement path of
      Nothing -> W.emptyWidget
      Just element ->
        case drawHeader path st element of
          [] -> W.emptyWidget
          widgets -> W.vLimit 1 $ W.hBox widgets
  parent (HeaderName path) = Just . ParentName . mName $ ElementNode path
  variant (HeaderName path) = pathVariant path

instance Drawable St CollapsingSwitch where
  draw (CollapsingSwitch path) st =
    W.withAttr (attrName "label") . W.str . (<> currentLabel) $
      bool " - " " + " $
        st ^. stIsTriggered (mName $ ElementNode path)
   where
    currentLabel =
      case st ^. stLayoutElement path of
        Nothing -> ""
        Just currentElement -> displayElementName currentElement <> " "

  parent (CollapsingSwitch path) =
    Just . ParentName . mName $ ElementNode path
  variant (CollapsingSwitch path) = pathVariant path
  onMouseLeftUp (CollapsingSwitch path) = Just $ \_ -> toggleCollapsed path

instance Drawable St CollapsingSwitch' where
  draw (CollapsingSwitch' path) st =
    W.withAttr (attrName "label") . W.str $
      bool " - " " + " $
        st ^. stIsTriggered (mName $ ElementNode path)
  parent (CollapsingSwitch' path) =
    Just . ParentName . mName $ ElementNode path
  variant (CollapsingSwitch' path) = pathVariant path
  onMouseLeftUp (CollapsingSwitch' path) = Just $ \_ -> toggleCollapsed path

instance Drawable St TabButton where
  draw (TabButton path) st =
    withStyle $ W.str (" " <> label <> " ")
   where
    label =
      case st ^. stLayoutElement path of
        Nothing -> "unknown"
        Just child ->
          case displayElementName child of
            "" -> formatElementName child
            name -> name

    withStyle =
      if isCurrentTab
        then W.withAttr (attrName "label")
        else W.withAttr (attrName "textOnTabs")

    isCurrentTab =
      case unsnoc path of
        Nothing -> False
        Just (parentPath', childIndex) ->
          Map.findWithDefault 0 parentPath' (st ^. stTabStates) == childIndex

  parent (TabButton path) =
    case unsnoc path of
      Nothing -> Nothing
      Just (parentPath', _) -> Just . ParentName . mName $ ElementNode parentPath'
  variant (TabButton path) = pathVariant path
  onMouseLeftUp (TabButton path) = Just $ \_ ->
    case unsnoc path of
      Nothing -> pure ()
      Just (parentPath', childIndex) ->
        stTabStates %= Map.insert parentPath' childIndex

-- | Draw the one-line header that remains when an element is collapsed.
drawCollapsedHeader :: ElementPath -> St -> Widget (MName St)
drawCollapsedHeader path st =
  W.vLimit 1 $
    case st ^. stLayoutElement path of
      Just ETabs{} -> drawNamed st (CollapsingSwitch' path)
      _ -> drawNamed st (CollapsingSwitch path)

{- | Build the header pieces for an expanded element.

Tab containers append the current tab body's header pieces after their tab
selectors, avoiding a second frame around the selected child.
-}
drawHeader :: ElementPath -> St -> LayoutElement -> [Widget (MName St)]
drawHeader path st = \case
  EAlbumList ->
    [ drawNamed st $ CollapsingSwitch path
    , W.fill ' '
    ]
  ETrackList ->
    [ drawNamed st $ CollapsingSwitch path
    , W.fill ' '
    , W.padLeft W.Max $
        W.withAttr (attrName "meta") $
          strClippedWithEllipsis album
    ]
  ECurrentQueue ->
    [ drawNamed st $ CollapsingSwitch path
    , W.fill ' '
    , W.padLeft W.Max $ drawNamed st ShuffleButton
    , W.padLeft (W.Pad 1) $ drawNamed st ReverseOrderButton
    , W.padLeft (W.Pad 1) . W.padRight (W.Pad 1) $ drawNamed st ClearButton
    ]
  EEqualizer ->
    [ drawNamed st $ CollapsingSwitch path
    , W.fill ' '
    , W.padLeft W.Max $ drawNamed st (EQSwitch path)
    ]
  ESongInfo ->
    [ drawNamed st $ CollapsingSwitch path
    , W.fill ' '
    ]
  ETabs children ->
    [ drawNamed st $ CollapsingSwitch' path
    , case childPaths children path of
        [] -> W.emptyWidget
        childPaths' ->
          W.hBox $
            drawNamed st . TabButton <$> childPaths'
    , case currentTabElement st path children of
        Nothing -> W.emptyWidget
        Just (childPath, child) ->
          W.hBox $ drop 1 $ drawHeader childPath st child
    ]
  _ -> []
 where
  selectedAlbum = (st ^. stSelectedAlbum) >>= ((st ^. stConfig . csAllAlbums) Vec.!?)
  album = maybe "" albumName selectedAlbum

toggleCollapsed :: ElementPath -> EventM (MName St) St ()
toggleCollapsed path =
  use (stIsTriggered (mName $ ElementNode path)) >>= \case
    True -> unTrigger (mName $ ElementNode path)
    False -> trigger (mName $ ElementNode path)
