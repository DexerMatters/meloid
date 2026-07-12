{- | This module contains all the widgets that represent
elements in the layout editor.
Elements are the building blocks of a layout, which can
be moved, resized, deleted, and added to by the user.
-}
module Widgets.Elements (
  ElementScaffoldName (..),
  ElementName (..),
  displayElementName,
) where

import Brick
import Brick qualified as W
import Brick.Widgets.Border qualified as Bd
import Brick.Widgets.Center qualified as C
import Data.Bool (bool)
import Data.List
import Data.Map qualified as Map
import Data.Vector qualified as Vec
import Lens.Micro
import Lens.Micro.Mtl
import Types
import Widgets.Common (strClippedWithEllipsis)
import Widgets.Controls
import Widgets.Lists
import Widgets.Visual.EQ

{- | The representation of layout elements in the edit mode.
They appear as scaffolds in the edit mode which supports
moving, resizing, deleting, and adding new elements.
-}
data ElementScaffoldName = ElementScaffoldName ElementPath
  deriving (Show, Eq)

{- | The representation of layout elements in the non-edit
mode. They appear as widgets in the non-edit mode.
-}
data ElementName = ElementName ElementPath
  deriving (Show, Eq)

{- | The headers of layout elements in the non-edit mode.
They contain some buttons and a collapsing switch.
-}
data HeaderName = HeaderName ElementPath
  deriving (Show, Eq)

data CollapsingSwitch = CollapsingSwitch ElementPath
  deriving (Show, Eq)

data CollapsingSwitch' = CollapsingSwitch' ElementPath
  deriving (Show, Eq)

data TabButton = TabButton ElementPath
  deriving (Show, Eq)

-- | The location of an element in the layout tree.
type ElementPath = [Int]

-- | Compute the variant number of an element path.
pathVariant :: ElementPath -> Int
pathVariant = foldl' (\acc i -> acc * 131 + i + 1) 0

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

instance Drawable St ElementName where
  draw (ElementName path) st
    | st ^. stMode == EditMode = drawNamed st (ElementScaffoldName path)
    | otherwise =
        case st ^. stLayoutElement path of
          Nothing -> W.emptyWidget
          Just element -> drawElement path st element
  parent (ElementName path) = case path of
    [] -> Just (ParentView MainView)
    is -> ParentName . mName . ElementName . fst <$> unsnoc is
  variant (ElementName path) = pathVariant path

-- | Draw all the elements of a layout element.
drawElement :: ElementPath -> St -> LayoutElement -> Widget (MName St)
drawElement path st = go True path
 where
  go framed currentPath = \case
    EHBox weights children ->
      drawBox W.hBox W.hLimitPercent (W.padLeft (W.Pad 1)) currentPath weights children
    EVBox weights children ->
      drawBox W.vBox W.vLimitPercent (W.padTop (W.Pad 1)) currentPath weights children
    ETabs children ->
      frame framed currentPath $
        maybe W.emptyWidget (uncurry (go False)) (currentTabElement currentPath children)
    EAlbumList ->
      frame framed currentPath $ drawNamed st AllAlbumList
    ETrackList ->
      frame framed currentPath $ drawNamed st TrackList
    ECurrentQueue ->
      frame framed currentPath $ drawNamed st QueueSongList
    EEqualizer ->
      frame framed currentPath $ drawEqualizerPanel st
    ESongInfo ->
      frame framed currentPath $ drawNamed st SongInfoList
    EPlaceholder ->
      W.emptyWidget

  drawBox box limit pad currentPath weights children =
    box $
      applyElementSpacing pad children $
        fitChildren limit currentPath weights children $
          drawNamed st . ElementName <$> childPaths children currentPath

  frame framed currentPath body
    | not framed = body
    | otherwise =
        W.vBox
          [ drawNamed st (HeaderName currentPath)
          , bool W.emptyWidget body (isCollapsed currentPath)
          ]

  isCollapsed p = not $ st ^. stIsTriggered (mName $ ElementName p)

  currentTabIndex currentPath =
    Map.findWithDefault 0 currentPath (st ^. stTabStates)

  currentTabElement currentPath children =
    let childPath = childPaths children currentPath !? currentTabIndex currentPath
     in childPath >>= \p -> (\element -> (p, element)) <$> (st ^. stLayoutElement p)

  fitChildren limit currentPath weights children widgets =
    snd $
      mapAccumL step stretchPercents $
        zip (childPaths children currentPath) widgets
   where
    stretchPercents =
      layoutPercents weights
        [ ()
        | childPath <- childPaths children currentPath
        , not (isCollapsed childPath)
        ]

    step percents (childPath, widget)
      | isCollapsed childPath = (percents, widget)
      | otherwise =
          case percents of
            percent : rest -> (rest, limit percent widget)
            [] -> ([], widget)

  -- Calculate the percentage widths of each child element.
  -- It is always the last child that takes up the remaining space.
  layoutPercents weights children =
    remainingPercents effectiveWeights
   where
    effectiveWeights =
      case weights of
        Just values
          | length values == length children
          , all (> 0) values ->
              values
        _ ->
          replicate (length children) 1

  remainingPercents = \case
    [] -> []
    [_] -> [100]
    value : rest ->
      let remaining = value + sum rest
          current = max 1 . floor $ (value / remaining) * 100
       in current : remainingPercents rest

  -- Scrollbar is considered as a one-size divider.
  -- So we don't need padding when the element has a scrollbar.
  hasScrollBar = \case
    EAlbumList -> True
    ETrackList -> True
    ECurrentQueue -> True
    EEqualizer -> True
    ESongInfo -> True
    EPlaceholder -> False
    EHBox _ children -> any hasScrollBar children
    EVBox _ children -> any hasScrollBar children
    ETabs children ->
      case currentTabElement path children of
        Nothing -> False
        Just (_, element) -> hasScrollBar element

  applyElementSpacing pad elements widgets =
    case zip elements widgets of
      [] -> []
      (firstElement, firstWidget) : rest ->
        firstWidget
          : [ applyPadding left right widget
            | ((left, _), (right, widget)) <- zip ((firstElement, firstWidget) : rest) rest
            ]
   where
    applyPadding left right
      | hasScrollBar left || hasScrollBar right = id
      | otherwise = pad

instance Drawable St ElementScaffoldName where
  draw (ElementScaffoldName path) st =
    case st ^. stLayoutElement path of
      Nothing -> W.emptyWidget
      Just element -> drawElementScaffold path element st
  parent (ElementScaffoldName path) = case path of
    [] -> Just (ParentView MainView)
    is -> ParentName . mName . ElementScaffoldName . fst <$> unsnoc is
  variant (ElementScaffoldName path) = pathVariant path
  onMouseLeftUp (ElementScaffoldName path) =
    Just $ \_ ->
      use (stLayoutElement path) >>= \case
        Nothing ->
          logReqDebug "onMouseLeftUp" ("missing layout element at " <> show path)
        Just element ->
          logReqDebug
            "onMouseLeftUp"
            (formatElementName element <> " at " <> show path)

instance Drawable St HeaderName where
  draw (HeaderName path) st =
    case st ^. stLayoutElement path of
      Nothing -> W.emptyWidget
      Just element ->
        case drawHeader path st element of
          [] -> W.emptyWidget
          widgets ->
            W.vLimit 1 $
              W.hBox widgets
  parent (HeaderName path) = Just . ParentName . mName . ElementName $ path
  variant (HeaderName path) = pathVariant path

instance Drawable St CollapsingSwitch where
  draw (CollapsingSwitch path) st =
    W.withAttr (attrName "label") . W.str . (<> currentLabel) $
      bool " - " " + " $
        st ^. stIsTriggered (mName $ ElementName path)
   where
    currentLabel =
      case st ^. stLayoutElement path of
        Nothing -> ""
        Just currentElement -> displayElementName currentElement <> " "
  parent (CollapsingSwitch path) =
    Just . ParentName . mName . ElementName $ path
  variant (CollapsingSwitch path) = pathVariant path
  onMouseLeftUp (CollapsingSwitch path) = Just $ \_ ->
    use (stIsTriggered (mName $ ElementName path)) >>= \case
      True -> unTrigger (mName $ ElementName path)
      False -> trigger (mName $ ElementName path)

instance Drawable St CollapsingSwitch' where
  draw (CollapsingSwitch' path) st =
    W.withAttr (attrName "label") . W.str $
      bool " - " " + " $
        st ^. stIsTriggered (mName $ ElementName path)
  parent (CollapsingSwitch' path) =
    Just . ParentName . mName . ElementName $ path
  variant (CollapsingSwitch' path) = pathVariant path
  onMouseLeftUp (CollapsingSwitch' path) = Just $ \_ ->
    use (stIsTriggered (mName $ ElementName path)) >>= \case
      True -> unTrigger (mName $ ElementName path)
      False -> trigger (mName $ ElementName path)

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
        Just (parentPath, childIndex) ->
          Map.findWithDefault 0 parentPath (st ^. stTabStates) == childIndex

  parent (TabButton path) =
    case unsnoc path of
      Nothing -> Nothing
      Just (parentPath, _) -> Just . ParentName . mName . ElementName $ parentPath
  variant (TabButton path) = pathVariant path
  onMouseLeftUp (TabButton path) = Just $ \_ ->
    case unsnoc path of
      Nothing -> pure ()
      Just (parentPath, childIndex) ->
        stTabStates %= Map.insert parentPath childIndex

{- | Draw the header of a layout element.
Tabs use the same header as the selected child.
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
    , W.padLeft W.Max $ drawNamed st EQSwitch
    ]
  ESongInfo ->
    [ drawNamed st $ CollapsingSwitch path
    , W.fill ' '
    ]
  ETabs children ->
    [ drawNamed st $ CollapsingSwitch' path
    , drawTabNames children
    , drawCurrentTabHeader children
    ]
  _ -> []
 where
  currentTabIndex =
    Map.findWithDefault 0 path (st ^. stTabStates)

  drawTabNames children =
    case childPaths children path of
      [] -> W.emptyWidget
      childPaths' ->
        W.hBox $
          drawNamed st . TabButton <$> childPaths'

  drawCurrentTabHeader children =
    case currentTabElement children of
      Nothing -> W.emptyWidget
      Just (childPath, child) ->
        W.hBox $ drop 1 $ drawHeader childPath st child

  currentTabElement children =
    let childPath = childPaths children path !? currentTabIndex
     in childPath >>= \p -> (\child -> (p, child)) <$> (st ^. stLayoutElement p)

  selectedAlbum = (st ^. stSelectedAlbum) >>= ((st ^. stConfig . csAllAlbums) Vec.!?)
  album = maybe "" albumName selectedAlbum

-- | Draw all the scaffolding of a layout element.
drawElementScaffold :: ElementPath -> LayoutElement -> St -> Widget (MName St)
drawElementScaffold path element st =
  case element of
    EHBox weights children ->
      drawElementContainer (formatElementName element) $
        W.hBox $
          applyPlaceholderSpacing (W.padLeft (W.Pad 1)) $
            zipWith W.hLimitPercent (layoutPercents weights children) $
              drawChildren children
    EVBox weights children ->
      drawElementContainer (formatElementName element) $
        W.vBox $
          applyPlaceholderSpacing (W.padTop (W.Pad 1)) $
            zipWith W.vLimitPercent (layoutPercents weights children) $
              drawChildren children
    ETabs children ->
      drawElementContainer (formatElementName element) $
        W.vBox $
          applyPlaceholderSpacing (W.padTop (W.Pad 1)) $
            drawChildren children
    leaf ->
      drawElementPlaceholder (formatElementName leaf)
 where
  drawChildren :: [LayoutElement] -> [Widget (MName St)]
  drawChildren children =
    drawNamed st . ElementScaffoldName <$> childPaths children path

  layoutPercents :: Maybe [Double] -> [a] -> [Int]
  layoutPercents weights children =
    remainingPercents effectiveWeights
   where
    effectiveWeights =
      case weights of
        Just values
          | length values == length children
          , all (> 0) values ->
              values
        _ ->
          replicate (length children) 1

  drawElementPlaceholder label =
    Bd.border $ C.center $ W.str label

  drawElementContainer label =
    Bd.borderWithLabel (W.str (" " <> label <> " "))

  remainingPercents :: [Double] -> [Int]
  remainingPercents = \case
    [] -> []
    [_] -> [100]
    value : rest ->
      let remaining = value + sum rest
          current = max 1 . floor $ (value / remaining) * 100
       in current : remainingPercents rest

  applyPlaceholderSpacing _ [] = []
  applyPlaceholderSpacing pad (widget : widgets) =
    widget : fmap pad widgets

childPaths :: [LayoutElement] -> ElementPath -> [ElementPath]
childPaths children parent' =
  fmap (\i -> parent' <> [i]) [0 .. length children - 1]

drawEqualizerPanel :: St -> Widget (MName St)
drawEqualizerPanel st
  | Map.null (st ^. stConfig . csEQConfigs) = W.emptyWidget
drawEqualizerPanel st =
  W.hBox
    [ W.hLimit 11 $ drawNamed st EQConfigList
    , case st ^. stIsTriggered (mName EQSwitch) of
        False -> drawNamed st EQCurveVisualizer
        True -> W.padLeft (W.Pad 1) $ drawNamed st EQGainBarsViewport
    ]
