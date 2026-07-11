{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | This module provides views for the application.
Views are the top-level widgets of the application which
arrange the other widgets.

Since views use other widgets, while the other widgets may
also specify their parents with views. To avoid circular
dependencies, the views also have a `ViewName` to identify
themselves in the context of child widgets.
-}
module Widgets.Views (
  DebugViewport (..),
  lookupRenderedImage,
  drawView,
  drawDialogView,
) where

import Brick
import Brick.Widgets.Border qualified as Bd
import Brick.Widgets.Center qualified as C
import Brick.Widgets.Core qualified as W
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Vector qualified as Vec
import Lens.Micro ((^.), (^?))
import Network.MPD qualified as MPD
import Types
import Widgets.Common
import Widgets.Controls
import Widgets.Edits (CommandEditor (CommandEditor))
import Widgets.Lists
import Widgets.Visual.Art
import Widgets.Visual.EQ

data DebugViewport = DebugViewport

instance Drawable St LayoutElement where
  draw element st
    | st ^. stMode == EditMode = drawNamed st (ElementScaffoldName element)
    | otherwise = go element
   where
    go = \case
      EHBox weights children ->
        W.hBox $
          applyElementSpacing (W.padLeft (W.Pad 1)) children $
            zipWith W.hLimitPercent (layoutPercents weights children) $
              fmap go children
      EVBox weights children ->
        W.vBox $
          applyElementSpacing (W.padTop (W.Pad 1)) children $
            zipWith W.vLimitPercent (layoutPercents weights children) $
              fmap go children
      EAlbumList ->
        drawAllAlbumList st
      EAlbumSongList ->
        drawAlbumSongList st
      EPlayingQueue ->
        drawCurrentQueueList st

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
      EAlbumSongList -> True
      EPlayingQueue -> True
      EHBox _ children -> any hasScrollBar children
      EVBox _ children -> any hasScrollBar children

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
  parent _ = Just (ParentView MainView)

instance Drawable St ElementScaffoldName where
  draw (ElementScaffoldName element) st =
    case element of
      EHBox weights children ->
        drawElementContainer (formatElementName element) $
          W.hBox $
            applyPlaceholderSpacing (W.padLeft (W.Pad 1)) $
              zipWith W.hLimitPercent (layoutPercents weights children) $
                fmap (drawNamed st . ElementScaffoldName) children
      EVBox weights children ->
        drawElementContainer (formatElementName element) $
          W.vBox $
            applyPlaceholderSpacing (W.padTop (W.Pad 1)) $
              zipWith W.vLimitPercent (layoutPercents weights children) $
                fmap (drawNamed st . ElementScaffoldName) children
      leaf ->
        drawElementPlaceholder (formatElementName leaf)
   where
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
  parent _ = Just (ParentView MainView)
  onMouseLeftUp n _ = logReqDebug "onMouseLeftUp" (show n)

{- | This function combines the lookup of the playing
album image and ones in the album list to search for
an arbitrary rendered image
-}
lookupRenderedImage :: St -> MName St -> ImageSize -> Maybe RenderedImage
lookupRenderedImage st name size
  | Just AlbumArtPlaying <- castMName name =
      st ^. stCurrentAlbumArt >>= (Map.!? size)
  | Just (AlbumArtThumb i) <- castMName name =
      lookupAlbumThumbRenderedImage st i size
  | otherwise = Nothing

drawView :: ViewName -> St -> Widget (MName St)
drawView MainView st =
  W.vBox
    [ W.hBox
        [ drawControlPanel st
        , W.padLeft (W.Pad 2) . W.padRight (W.Pad 1) $ drawSongPanel st
        , drawNamed st AlbumArtPlaying
        ]
    , W.padTop (W.Pad 1) $
        drawNamed st (st ^. stConfig . csConfigs . cvLayout)
    , drawBottomBar st
    ]
drawView DebugView st = drawNamed st DebugViewport
drawView _ _ = W.emptyWidget

drawDialogView :: ViewName -> St -> Widget (MName St)
drawDialogView WelcomeDialog st =
  drawWelcomeDialog st
drawDialogView SimpleDialog st =
  drawSimpleDialog st
drawDialogView _ _ = W.emptyWidget

drawWelcomeDialog :: St -> Widget (MName St)
drawWelcomeDialog st =
  W.withAttr (attrName "dialog") $
    W.padAll 2 . W.vBox $
      [ C.hCenter $ W.str "Welcome to Gaze Player"
      , C.hCenter pageWidget
      , W.padTop (W.Pad 2) $
          W.hBox
            [ skipButton
            , W.padLeft W.Max $
                W.hBox
                  [ prevButton
                  , W.padLeft (W.Pad 1) nextOrFinish
                  ]
            ]
      ]
 where
  page = fromMaybe 1 (st ^? stDialog .? dsPage)

  prevButton
    | page > 1 = drawNamed st PrevButton
    | otherwise = W.emptyWidget

  skipButton
    | page < 3 = drawNamed st SkipButton
    | otherwise = W.emptyWidget

  nextOrFinish
    | page < 3 = drawNamed st NextButton
    | otherwise = drawNamed st FinishButton

  pageWidget
    | page == 1 =
        W.str $
          unlines
            [ "This is a simple video player made in Haskell."
            , "It aims to be fast, simple, and easy to use."
            ]
    | page == 2 =
        W.str $
          unlines
            [ "This is the next page."
            ]
    | otherwise = W.str "\nUnknown page"

drawSimpleDialog :: St -> Widget (MName St)
drawSimpleDialog st =
  W.withAttr (attrName "dialog") $
    W.padAll 2 . W.vBox $
      [ C.hCenter $ W.str "Welcome to Gaze Player"
      , C.hCenter $ W.strWrap (st ^. stDialog .? dsText)
      , W.padTop (W.Pad 2) . W.padLeft W.Max $ drawNamed st OkButton
      ]

instance Drawable St DebugViewport where
  draw _ st =
    W.viewport (mName DebugViewport) Vertical $
      W.vBox $
        W.str "Debug view\n\n"
          : reverse
            [ W.withAttr (attrName attrStyle) $ W.strWrap msg
            | (logLevel, msg) <- st ^. stLogs
            , let attrStyle = case logLevel of
                    Debug -> "debugLog"
                    Info -> "infoLog"
                    Warn -> "warnLog"
                    Error -> "errorLog"
            ]
  parent _ = Just (ParentView DebugView)
  onMouseScrollUp _ = Just $ scrollViewportBy (mName DebugViewport) (-1)
  onMouseScrollDown _ = Just $ scrollViewportBy (mName DebugViewport) 1

drawAllAlbumList :: St -> Widget (MName St)
drawAllAlbumList st =
  W.vBox
    [ W.withAttr (attrName "label") $ W.str " ALBUMS "
    , drawNamed st AllAlbumList
    ]

drawAlbumSongList :: St -> Widget (MName St)
drawAlbumSongList st =
  case selected of
    Nothing -> W.emptyWidget
    Just _ ->
      W.vBox
        [ W.hBox
            [ withAttr (attrName "label") (W.str " TRACKS ")
            , W.padLeft W.Max $ withAttr (attrName "meta") $ W.str $ album
            ]
        , drawNamed st AlbumSongList
        ]
 where
  selected = (st ^. stSelectedAlbum) >>= ((st ^. stConfig . csAllAlbums) Vec.!?)
  album = maybe "" albumName selected

drawSongPanel :: St -> Widget (MName St)
drawSongPanel st =
  W.vBox
    [ W.hBox
        [ W.padRight W.Max $ withAttr (attrName "header") $ strClippedWithEllipsis title
        , W.padLeft W.Max $ withAttr (attrName "meta") $ strClippedWithEllipsis ("by " <> artist)
        ]
    , strClippedWithEllipsis album
    , drawNamed st SongProgressBar
    ]
 where
  title = NonEmpty.head $ st ^. stCurrentSongMeta MPD.Title
  artist = concat $ NonEmpty.intersperse ", " (st ^. stCurrentSongMeta MPD.Artist)
  album = concat $ NonEmpty.intersperse " - " (st ^. stCurrentSongMeta MPD.Album)

drawControlPanel :: St -> Widget (MName St)
drawControlPanel st =
  W.hLimit 21 $
    W.vBox
      [ W.vBox
          [ W.str $ "TIME " <> formatSecs (floor elapsed) <> "/" <> formatSecs (floor total)
          , W.hBox
              [ W.str $ "VOL  " <> show (st ^. stConfig . csVolume) <> "%"
              , W.padLeft W.Max $ drawNamed st RewindButton
              , W.padLeft (W.Pad 1) $ drawNamed st PlayButton
              , W.padLeft (W.Pad 1) $ drawNamed st ForwardButton
              ]
          ]
      , drawNamed st VolumeBar
      ]
 where
  (elapsed, total) = fromMaybe (0, 0) $ st ^. stShownCurrentTime

drawCurrentQueueList :: St -> Widget (MName St)
drawCurrentQueueList st =
  W.vBox
    [ W.hBox
        [ withAttr (attrName "label") $ W.str " CURRENT QUEUE "
        , W.padLeft W.Max $ drawNamed st ShuffleButton
        , W.padLeft (W.Pad 1) $ drawNamed st ReverseOrderButton
        , W.padLeft (W.Pad 1) . W.padRight (W.Pad 1) $ drawNamed st ClearButton
        ]
    , drawNamed st QueueSongList
    ]

drawEqualizerPanel :: St -> Widget (MName St)
drawEqualizerPanel st
  | Map.null (st ^. stConfig . csEQConfigs) = W.emptyWidget
drawEqualizerPanel st =
  W.vBox
    [ W.hBox
        [ withAttr
            (attrName "label")
            (W.str " EQUALIZER ")
        , W.padLeft W.Max $ drawNamed st EQSwitch
        ]
    , W.hBox
        [ W.hLimit 11 $ drawNamed st EQConfigList
        , case st ^. stIsTriggered (mName EQSwitch) of
            False -> drawNamed st EQCurveVisualizer
            True -> W.padLeft (W.Pad 1) $ drawNamed st EQGainBarsViewport
        ]
    ]

drawBottomBar :: St -> Widget (MName St)
drawBottomBar st =
  W.vLimit 1 $
    W.hBox
      [ withAttr (attrName "bottomLabel") $
          W.padLeftRight 1 . W.str . formatMode $
            mode
      , drawIfCmd $ drawNamed st CommandEditor
      ]
 where
  mode = st ^. stMode
  isCommand = mode == CommandMode
  drawIfCmd w = if isCommand then w else W.emptyWidget
