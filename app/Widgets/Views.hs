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
) where

import Brick
import Brick.Widgets.Center qualified as C
import Brick.Widgets.Core qualified as W
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Vector qualified as Vec
import Lens.Micro
import Network.MPD qualified as MPD
import Types
import Widgets.Common
import Widgets.Controls
import Widgets.Edits (CommandEditor (CommandEditor))
import Widgets.Images (AlbumArtPlaying (..), lookupAlbumThumbRenderedImage)
import Widgets.Lists (
  AlbumArtThumb (..),
  AlbumSongList (..),
  AllAlbumList (..),
  QueueSongList (..),
 )

data DebugViewport = DebugViewport

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

instance Drawable St ViewName where
  draw MainView st =
    W.vBox
      [ W.hBox
          [ drawControlPanel st
          , W.padLeft (W.Pad 2) . W.padRight (W.Pad 1) $ drawSongPanel st
          , drawNamed st AlbumArtPlaying
          ]
      , W.padTop (W.Pad 1) . W.hBox $
          [ W.hLimitPercent 40 $ drawNamed st AllAlbumList
          , W.padLeft (W.Pad 1) $
              W.vBox
                [ drawAlbumSongList st
                , drawCurrentQueueList st
                ]
          ]
      , drawBottomBar st
      ]
  draw DebugView st = drawNamed st DebugViewport
  draw WelcomeDialog st =
    C.center $
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
  draw SimpleDialog st =
    C.center $
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
            [ W.withAttr (logAttr logLevel) $ W.strWrap msg
            | (logLevel, msg) <- st ^. stLogs
            ]
  parent _ = Just (ParentView DebugView)
  handlesMouseScrollUp _ = True
  handlesMouseScrollDown _ = True
  onMouseScrollUp _ = scrollViewportBy (mName DebugViewport) (-1)
  onMouseScrollDown _ = scrollViewportBy (mName DebugViewport) 1

drawAlbumSongList :: St -> Widget (MName St)
drawAlbumSongList st =
  case selected of
    Nothing -> W.emptyWidget
    Just _ ->
      W.vBox
        [ W.hBox
            [ withAttr (attrName "label") (W.str " TRACKS ")
            , W.padLeft W.Max $
                withAttr (attrName "meta") . W.hLimitPercent 80 $
                  strClippedWithEllipsis album
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
      [ W.hBox
          [ W.vBox
              [ drawNamed st IncreaseVolumeButton
              , drawNamed st DecreaseVolumeButton
              ]
          , W.vBox
              [ W.str $ "TIME " <> formatSecs (floor elapsed) <> "/" <> formatSecs (ceiling total)
              , W.hBox
                  [ W.str $ "VOL  " <> show (st ^. stConfig . csVolume) <> "%"
                  , W.padLeft W.Max $ drawNamed st RewindButton
                  , W.padLeft (W.Pad 1) $ drawNamed st PlayButton
                  , W.padLeft (W.Pad 1) $ drawNamed st ForwardButton
                  ]
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

drawBottomBar :: St -> Widget (MName St)
drawBottomBar st =
  W.vLimit 1 $
    W.hBox
      [ withAttr (attrName "bottomLabel") $
          W.padLeftRight 1 $
            W.str (formatMode mode)
      , case mode of
          CommandMode ->
            W.hBox
              [ withAttr (attrName "label") $ W.str $ commandLabel command stage
              , W.padLeft (W.Pad 1) $ drawNamed st CommandEditor
              ]
          -- TODO: `strWrap` contradicts with `W.vLimit 1`.
          -- It is recommended to make the bottom bar multi-line in case
          -- that the content is too long.
          NormalMode -> W.padLeft (W.Pad 1) $ withAttr (logAttr logLevel) $ W.strWrap content
          EditMode -> W.emptyWidget
      ]
 where
  mode = st ^. stMode
  (logLevel, content) = st ^. stInlineOutput
  command
    | st ^. stIsProceedingCmd = st ^. stCommandStages . cpsCommand
    | otherwise = ""
  stage =
    st ^. stCurrentStage & \case
      Just (InputStage s _ _) -> s
      _ -> ""
  commandLabel cmd currentStage
    | null cmd && null currentStage = ""
    | null cmd = "/" <> currentStage <> ": "
    | null currentStage = cmd
    | otherwise = cmd <> "/" <> currentStage <> ": "
