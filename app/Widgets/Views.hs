{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

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
  drawView,
  drawDialogView,
) where

import Brick
import Brick.Widgets.Center qualified as C
import Brick.Widgets.Core qualified as W
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe)
import Lens.Micro
import Network.MPD qualified as MPD
import Types
import Widgets.Common
import Widgets.Controls
import Widgets.Edits (CommandEditor (CommandEditor))
import Widgets.Elements.Element (ElementName (..))
import Widgets.Image (drawPlayingImage)

data DebugViewport = DebugViewport

drawView :: ViewName -> St -> Widget (MName St)
drawView MainView st =
  W.vBox
    [ W.hBox
        [ drawControlPanel st
        , W.padLeft (W.Pad 2) . W.padRight (W.Pad 1) $ drawSongPanel st
        , W.hLimit 6 . W.vLimit 3 $ drawPlayingImage st
        ]
    , W.padTop (W.Pad 1) . W.padBottom W.Max $
        drawNamed st (ElementName [])
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
      [ C.hCenter $ W.str "Welcome to Meloid"
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
      [ C.hCenter $ W.str "Welcome to Meloid"
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
