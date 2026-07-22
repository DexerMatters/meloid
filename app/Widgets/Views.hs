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
  viewFocusChildren,
) where

import Brick
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

-- | The document-order focus children for each top-level view.  Layout
-- elements expand themselves recursively, so this remains independent of a
-- user's configured panel tree.
viewFocusChildren :: ViewName -> St -> [MName St]
viewFocusChildren MainView _ =
  [ mName RewindButton
  , mName PlayButton
  , mName ForwardButton
  , mName VolumeBar
  , mName SongProgressBar
  , mName (ElementName [])
  ]
viewFocusChildren DebugView _ = [mName DebugViewport]

instance Drawable St DebugViewport where
  draw _ st =
    W.viewport (mName DebugViewport) Vertical $
      W.vBox $
        W.str "Debug view\n\n"
          : reverse
            [ W.withAttr (attrName attrStyle) $ W.strWrap msg
            | (logLevel, msg) <- st ^. stLogs
            , let attrStyle =
                    case logLevel of
                      Debug -> "debugLog"
                      Info -> "infoLog"
                      Warn -> "warnLog"
                      Error -> "errorLog"
            ]
  parent _ = Just (ParentView DebugView)
  onMouseScrollUp _ = Just $ scrollViewportBy (mName DebugViewport) (-1)
  onMouseScrollDown _ = Just $ scrollViewportBy (mName DebugViewport) 1
  focusBinding _ _ = Just $ FocusAdjust beginDebugScroll

beginDebugScroll :: EventM (MName St) St (FocusTransaction St)
beginDebugScroll = scrollTransaction (mName DebugViewport)

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
