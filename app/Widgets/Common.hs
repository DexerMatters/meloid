{-# LANGUAGE LambdaCase #-}

{- | This module provides unnamed widgets used in the UI.
They are mostly just functions that return a widget. Widgets with
limited interaction and usage from other modules are recommended
to be put in this module.
Some related helper functions are also provided.
-}
module Widgets.Common (
  drawButton,
  drawIconButton,
  drawGeneralButton,
  drawAlbumList,
  drawSongList,
  drawSongRow,
  makeBar,
  makeBar',
  openMenu,
  scrollViewportBy,
  strClippedWithEllipsis,
  strFillingAvailableWidth,
  viewportWithBar,
)
where

import Brick
import Brick qualified as B
import Brick.Main qualified as M
import Brick.Widgets.Core qualified as W
import Control.Monad (when)
import Data.List (intercalate)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Typeable (Typeable)
import Data.Vector qualified as Vec
import Graphics.Vty qualified as V
import Lens.Micro (to, (^.))
import Network.MPD qualified as MPD
import Types
import Utils (ceilingDiv)

{- | Draws a button with the ability to change the attributes when
pressed.
-}
drawButton :: St -> MName St -> String -> Widget (MName St)
drawButton st name label =
  W.withDefAttr (attrName "button" <> pressedAttr st name) $
    W.str label

{- | Draws an icon button which is similar to a button but it has a
more conspicuous look. Fonts are also bolder.
-}
drawIconButton :: St -> MName St -> String -> Widget (MName St)
drawIconButton st name label =
  W.withAttr (attrName "iconButton" <> pressedAttr st name) $
    W.str label

{- | Draws a button with a weak pressing effect. The content of the
button can be any widget rather than just a string.
-}
drawGeneralButton :: St -> MName St -> Widget (MName St) -> Widget (MName St)
drawGeneralButton st name inner
  | st ^. stPressed == Just name = W.withDefAttr (attrName "focused") inner
  | otherwise = inner

{- | A scroll bar for a viewport, which provides a draggable thumb
to scroll the viewport.
Please ensure the field argument is a name of a viewport.
-}
data ScrollBar = ScrollBar (MName St)

instance Drawable St ScrollBar where
  draw (ScrollBar target) _ =
    Widget Greedy Greedy $ do
      ctx <- getContext
      maybeViewport <- W.unsafeLookupViewport target
      let height = max 0 $ ctx ^. availHeightL
          (scrollTop, total) =
            maybe (0, 0) viewportBarState maybeViewport
      render $
        W.vBox $
          fmap drawTrackCell $
            scrollbarThumb height total scrollTop
  onMouseLeftDown (ScrollBar target) = Just $ \(Location (ax, ay)) ->
    when (ax == 0) $
      B.lookupViewport target >>= \case
        Nothing ->
          pure ()
        Just viewport' -> do
          let visibleHeight = V.regionHeight $ viewport' ^. B.vpSize
              totalHeight = V.regionHeight $ viewport' ^. B.vpContentSize
              currentTop = viewport' ^. B.vpTop
              thumbHeight = scrollbarThumbHeight visibleHeight totalHeight
              maxThumbTop = max 0 (visibleHeight - thumbHeight)
              clickThumbTop =
                min maxThumbTop $
                  max 0 $
                    ay - thumbHeight `div` 2
              targetTop = thumbTopToScrollTop visibleHeight totalHeight clickThumbTop
              delta = targetTop - currentTop
          M.vScrollBy (B.viewportScroll target) delta
  parent (ScrollBar target) = Just (ParentName target)

pressedAttr :: St -> MName St -> AttrName
pressedAttr st name
  | st ^. stPressed == Just name = attrName "pressed"
  | otherwise = mempty

{- | Draws a string with ellipsis if it is too long.
"Too long" means there is no room for it in the available width.
It is useful when you want to display a possible long string
without wrapping or overflowing.
-}
strClippedWithEllipsis :: String -> Widget n
strClippedWithEllipsis s =
  Widget Greedy Fixed $ do
    ctx <- getContext
    let width = ctx ^. availWidthL
    render . W.str $
      if width <= 0
        then ""
        else
          if length s > width && width > 3
            then take (width - 3) s <> "..."
            else take width s

strFillingAvailableWidth :: String -> Widget n
strFillingAvailableWidth s =
  Widget Greedy Fixed $ do
    ctx <- getContext
    let width = ctx ^. availWidthL
    render . W.str $ s <> replicate (width - length s) ' '

scrollbarThumb :: Int -> Int -> Int -> [Bool]
scrollbarThumb height total scrollTop
  | height <= 0 = []
  | total <= 0 = replicate height True
  | total <= height = replicate height True
  | otherwise =
      [ i >= thumbTop && i < thumbTop + thumbHeight
      | i <- [0 .. height - 1]
      ]
 where
  thumbHeight =
    max 1 $
      min height $
        ceilingDiv (height * height) total
  maxThumbTop = height - thumbHeight
  maxScrollTop = max 1 (total - height)
  thumbTop =
    min maxThumbTop $
      max 0 $
        (maxThumbTop * max 0 scrollTop) `div` maxScrollTop

{- | Produce a string image for a solid progress bar.
Parameters are respectively: the width, the current count, and the
total count.
The solid one has been made for the volume bar.
-}
makeBar :: Int -> Integer -> Integer -> String
makeBar = makeBarWith 8 '█' "▏▎▍▌▋▊▉"

{- | Produce a string image for a braille progress bar.
Parameters are respectively: the width, the current count, and the
total count.
The braille one has been made for the song progress bar.
-}
makeBar' :: Int -> Integer -> Integer -> String
makeBar' = makeBarWith 8 '⣿' "⠁⠃⠇⡇⡏⡟⡿"

makeBarWith :: Int -> Char -> String -> Int -> Integer -> Integer -> String
makeBarWith steps fullChar partialChars fullWidth count total
  | fullWidth <= 0 = ""
  | total <= 0 = blank
  | clampedCount <= 0 = blank
  | otherwise = prefix <> replicate (fullWidth - length prefix) ' '
 where
  blank = replicate fullWidth ' '
  fullWidth' = toInteger fullWidth
  steps' = toInteger steps
  clampedCount = max 0 (min count total)
  scaled = (clampedCount * fullWidth' * steps' + total - 1) `div` total
  fullCells = fromInteger $ scaled `div` steps'
  remainder = fromInteger $ scaled `mod` steps'
  partial
    | remainder == 0 = ""
    | otherwise = [partialChars !! (remainder - 1)]
  prefix = replicate fullCells fullChar <> partial

-- | Draws a list of albums.
drawAlbumList ::
  (Typeable a, Drawable St a) =>
  -- | State
  St ->
  -- | List name
  MName St ->
  -- | Entry name (indexed)
  (Int -> a) ->
  -- | Thumbnail (indexed)
  (Int -> Widget (MName St)) ->
  -- | Row height
  Int ->
  -- | Albums to display
  Vec.Vector Album ->
  Widget (MName St)
drawAlbumList st listName entryName thumbWidget rowHeight albums =
  viewportWithBar st listName . W.vBox $
    Vec.toList $
      Vec.imap (drawAlbumRow st entryName thumbWidget rowHeight) albums

-- | Draw a list of songs
drawSongList ::
  (Typeable a, Drawable St a) =>
  -- | State
  St ->
  -- | List name
  MName St ->
  -- | Entry name (indexed)
  (Int -> a) ->
  -- | Songs to display
  Vec.Vector MPD.Song ->
  Widget (MName St)
drawSongList st listName entryName songs =
  viewportWithBar st listName . W.vBox $
    Vec.toList $
      Vec.imap (\i _ -> W.vLimit 1 $ drawNamed st (entryName i)) songs

-- | A helper function to scroll a viewport.
scrollViewportBy ::
  MName St -> Int -> EventM (MName St) St ()
scrollViewportBy name delta =
  M.vScrollBy (B.viewportScroll name) delta

-- | A vertical viewport with a scroll bar.
viewportWithBar :: St -> MName St -> Widget (MName St) -> Widget (MName St)
viewportWithBar st name inner =
  W.hBox
    [ W.hLimit 1 $ drawNamed st (ScrollBar name)
    , W.viewport name Vertical inner
    ]

{- | Draw the row of an album, which contains the album art, the
title, the artist, and the release date.
-}
drawAlbumRow ::
  (Typeable a, Drawable St a) =>
  -- | State
  St ->
  -- | Entry name (indexed)
  (Int -> a) ->
  -- | Thumbnail (indexed)
  (Int -> Widget (MName St)) ->
  -- | Row height
  Int ->
  -- | Index in the list
  Int ->
  -- | Album infomation
  Album ->
  Widget (MName St)
drawAlbumRow st entryName thumbWidget rowHeight i album =
  W.vLimit rowHeight $
    withSelectedAttr $
      W.hBox
        [ thumbWidget i
        , W.padLeft (W.Pad 1) . W.vBox $
            [ drawNamed st (entryName i)
            , W.withAttr (attrName "meta") $
                strClippedWithEllipsis ("by " <> albumArtistsLine album)
            , W.withAttr (attrName "text") $
                strClippedWithEllipsis (albumReleaseDate album)
            ]
        ]
 where
  withSelectedAttr
    | st ^. stSelectedAlbum == Just i = W.withDefAttr (attrName "secondary")
    | otherwise = id
  albumArtistsLine x =
    case albumArtists x of
      [] -> "Unknown Artist"
      artists -> intercalate ", " artists

{- | Draw a row of a song, which contains the order number, the
title, and the length.
-}
drawSongRow ::
  (Typeable a, Drawable St a) =>
  St ->
  (Int -> a) ->
  Int ->
  Vec.Vector MPD.Song ->
  (MPD.Song -> String) ->
  Widget (MName St)
drawSongRow st entryName i songs orderMapping =
  case songs Vec.!? i of
    Nothing -> W.emptyWidget
    Just song ->
      withStyle song . drawGeneralButton st (mName $ entryName i) $
        W.hBox
          [ W.hLimit 3 . W.withAttr (attrName "text") $
              W.str $
                orderMapping song <> "...."
          , W.withAttr (attrName "header") $
              W.str (NonEmpty.head $ songMeta MPD.Title song)
          , W.withAttr (attrName "text") $ W.fill '.'
          , W.str $ formatSecs (MPD.sgLength song)
          ]
 where
  withStyle song
    | st ^. stSelectedSong . to (fmap fst) == Just song =
        W.withDefAttr (attrName "focused")
    | otherwise = id

drawTrackCell :: Bool -> Widget (MName St)
drawTrackCell True =
  W.withAttr (attrName "scrollBarThumb") $
    W.str "▏"
drawTrackCell False =
  W.withAttr (attrName "scrollBarTrack") $
    W.str " "

scrollbarThumbHeight :: Int -> Int -> Int
scrollbarThumbHeight visibleHeight totalHeight
  | visibleHeight <= 0 = 0
  | totalHeight <= visibleHeight = visibleHeight
  | otherwise =
      max 1 $
        min visibleHeight $
          ceilingDiv (visibleHeight * visibleHeight) totalHeight

thumbTopToScrollTop :: Int -> Int -> Int -> Int
thumbTopToScrollTop visibleHeight totalHeight thumbTop
  | visibleHeight <= 0 = 0
  | totalHeight <= visibleHeight = 0
  | otherwise =
      min maxScrollTop $
        max 0 $
          (maxScrollTop * clampedThumbTop) `div` maxThumbTop
 where
  thumbHeight = scrollbarThumbHeight visibleHeight totalHeight
  maxThumbTop = max 1 (visibleHeight - thumbHeight)
  maxScrollTop = totalHeight - visibleHeight
  clampedThumbTop = min (visibleHeight - thumbHeight) $ max 0 thumbTop

viewportBarState :: B.Viewport -> (Int, Int)
viewportBarState vp =
  (vp ^. B.vpTop, V.regionHeight $ vp ^. B.vpContentSize)
