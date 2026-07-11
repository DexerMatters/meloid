{-# LANGUAGE ViewPatterns #-}

-- | This module provides widgets for lists.
module Widgets.Lists (
  AllAlbumList (..),
  AllAlbumEntry (..),
  AlbumSongList (..),
  AlbumSongEntry (..),
  AlbumArtThumb (..),
  QueueSongList (..),
  QueueSongEntry (..),
  EQConfigList (..),
  EQConfigEntry (..),
  MenuEntry (..),
  drawMenuLayer,
) where

import Brick
import Brick.Widgets.Core qualified as W
import Data.Map qualified as Map
import Data.Vector qualified as Vec
import Lens.Micro
import Lens.Micro.Mtl
import Network.MPD qualified as MPD
import Types
import Widgets.Common
import Widgets.Visual.Art (lookupAlbumThumbRenderedImage, renderImage)

data AllAlbumList = AllAlbumList

data AllAlbumEntry = AllAlbumEntry Int

data AlbumSongList = AlbumSongList

data AlbumSongEntry = AlbumSongEntry Int

data AlbumArtThumb = AlbumArtThumb Int

data QueueSongList = QueueSongList

data QueueSongEntry = QueueSongEntry Int

data EQConfigList = EQConfigList

data EQConfigEntry = EQConfigEntry Int

data MenuEntry = MenuEntry Int

selectedAlbumSongs :: St -> Vec.Vector MPD.Song
selectedAlbumSongs st =
  maybe Vec.empty albumSongs $
    (st ^. stSelectedAlbum) >>= ((st ^. stConfig . csAllAlbums) Vec.!?)

instance Drawable St AllAlbumList where
  draw _ st =
    drawAlbumList
      st
      (mName AllAlbumList)
      AllAlbumEntry
      (drawNamed st . AlbumArtThumb)
      (st ^. stConfig . csAllAlbums)
  onMouseScrollUp _ = Just $ scrollViewportBy (mName AllAlbumList) (negate $ snd albumArtThumbSize)
  onMouseScrollDown _ = Just $ scrollViewportBy (mName AllAlbumList) (snd albumArtThumbSize)
  parent _ = Just (ParentView MainView)

instance Drawable St AllAlbumEntry where
  draw (AllAlbumEntry i) st =
    case (st ^. stConfig . csAllAlbums) Vec.!? i of
      Nothing -> W.emptyWidget
      Just album ->
        drawGeneralButton st (mName $ AllAlbumEntry i) $
          W.withAttr (attrName "header") $
            strClippedWithEllipsis (albumName album)
  onMouseLeftUp (AllAlbumEntry i) = Just $ \_ -> stSelectedAlbum .= Just i
  parent (AllAlbumEntry _) = Just (ParentName (mName AllAlbumList))
  variant (AllAlbumEntry i) = i

instance Drawable St AlbumSongList where
  draw _ st =
    drawSongList
      st
      (mName AlbumSongList)
      AlbumSongEntry
      (selectedAlbumSongs st)
  onMouseScrollUp _ = Just $ scrollViewportBy (mName AlbumSongList) (-1)
  onMouseScrollDown _ = Just $ scrollViewportBy (mName AlbumSongList) 1
  parent _ = Just (ParentView MainView)

instance Drawable St AlbumSongEntry where
  draw (AlbumSongEntry i) st =
    drawSongRow st AlbumSongEntry i (selectedAlbumSongs st) songTrack
  onMouseLeftUp (AlbumSongEntry i) = Just $ \_ -> do
    st <- get
    sendRequest . MPDOperation . pure $ do
      let song = selectedAlbumSongs st Vec.! i
      MPD.add (MPD.sgFilePath song)
    sendRequest SignalCurrentQueue
  parent (AlbumSongEntry _) = Just (ParentName (mName AlbumSongList))
  variant (AlbumSongEntry i) = i

instance Drawable St AlbumArtThumb where
  draw (AlbumArtThumb i) st =
    renderImage albumArtThumbSize $
      lookupAlbumThumbRenderedImage st i albumArtThumbSize
  willReportExtent _ = True
  parent (AlbumArtThumb i) = Just (ParentName (mName $ AllAlbumEntry i))
  variant (AlbumArtThumb i) = i

instance Drawable St QueueSongList where
  draw _ st =
    drawSongList st (mName QueueSongList) QueueSongEntry $
      st ^. stPlaying . psCurrentQueue
  onMouseScrollUp _ = Just $ scrollViewportBy (mName QueueSongList) (-1)
  onMouseScrollDown _ = Just $ scrollViewportBy (mName QueueSongList) 1
  parent _ = Just (ParentView MainView)

instance Drawable St QueueSongEntry where
  draw (QueueSongEntry i) st =
    currentPlaying $
      drawSongRow st QueueSongEntry i (st ^. stPlaying . psCurrentQueue) (const (show (i + 1)))
   where
    currentPlaying
      | st ^. stCurrentSongPos == Just i =
          (W.str "> " <+>)
      | otherwise = id
  variant (QueueSongEntry i) = i
  parent (QueueSongEntry _) = Just (ParentName (mName QueueSongList))
  onMouseLeftUp (QueueSongEntry i) = Just $ \_ -> do
    stPlaying . psPaused .= False
    sendRequest . MPDOperation . pure $ MPD.play (Just i)

instance Drawable St EQConfigList where
  draw n st =
    viewportWithBar st (mName n) $
      W.vBox $
        map
          (drawNamed st . EQConfigEntry)
          [0 .. Map.size (st ^. stConfig . csEQConfigs) - 1]
  onMouseScrollUp _ = Just $ scrollViewportBy (mName EQConfigList) (-1)
  onMouseScrollDown _ = Just $ scrollViewportBy (mName EQConfigList) 1
  parent _ = Just (ParentView MainView)

instance Drawable St EQConfigEntry where
  draw (EQConfigEntry i) st =
    drawGeneralButton st (mName $ EQConfigEntry i) $
      strClippedWithEllipsis (tip <> text)
   where
    tip
      | st ^. stCurrentEQIndex == Just i = "> "
      | otherwise = ""
    -- SAFETY: EQConfigEntry is indexed within the length of EQConfigs
    text = st ^. stConfig . csEQConfigs . to (fst . (Map.elemAt i))
  variant (EQConfigEntry i) = i
  parent _ = Just (ParentName (mName EQConfigList))
  onMouseLeftUp (EQConfigEntry i) = Just $ \_ -> do
    eqs <- use $ stConfig . csEQConfigs
    let newId = fst $ Map.elemAt i eqs
    stConfig . csConfigs . cvEq .= newId
    paused <- use $ stPlaying . psPaused
    sendRequest (UpdateEQId newId)
    sendRequest $ MPDOperation [MPD.pause paused]

drawMenuLayer :: St -> Widget (MName St)
drawMenuLayer st = case st ^. stMenu of
  Just _ -> loc menu
  _ -> W.emptyWidget
 where
  loc = case st ^. stPressed of
    Just n -> W.relativeTo n (curry Location 0 0)
    _ -> id
  menu = case st ^. stMenu of
    (fmap length -> Just len) ->
      W.withDefAttr (attrName "secondary") $
        W.vBox $
          map (drawNamed st . MenuEntry) [0 .. len - 1]
    _ -> W.emptyWidget

instance Drawable St MenuEntry where
  draw (MenuEntry i) st = case st ^. stMenu of
    -- SAFETY: MenuEntry is indexed within the length of the menu
    -- So the menu must exist
    (fmap (!! i) -> Just (name, _)) ->
      drawButton
        st
        (mName $ MenuEntry i)
        (name <> "          ")
    _ -> W.emptyWidget
  parent _ = Just (ParentView MainView)
  variant (MenuEntry i) = i
  onMouseLeftUp (MenuEntry i) = Just $ \_ ->
    use stMenu >>= \case
      (fmap (!! i) -> Just (_, action)) ->
        action
          >> stMenu .= Nothing
      _ -> pure ()
