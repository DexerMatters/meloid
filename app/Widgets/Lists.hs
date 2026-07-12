{-# LANGUAGE ViewPatterns #-}

-- | This module provides widgets for lists.
module Widgets.Lists (
  AllAlbumList (..),
  AllAlbumEntry (..),
  TrackList (..),
  AlbumSongEntry (..),
  QueueSongList (..),
  QueueSongEntry (..),
  SongInfoList (..),
  EQConfigList (..),
  EQConfigEntry (..),
  MenuEntry (..),
  drawMenuLayer,
) where

import Brick
import Brick qualified as B
import Brick.Widgets.Core qualified as W
import Compat.Software (extractExtraInfo)
import Data.Bool (bool)
import Data.List (intercalate)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Time qualified as Time
import Data.Vector qualified as Vec
import Lens.Micro
import Lens.Micro.Mtl
import Network.MPD qualified as MPD
import Types
import Widgets.Common
import Widgets.Image (AlbumImageClip (..), albumThumbnailSize, drawAlbumThumbnail)

data AllAlbumList = AllAlbumList

data AllAlbumEntry = AllAlbumEntry Int

data TrackList = TrackList

data AlbumSongEntry = AlbumSongEntry Int

data QueueSongList = QueueSongList

data QueueSongEntry = QueueSongEntry Int

data SongInfoList = SongInfoList

data EQConfigList = EQConfigList

data EQConfigEntry = EQConfigEntry Int

data MenuEntry = MenuEntry Int

albumThumbnailHeight :: Int
albumThumbnailHeight = snd albumThumbnailSize

instance Drawable St AllAlbumList where
  draw _ st =
    B.reportExtent (mName AlbumImageClip) $
      drawAlbumList
        st
        (mName AlbumImageClip)
        AllAlbumEntry
        (drawAlbumThumbnail st)
        albumThumbnailHeight
        (st ^. stConfig . csAllAlbums)
  onMouseScrollUp _ = Just $ scrollViewportBy (mName AlbumImageClip) (negate albumThumbnailHeight)
  onMouseScrollDown _ = Just $ scrollViewportBy (mName AlbumImageClip) albumThumbnailHeight
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

instance Drawable St TrackList where
  draw _ st =
    drawSongList
      st
      (mName TrackList)
      AlbumSongEntry
      (st ^. stSelectedAlbumSongs)
  onMouseScrollUp _ = Just $ scrollViewportBy (mName TrackList) (-1)
  onMouseScrollDown _ = Just $ scrollViewportBy (mName TrackList) 1
  parent _ = Just (ParentView MainView)

instance Drawable St AlbumSongEntry where
  draw (AlbumSongEntry i) st =
    drawSongRow st AlbumSongEntry i (st ^. stSelectedAlbumSongs) songTrack
  onMouseDoubleClick (AlbumSongEntry i) = Just $ \_ -> do
    song <- use stSelectedAlbumSongs <&> (Vec.! i)
    sendRequest $ MPDOperation [MPD.add (MPD.sgFilePath song)]
    sendRequest SignalCurrentQueue
  onMouseLeftUp (AlbumSongEntry i) = Just $ \_ -> do
    song <- use stSelectedAlbumSongs <&> (Vec.! i)
    songExInfo <- extractExtraInfo song
    case songExInfo of
      Right info -> stSelectedSong .= Just (song, info)
      Left err -> logReqWarn "ffprobe" err
  parent (AlbumSongEntry _) = Just (ParentName (mName TrackList))
  variant (AlbumSongEntry i) = i

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

instance Drawable St SongInfoList where
  draw _ st =
    viewportWithBar st (mName SongInfoList) . W.vBox $
      [ header "MUSIC INFO: "
      , "Disc" <-> (intercalate ", " $ NonEmpty.toList $ meta MPD.Disc)
      , "Track" <-> (h $ meta MPD.Track)
      , "Name" <-> (h $ meta MPD.Title)
      , "Artist" <-> (intercalate ", " $ NonEmpty.toList $ meta MPD.Artist)
      , "Album" <-> (intercalate ", " $ NonEmpty.toList $ meta MPD.Album)
      , "Genre" <-> (intercalate "/" $ NonEmpty.toList $ meta MPD.Genre)
      , "Date" <-> (h $ meta MPD.Date)
      , "Comment" <-> (h $ meta MPD.Comment)
      , "Label" <-> (h $ meta MPD.Label)
      , header "\nFILE INFO: "
      , "Location" <-> (st ^. stSelectedSong .? to (MPD.toString . MPD.sgFilePath . fst))
      , "Last Modified" <-> (st ^. stSelectedSong .? to (formatTime . MPD.sgLastModified . fst))
      , "Size" <-> (st ^. stSelectedSong .? to (songSize . snd))
      , "Length" <-> (st ^. stSelectedSong .? to (formatSecs . MPD.sgLength . fst))
      , "Bitrate" <-> (st ^. stSelectedSong .? to (songBitRate . snd))
      , "Sample Rate" <-> (st ^. stSelectedSong .? to (songSampleRate . snd))
      , "Channels" <-> (st ^. stSelectedSong .? to (songChannels . snd))
      ]
   where
    h = NonEmpty.head
    meta m = st ^. stSelectedSongMeta m
    formatTime = \case
      Just t -> Time.formatTime Time.defaultTimeLocale "%Y-%m-%d %H:%M:%S" t
      Nothing -> "Unknown"
    key <-> value =
      W.hBox
        [ W.withAttr (attrName "text") $ W.str "- "
        , W.hLimit 15 . W.vLimit 1 $
            W.hBox
              [ W.withAttr (attrName "header") $ W.str key
              , W.withAttr (attrName "text") $ W.fill (bool '.' ' ' $ null value)
              ]
        , W.strWrap value
        ]
    header text = W.withAttr (attrName "text") $ W.str text
  onMouseScrollUp _ = Just $ scrollViewportBy (mName SongInfoList) (-1)
  onMouseScrollDown _ = Just $ scrollViewportBy (mName SongInfoList) 1
  parent _ = Just (ParentView MainView)

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
