-- | This module provides widgets for lists.
module Widgets.Lists (
  AllAlbumList (..),
  AllAlbumEntry (..),
  TrackList (..),
  TrackSongEntry (..),
  PlaylistList (..),
  PlaylistEntry (..),
  QueueSongList (..),
  QueueSongEntry (..),
  SongInfoList (..),
  EQConfigList (..),
  EQConfigEntry (..),
  stCurrentEQ,
  stCurrentEQIndex,
  MenuEntry (..),
  drawMenuLayer,
  menuFocusChildren,
) where

import Brick
import Brick qualified as B
import Brick.Widgets.Border qualified as Bd
import Brick.Widgets.Core qualified as W
import Compat.Software (extractExtraInfo)
import Data.Bool (bool)
import Data.List (intercalate)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Time qualified as Time
import Data.Vector qualified as Vec
import Lens.Micro
import Lens.Micro.Mtl
import Network.MPD qualified as MPD
import Types
import Widgets.Common
import Widgets.Elements.Common (ElementNode (..), ElementPath, pathVariant)
import Widgets.Image (AlbumImageClip (..), albumThumbnailSize, drawAlbumThumbnail)

data AllAlbumList = AllAlbumList ElementPath

data AllAlbumEntry = AllAlbumEntry ElementPath Int

data TrackList = TrackList ElementPath

data TrackSongEntry = TrackSongEntry ElementPath Int

data QueueSongList = QueueSongList ElementPath

data QueueSongEntry = QueueSongEntry ElementPath Int

data SongInfoList = SongInfoList ElementPath

data PlaylistList = PlaylistList ElementPath

data PlaylistEntry = PlaylistEntry ElementPath Int

data EQConfigList = EQConfigList ElementPath

data EQConfigEntry = EQConfigEntry ElementPath Int
  deriving (Show)

data MenuEntry = MenuEntry Int

stCurrentEQ :: SimpleGetter St EQConfigSpecs
stCurrentEQ = to $ \st ->
  case st ^. stConfig . csEQConfigs of
    EQConfigValue configs ->
      let selected = fromMaybe (st ^. stConfig . csConfigs . cvEq) (st ^. stSelectedEQConfig)
       in Map.findWithDefault (EQConfigSpecs $ replicate (length eqFrequencies) 0) selected configs

stCurrentEQIndex :: SimpleGetter St (Maybe Int)
stCurrentEQIndex = to $ \st ->
  case st ^. stConfig . csEQConfigs of
    EQConfigValue configs ->
      let selected = fromMaybe (st ^. stConfig . csConfigs . cvEq) (st ^. stSelectedEQConfig)
       in Map.lookupIndex selected configs

albumThumbnailHeight :: Int
albumThumbnailHeight = snd albumThumbnailSize

instance Drawable St AllAlbumList where
  draw (AllAlbumList path) st =
    B.reportExtent (mName $ AlbumImageClip path) $
      drawAlbumList
        st
        (mName $ AlbumImageClip path)
        (AllAlbumEntry path)
        (drawAlbumThumbnail st path)
        albumThumbnailHeight
        (st ^. stConfig . csAllAlbums)
  onMouseScrollUp (AllAlbumList path) = Just $ scrollViewportBy (mName $ AlbumImageClip path) (negate albumThumbnailHeight)
  onMouseScrollDown (AllAlbumList path) = Just $ scrollViewportBy (mName $ AlbumImageClip path) albumThumbnailHeight
  parent (AllAlbumList path) = Just . ParentName . mName $ ElementNode path
  variant (AllAlbumList path) = pathVariant path
  focusChildren (AllAlbumList path) st =
    [ mName $ AllAlbumEntry path i
    | i <- [0 .. Vec.length (st ^. stConfig . csAllAlbums) - 1]
    ]

instance Drawable St AllAlbumEntry where
  draw (AllAlbumEntry path i) st =
    case (st ^. stConfig . csAllAlbums) Vec.!? i of
      Nothing -> W.emptyWidget
      Just album ->
        drawGeneralButton st (mName $ AllAlbumEntry path i) $
          W.withAttr (attrName "header") $
            strClippedWithEllipsis (albumName album)
  onMouseLeftUp (AllAlbumEntry _ i) = Just $ \_ -> selectAlbum i
  parent (AllAlbumEntry path _) = Just (ParentName $ mName $ AllAlbumList path)
  variant (AllAlbumEntry _ i) = i
  focusBinding _ _ = Just FocusPassive
  onFocus (AllAlbumEntry _ i) _ = Just $ selectAlbum i

instance Drawable St TrackList where
  draw (TrackList path) st =
    drawSongList
      st
      (mName $ TrackList path)
      (TrackSongEntry path)
      (st ^. stSelectedTrackSongs)
  onMouseScrollUp (TrackList path) = Just $ scrollViewportBy (mName $ TrackList path) (-1)
  onMouseScrollDown (TrackList path) = Just $ scrollViewportBy (mName $ TrackList path) 1
  parent (TrackList path) = Just . ParentName . mName $ ElementNode path
  variant (TrackList path) = pathVariant path
  focusChildren (TrackList path) st =
    [ mName $ TrackSongEntry path i
    | i <- [0 .. Vec.length (st ^. stSelectedTrackSongs) - 1]
    ]

instance Drawable St TrackSongEntry where
  draw (TrackSongEntry path i) st =
    drawSongRow st (TrackSongEntry path) i (st ^. stSelectedTrackSongs) songTrack
  onMouseDoubleClick (TrackSongEntry _ i) = Just $ \_ -> queueTrackSong i
  onMouseLeftUp (TrackSongEntry _ i) = Just $ \_ -> selectTrackSong i
  parent (TrackSongEntry path _) = Just (ParentName $ mName $ TrackList path)
  variant (TrackSongEntry _ i) = i
  focusBinding (TrackSongEntry _ i) _ = Just $ FocusAction (queueTrackSong i)
  onFocus (TrackSongEntry _ i) _ = Just $ selectTrackSong i

instance Drawable St PlaylistList where
  draw (PlaylistList path) st =
    viewportWithBar st (mName $ PlaylistList path) . W.vBox $
      map
        (drawNamed st . PlaylistEntry path)
        [0 .. Vec.length (st ^. stConfig . csAllPlaylists) - 1]
  onMouseScrollUp (PlaylistList path) = Just $ scrollViewportBy (mName $ PlaylistList path) (-1)
  onMouseScrollDown (PlaylistList path) = Just $ scrollViewportBy (mName $ PlaylistList path) 1
  parent (PlaylistList path) = Just . ParentName . mName $ ElementNode path
  variant (PlaylistList path) = pathVariant path
  focusChildren (PlaylistList path) st =
    [ mName $ PlaylistEntry path i
    | i <- [0 .. Vec.length (st ^. stConfig . csAllPlaylists) - 1]
    ]

instance Drawable St PlaylistEntry where
  draw (PlaylistEntry path i) st =
    case (st ^. stConfig . csAllPlaylists) Vec.!? i of
      Nothing -> W.emptyWidget
      Just playlist ->
        drawGeneralButton st (mName $ PlaylistEntry path i) $
          W.withAttr (attrName "header") $
            strClippedWithEllipsis (MPD.toString $ playlistName playlist)
  onMouseLeftUp (PlaylistEntry _ i) = Just $ \_ -> selectPlaylist i
  parent (PlaylistEntry path _) = Just (ParentName $ mName $ PlaylistList path)
  variant (PlaylistEntry _ i) = i
  focusBinding _ _ = Just FocusPassive
  onFocus (PlaylistEntry _ i) _ = Just $ selectPlaylist i

instance Drawable St QueueSongList where
  draw (QueueSongList path) st =
    drawSongList st (mName $ QueueSongList path) (QueueSongEntry path) $
      st ^. stPlaying . psCurrentQueue
  onMouseScrollUp (QueueSongList path) = Just $ scrollViewportBy (mName $ QueueSongList path) (-1)
  onMouseScrollDown (QueueSongList path) = Just $ scrollViewportBy (mName $ QueueSongList path) 1
  parent (QueueSongList path) = Just . ParentName . mName $ ElementNode path
  variant (QueueSongList path) = pathVariant path
  focusChildren (QueueSongList path) st =
    [ mName $ QueueSongEntry path i
    | i <- [0 .. Vec.length (st ^. stPlaying . psCurrentQueue) - 1]
    ]

instance Drawable St QueueSongEntry where
  draw (QueueSongEntry path i) st =
    currentPlaying $
      drawSongRow st (QueueSongEntry path) i (st ^. stPlaying . psCurrentQueue) (const (show (i + 1)))
   where
    currentPlaying
      | st ^. stCurrentSongPos == Just i =
          (W.str "> " <+>)
      | otherwise = id
  variant (QueueSongEntry _ i) = i
  parent (QueueSongEntry path _) = Just (ParentName $ mName $ QueueSongList path)
  onMouseLeftUp (QueueSongEntry _ i) = Just $ \_ -> do
    playQueueSong i
  focusBinding (QueueSongEntry _ i) _ = Just $ FocusAction (playQueueSong i)
  onFocus (QueueSongEntry _ i) _ = Just $ selectQueueSong i

instance Drawable St SongInfoList where
  draw (SongInfoList path) st =
    viewportWithBar st (mName $ SongInfoList path) . W.vBox $
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
  onMouseScrollUp (SongInfoList path) = Just $ scrollViewportBy (mName $ SongInfoList path) (-1)
  onMouseScrollDown (SongInfoList path) = Just $ scrollViewportBy (mName $ SongInfoList path) 1
  parent (SongInfoList path) = Just . ParentName . mName $ ElementNode path
  variant (SongInfoList path) = pathVariant path
  focusBinding (SongInfoList path) _ = Just $ FocusAdjust (scrollTransaction $ mName $ SongInfoList path)

instance Drawable St EQConfigList where
  draw (EQConfigList path) st =
    W.hBox
      [ viewportWithBar st (mName $ EQConfigList path) $
          W.vBox $
            map
              (drawNamed st . EQConfigEntry path)
              [0 .. Map.size configs - 1]
      , B.withAttr (attrName "text") . W.hLimit 1 $ W.fill '¦'
      ]
   where
    EQConfigValue configs = st ^. stConfig . csEQConfigs
  onMouseScrollUp (EQConfigList path) = Just $ scrollViewportBy (mName $ EQConfigList path) (-1)
  onMouseScrollDown (EQConfigList path) = Just $ scrollViewportBy (mName $ EQConfigList path) 1
  parent (EQConfigList path) = Just . ParentName . mName $ ElementNode path
  variant :: EQConfigList -> Int
  variant (EQConfigList path) = pathVariant path
  focusChildren (EQConfigList path) st =
    case st ^. stConfig . csEQConfigs of
      EQConfigValue configs -> [mName $ EQConfigEntry path i | i <- [0 .. Map.size configs - 1]]

instance Drawable St EQConfigEntry where
  draw (EQConfigEntry path i) st =
    drawGeneralButton st (mName $ EQConfigEntry path i) $
      with $
        W.strWrap (tip <> text <> star)
   where
    tip
      | st ^. stSelectedEQConfig == Just text = "> "
      | otherwise = ""
    -- SAFETY: EQConfigEntry is indexed within the length of EQConfigs
    text = fst $ Map.elemAt i configs
    star
      | st ^. stUnsaved . usEQ == Just i = "*"
      | otherwise = ""
    with
      | st ^. stUnsaved . usEQ == Just i = B.withDefAttr (attrName "unsaved")
      | otherwise = id
    EQConfigValue configs = st ^. stConfig . csEQConfigs
  variant (EQConfigEntry _ i) = i
  parent (EQConfigEntry path _) = Just (ParentName $ mName $ EQConfigList path)
  onMouseLeftUp (EQConfigEntry _ i) = Just $ \_ -> do
    selectEQConfig i
  focusBinding _ _ = Just FocusPassive
  onFocus (EQConfigEntry _ i) _ = Just $ selectEQConfig i

drawMenuLayer :: St -> Widget (MName St)
drawMenuLayer st
  | null widgets = W.emptyWidget
  | otherwise = W.relativeTo location offset menu
 where
  MenuSt widgets location offset (menuWidth, menuHeight) = st ^. stMenu
  menu =
    W.hLimit menuWidth . W.padRight W.Max . W.vLimit menuHeight . Bd.border $
      W.vBox $
        map (drawNamed st . MenuEntry) [0 .. length widgets - 1]

menuFocusChildren :: St -> [MName St]
menuFocusChildren st =
  [ mName $ MenuEntry i
  | (i, widget) <- zip [0 ..] (st ^. stMenu . msWidgets)
  , case widget of
      MWHeader{} -> False
      _ -> True
  ]

instance Drawable St MenuEntry where
  draw (MenuEntry i) st = case (st ^. stMenu . msWidgets) !! i of
    -- SAFETY: MenuEntry is indexed within the length of the menu
    -- So the menu must exist
    MWButton name _ ->
      drawButton
        st
        (mName $ MenuEntry i)
        (" " <> name <> "          ")
    MWHeader title ->
      W.withAttr (attrName "text") $ W.str title
    MWSubmenu name _ ->
      drawButton
        st
        (mName $ MenuEntry i)
        (" " <> name <> "          ")
  parent _ = Just (ParentView MainView)
  variant (MenuEntry i) = i
  onMouseLeftUp (MenuEntry i) = Just $ \_ -> do
    activateMenuEntry i

selectAlbum :: Int -> EventM (MName St) St ()
selectAlbum i =
  use (stConfig . csAllAlbums) >>= \albums ->
    mapM_ (\_ -> stSelectedAlbum .= Just i >> stSelectedPlaylist .= Nothing) (albums Vec.!? i)

selectTrackSong :: Int -> EventM (MName St) St ()
selectTrackSong i = withSelectedTrackSong i selectSong

queueTrackSong :: Int -> EventM (MName St) St ()
queueTrackSong i = withSelectedTrackSong i queueSong

selectPlaylist :: Int -> EventM (MName St) St ()
selectPlaylist i =
  use (stConfig . csAllPlaylists) >>= \playlists ->
    mapM_ (\playlist -> stSelectedAlbum .= Nothing >> stSelectedPlaylist .= Just (playlistName playlist)) (playlists Vec.!? i)

selectQueueSong :: Int -> EventM (MName St) St ()
selectQueueSong i = use (stPlaying . psCurrentQueue) >>= withSongAt i selectSong

withSelectedTrackSong :: Int -> (MPD.Song -> EventM (MName St) St ()) -> EventM (MName St) St ()
withSelectedTrackSong i action = use stSelectedTrackSongs >>= withSongAt i action

withSongAt :: Int -> (MPD.Song -> EventM (MName St) St ()) -> Vec.Vector MPD.Song -> EventM (MName St) St ()
withSongAt i action = maybe (pure ()) action . (Vec.!? i)

selectSong :: MPD.Song -> EventM (MName St) St ()
selectSong song = do
  stSelectedSong .= Just (song, defaultExtraInfo)
  extractExtraInfo song >>= \case
    Right info -> stSelectedSong .= Just (song, info)
    Left err -> logReqWarn "ffprobe" err

queueSong :: MPD.Song -> EventM (MName St) St ()
queueSong song = do
  sendRequest $ MPDOperation [MPD.add (MPD.sgFilePath song)]
  sendRequest SignalCurrentQueue

playQueueSong :: Int -> EventM (MName St) St ()
playQueueSong i = do
  stPlaying . psPaused .= False
  stPlaying . psStopped .= False
  sendRequest . MPDOperation . pure $ MPD.play (Just i)

selectEQConfig :: Int -> EventM (MName St) St ()
selectEQConfig i =
  use (stConfig . csEQConfigs) >>= \(EQConfigValue configs) ->
    mapM_ (\(configId, _) -> stSelectedEQConfig .= Just configId) (Map.lookupMin $ Map.drop i configs)

activateMenuEntry :: Int -> EventM (MName St) St ()
activateMenuEntry i = do
  widgets <- use (stMenu . msWidgets)
  location <- use (stMenu . msLocation)
  case widgets !! i of
    MWButton _ action -> action >> closeMenu >> (stFocus %= invalidateFocus)
    MWSubmenu _ sub -> openMenu location sub >> (stFocus %= invalidateFocus)
    MWHeader{} -> pure ()
