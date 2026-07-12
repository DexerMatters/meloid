{- | Pure derived helpers for albums, songs, and current playback.
These functions are intentionally kept state-only so they remain
cheap to reuse from multiple modules.
-}
module Types.Helpers (
  albumArtPlayingSize,
  albumArtThumbSize,
  defaultAlbum,
  songMeta,
  songTrack,
  sortSongsByTrack,
  songAlbumArtKey,
  albumArtKey,
  stCurrentAlbum,
  stCurrentAlbumArt,
  stCurrentSongMeta',
  stCurrentSongMeta,
  stSelectedSongMeta,
  stCurrentSongPos,
  stSelectedAlbumSongs,
  stCurrentEQ,
  stCurrentEQIndex,
  stLayoutElement,
  stShownCurrentTime,
  stIsTriggered,
  formatSecs,
  (.?),
) where

import Data.List (sortBy, (!?))
import Data.List.NonEmpty (NonEmpty, fromList)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set qualified as Set
import Data.Vector qualified as Vec
import Lens.Micro (to, (<&>), (^.), _Just)
import Lens.Micro.Type (SimpleGetter)
import Network.MPD qualified as MPD
import Text.Read (readMaybe)
import Types.Core
import Types.Identity (MName)
import Types.Model
import Types.Schemas
import Utils (formatSecs)

-- | Size reserved for the large now-playing art slot.
albumArtPlayingSize :: ImageSize
albumArtPlayingSize = (6, 3)

-- | Size reserved for album thumbnails in list rows.
albumArtThumbSize :: ImageSize
albumArtThumbSize = (6, 3)

-- | A blank album record used as a harmless fallback.
defaultAlbum :: Album
defaultAlbum =
  Album "" [] Vec.empty "" ""

-- | Read metadata from a song, providing a stable fallback string.
songMeta :: MPD.Metadata -> MPD.Song -> NonEmpty String
songMeta meta song =
  fromMaybe (pure $ unknown meta) (MPD.sgTags song Map.!? meta <&> fromList . fmap MPD.toString)
 where
  unknown MPD.Artist = "Unknown Artist"
  unknown MPD.Album = "Unknown Album"
  unknown MPD.Title = "Unknown Title"
  unknown _ = "?"

-- | Extract the track number as a string for ordering and display.
songTrack :: MPD.Song -> String
songTrack = NonEmpty.head . songMeta MPD.Track

-- | Sort songs by track number when available.
sortSongsByTrack :: [MPD.Song] -> Vec.Vector MPD.Song
sortSongsByTrack = Vec.fromList . sortBy orderSongs
 where
  orderSongs a b =
    case (readMaybe (songTrack a) :: Maybe Int, readMaybe (songTrack b) :: Maybe Int) of
      (Just a', Just b') -> compare a' b'
      _ -> compare (songTrack a) (songTrack b)

-- | Derive a stable album-art key from a song.
songAlbumArtKey :: MPD.Song -> AlbumArtKey
songAlbumArtKey song =
  case tag MPD.Album of
    Just album -> "album:" <> fromMaybe "" (tag MPD.Artist) <> "\0" <> album
    Nothing -> "file:" <> MPD.toString (MPD.sgFilePath song)
 where
  tag meta = MPD.toString <$> (MPD.sgTags song Map.!? meta >>= listToMaybe)

-- | Derive a stable album-art key from an album record.
albumArtKey :: Album -> AlbumArtKey
albumArtKey album =
  case albumSongs album Vec.!? 0 of
    Just song -> songAlbumArtKey song
    Nothing -> "album:" <> concat (albumArtists album) <> "\0" <> albumName album

-- | The album that matches the currently playing song, if any.
stCurrentAlbum :: SimpleGetter St (Maybe Album)
stCurrentAlbum = to $ \st -> do
  song <- st ^. stPlaying . psCurrentSong
  let key = songAlbumArtKey song
  listToMaybe
    [ album
    | album <- Vec.toList (st ^. stConfig . csAllAlbums)
    , albumArtKey album == key
    ]

-- | The rendered art for the currently playing album, if cached.
stCurrentAlbumArt :: SimpleGetter St (Maybe AlbumArt)
stCurrentAlbumArt = to $ \st ->
  st ^. stCurrentAlbum >>= \album ->
    (st ^. stPicCache) Map.!? albumArtKey album

-- | The raw metadata of the current song, preserving missingness.
stCurrentSongMeta' :: MPD.Metadata -> SimpleGetter St (Maybe (NonEmpty MPD.Value))
stCurrentSongMeta' meta = stPlaying . psCurrentSong . to f
 where
  f (Just s) = fromList <$> MPD.sgTags s Map.!? meta
  f Nothing = Nothing

-- | The position of the current song inside the active queue.
stCurrentSongPos :: SimpleGetter St (Maybe MPD.Position)
stCurrentSongPos = stPlaying . psCurrentSong . to (>>= MPD.sgIndex)

-- | The human-readable metadata of the current song.
stCurrentSongMeta :: MPD.Metadata -> SimpleGetter St (NonEmpty String)
stCurrentSongMeta meta = stPlaying . psCurrentSong . to (fromList . f)
 where
  f (Just s) = fromMaybe [unknown meta] (MPD.sgTags s Map.!? meta <&> fmap MPD.toString)
  f Nothing = [unknown meta]
  unknown MPD.Artist = "Unknown Artist"
  unknown MPD.Album = "Unknown Album"
  unknown MPD.Title = "Unknown Title"
  unknown _ = "Unknown"

-- | The human-readable metadata of the selected song.
stSelectedSongMeta :: MPD.Metadata -> SimpleGetter St (NonEmpty String)
stSelectedSongMeta meta = stSelectedSong . to (fromList . f)
 where
  f (Just s) = fromMaybe [unknown meta] (MPD.sgTags s Map.!? meta <&> fmap MPD.toString)
  f Nothing = [unknown meta]
  unknown MPD.Artist = "Unknown Artist"
  unknown MPD.Album = "Unknown Album"
  unknown MPD.Title = "Unknown Title"
  unknown _ = "Unknown"

stSelectedAlbumSongs :: SimpleGetter St (Vec.Vector MPD.Song)
stSelectedAlbumSongs = to $ \st ->
  maybe Vec.empty albumSongs $
    (st ^. stSelectedAlbum) >>= ((st ^. stConfig . csAllAlbums) Vec.!?)

stIsTriggered :: MName St -> SimpleGetter St Bool
stIsTriggered name = to $ \st -> Set.member name (st ^. stTriggeredNames)

-- | The time shown in the UI, taking drag previews into account.
stShownCurrentTime :: SimpleGetter St (Maybe (Double, Double))
stShownCurrentTime =
  to $ \st ->
    case st ^. stSongProgressPreview of
      Just previewTime -> Just previewTime
      Nothing -> st ^. stPlaying . psCurrentTime

-- | The current EQ config value.
stCurrentEQ :: SimpleGetter St EQConfigValue
stCurrentEQ = to $ \st ->
  -- SAFETY: It is guaranteed at the config loading stage
  (st ^. stConfig . csEQConfigs) Map.! (st ^. stConfig . csConfigs . cvEq)

-- | The current EQ config index.
stCurrentEQIndex :: SimpleGetter St (Maybe Int)
stCurrentEQIndex = to $ \st ->
  -- SAFETY: It is guaranteed at the config loading stage
  Map.lookupIndex (st ^. stConfig . csConfigs . cvEq) (st ^. stConfig . csEQConfigs)

stLayoutElement :: [Int] -> SimpleGetter St (Maybe LayoutElement)
stLayoutElement path =
  to $ \st -> lookupElement path (st ^. stConfig . csConfigs . cvLayout)

-- | Lookup an element in the layout tree.
lookupElement :: [Int] -> LayoutElement -> Maybe LayoutElement
lookupElement [] a = Just a
lookupElement (i : is) (EHBox _ es) = es !? i >>= lookupElement is
lookupElement (i : is) (EVBox _ es) = es !? i >>= lookupElement is
lookupElement (i : is) (ETabs es) = es !? i >>= lookupElement is
lookupElement _ _ = Nothing

-- | Lens helper for optional nested fields.
(.?) :: (Applicative f) => ((Maybe a1 -> f (Maybe a')) -> c) -> (a2 -> a1 -> f a') -> a2 -> c
a .? b = a . _Just . b

infixr 9 .?
