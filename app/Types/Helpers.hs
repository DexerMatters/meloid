{- | Pure derived helpers for albums, songs, and current playback.
These functions are intentionally kept state-only so they remain
cheap to reuse from multiple modules.
-}
module Types.Helpers (
  defaultAlbum,
  defaultExtraInfo,
  songMeta,
  songTrack,
  sortSongsByTrack,
  stCurrentSongMeta',
  stCurrentSongMeta,
  stSelectedSongMeta,
  stCurrentSongPos,
  stSelectedTrackSongs,
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
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Vector qualified as Vec
import Lens.Micro (to, (<&>), (^.), _Just)
import Lens.Micro.Type (SimpleGetter)
import Network.MPD qualified as MPD
import Text.Read (readMaybe)
import Types.Identity (MName)
import Types.Model
import Types.Schemas
import Utils (formatSecs)

-- | A blank album record used as a harmless fallback.
defaultAlbum :: Album
defaultAlbum =
  Album "" [] Vec.empty "" ""

defaultExtraInfo :: SongFileExtraInfo
defaultExtraInfo = SongFileExtraInfo "Unknown" "Unknown" "Unknown" "Unknown"

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
  f (Just (s, _)) = fromMaybe [unknown meta] (MPD.sgTags s Map.!? meta <&> fmap MPD.toString)
  f Nothing = [unknown meta]
  unknown MPD.Artist = "Unknown Artist"
  unknown MPD.Album = "Unknown Album"
  unknown MPD.Title = "Unknown Title"
  unknown _ = "Unknown"

-- | The songs shown by the shared Tracks panel. Album and playlist selection
-- are mutually exclusive, so the selected playlist takes precedence here.
stSelectedTrackSongs :: SimpleGetter St (Vec.Vector MPD.Song)
stSelectedTrackSongs = to $ \st ->
  case st ^. stSelectedPlaylist of
    Just selected ->
      maybe Vec.empty playlistSongs $
        Vec.find ((== selected) . playlistName) (st ^. stConfig . csAllPlaylists)
    Nothing ->
      maybe Vec.empty albumSongs $
        (st ^. stSelectedAlbum) >>= ((st ^. stConfig . csAllAlbums) Vec.!?)

stIsTriggered :: MName St -> SimpleGetter St Bool
stIsTriggered name = to $ \st -> Set.member name (st ^. stTriggerItem)

-- | The time shown in the UI, taking drag previews into account.
stShownCurrentTime :: SimpleGetter St (Maybe (Double, Double))
stShownCurrentTime =
  to $ \st ->
    case st ^. stSongProgressPreview of
      Just previewTime -> Just previewTime
      Nothing -> st ^. stPlaying . psCurrentTime

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
