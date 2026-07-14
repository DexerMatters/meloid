{-# LANGUAGE TemplateHaskell #-}

{- | Shared state records for the application.
The records live here so the rest of the modules can
depend on a stable data layer without pulling in unrelated
helpers.
-}
module Types.Model (
  DialogSt (..),
  Album (..),
  Playlist (..),
  SongFileExtraInfo (..),
  ConfigSt (..),
  PlayingSt (..),
  EditSt' (..),
  Environment (..),
  MenuSt (..),
  MenuWidget (..),
  St (..),
  Event,
  EditSt,
  PaintedScene,
  -- St lenses
  stEdits,
  stPressed,
  stLastLeftClick,
  stSongProgressPreview,
  stLastRightPressed,
  stTriggeredNames,
  stTabStates,
  stCurrentView,
  stLastView,
  stDialog,
  stMenu,
  stMode,
  stDialogView,
  stSelectedAlbum,
  stSelectedPlaylist,
  stSelectedSong,
  stConfig,
  stPlaying,
  stLogs,
  stChannel,
  stImageCache,
  stLayoutResize,
  stPanic,
  stEnv,
  -- ConfigSt lenses
  csVolume,
  csMusicDir,
  csAllPlaylists,
  csAllDirs,
  csAllAlbums,
  csConfigs,
  csEQConfigs,
  -- PlayingSt lenses
  psCurrentSong,
  psCurrentTime,
  psCurrentQueue,
  psPaused,
  -- EditSt' lenses
  esCommand,
  -- Environment lenses
  envTermType,
  envImageFormat,
  -- Dialog lenses
  dsText,
  dsPage,
  -- MenuSt lenses
  msWidgets,
  msLocation,
) where

import Brick.BChan (BChan)
import Brick.Types (EventM, Extent)
import Brick.Widgets.Edit qualified as E
import Compat.Term (ImageFormat, TermType)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Time.Clock (UTCTime)
import Data.Vector qualified as Vec
import Lens.Micro.TH (makeLenses)
import Network.MPD qualified as MPD
import Types.Core
import Types.Identity (MName, ViewName)
import Types.Image
import Types.Schemas

-- | State for a simple text dialog.
data DialogSt = DialogSt
  { _dsPage :: Int
  , _dsText :: String
  }

makeLenses ''DialogSt

-- | An album aggregate used by the library browser.
data Album = Album
  { albumName :: String
  , albumArtists :: [String]
  , albumSongs :: Vec.Vector MPD.Song
  , albumGenre :: String
  , albumReleaseDate :: String
  }

-- | A playlist snapshot returned from MPD.
data Playlist = Playlist
  { playlistName :: MPD.PlaylistName
  , playlistSongs :: [MPD.Song]
  }

{- | Song file extra information.
This needs to maunally be extracted from the file with the
algorithm written by ourselves.
-}
data SongFileExtraInfo = SongFileExtraInfo
  { songSize :: String
  , songBitRate :: String
  , songSampleRate :: String
  , songChannels :: String
  }

-- | Static and semi-static configuration loaded from MPD.
data ConfigSt = ConfigSt
  { _csVolume :: MPD.Volume
  , _csMusicDir :: FilePath
  , _csAllPlaylists :: Vec.Vector Playlist
  , _csAllDirs :: Vec.Vector FilePath
  , _csAllAlbums :: Vec.Vector Album
  , -- Config loaded from /config.yaml
    _csConfigs :: ConfigValue
  , -- Eq config loaded from /eq/*
    _csEQConfigs :: Map.Map String EQConfigValue
  }

makeLenses ''ConfigSt

-- | The rolling playback state.
data PlayingSt = PlayingSt
  { _psCurrentSong :: Maybe MPD.Song
  , _psCurrentTime :: Maybe (Double, Double)
  , _psCurrentQueue :: Vec.Vector MPD.Song
  , _psPaused :: Bool
  }

makeLenses ''PlayingSt

-- | Editor state parameterized by the application state type.
data EditSt' st = EditSt
  { _esCommand :: E.Editor String (MName st)
  }

makeLenses ''EditSt'

-- | Results from terminal and image-format detection.
data Environment = Environment
  { _envTermType :: TermType
  , _envImageFormat :: ImageFormat
  }

makeLenses ''Environment

data MenuWidget
  = MWButton String (EventM (MName St) St ())
  | MWHeader String
  | MWSubmenu String [MenuWidget]

{- | A transient menu and the widget it is positioned relative to.
An empty widget list means that no menu is open.
-}
data MenuSt = MenuSt
  { _msWidgets :: [MenuWidget]
  , _msLocation :: MName St
  }

-- | The full application state.
data St
  = St
  { _stEdits :: EditSt' St
  , _stPressed :: Maybe (MName St)
  , _stLastLeftClick :: Maybe (MName St, UTCTime)
  , _stSongProgressPreview :: Maybe (Double, Double)
  , _stTriggeredNames :: Set.Set (MName St)
  , _stTabStates :: Map.Map [Int] Int
  , _stLastRightPressed :: Maybe (MName St)
  , _stCurrentView :: Maybe ViewName
  , _stLastView :: Maybe ViewName
  , _stDialog :: Maybe DialogSt
  , _stMenu :: MenuSt
  , _stMode :: Mode
  , _stDialogView :: Maybe ViewName
  , _stSelectedAlbum :: Maybe Int
  , _stSelectedPlaylist :: Int
  , _stSelectedSong :: Maybe (MPD.Song, SongFileExtraInfo)
  , _stConfig :: ConfigSt
  , _stPlaying :: PlayingSt
  , _stLogs :: [(LogLevel, String)]
  , _stChannel :: Maybe (BChan Request)
  , _stImageCache :: ImageCache
  , _stLayoutResize :: Maybe ([Int], Int)
  , _stPanic :: Bool
  , _stEnv :: Environment
  }

makeLenses ''MenuSt
makeLenses ''St

type Event = Event' ConfigSt

type EditSt = EditSt' St

-- | The currently painted out-of-band image scene.
type PaintedScene = Map.Map (MName St) (Extent (MName St), RenderedImage)
