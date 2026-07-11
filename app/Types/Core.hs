{-# LANGUAGE OverloadedStrings #-}

{- | This module contains core application payloads and request
types. These types are intentionally independent from the full
application state.
-}
module Types.Core (
  Mode (..),
  formatMode,
  Event' (..),
  Request (..),
  ImageRequest (..),
  ImageSize,
  AlbumArtKey,
  RenderedImage (..),
  AlbumArt,
  LogLevel (..),
) where

import Compat.Term (ImageFormat)
import Data.ByteString (ByteString)
import Data.Map qualified as Map
import Data.Vector qualified as Vec
import Network.MPD qualified as MPD

-- | The UI mode of the application.
data Mode = NormalMode | CommandMode | EditMode
  deriving (Show, Eq)

-- | Render a human-readable name for the current mode.
formatMode :: Mode -> String
formatMode NormalMode = "Normal"
formatMode CommandMode = "Command"
formatMode EditMode = "Edit"

data Event' a
  = Log (LogLevel, String)
  | RefreshImages
  | UpdateSong (Maybe MPD.Song)
  | UpdateStatus MPD.Status
  | UpdateTime (Maybe (Double, Double))
  | UpdateCurrentQueueState MPD.Status (Maybe MPD.Song) (Vec.Vector MPD.Song)
  | LoadAlbumArt (AlbumArtKey, AlbumArt)
  | UpdateConfig a
  | Halt

-- | Messages sent to the background worker and MPD layer.
data Request
  = MPDOperation [MPD.MPD ()]
  | SignalInit
  | SignalQuit
  | SignalCurrentQueue
  | LogConfig LogLevel String
  | UpdateEQId String
  | GetConfig

-- | Work items for image extraction and conversion.
data ImageRequest
  = RenderAlbumArt AlbumArtKey FilePath ImageFormat

type ImageSize = (Int, Int)

type AlbumArtKey = String

-- | The rendered image payload stored in the cache.
data RenderedImage
  = InlineSymbols String
  | TerminalGraphic ImageFormat ByteString
  deriving (Eq, Show)

type AlbumArt = Map.Map ImageSize RenderedImage

-- | Log levels used by the in-app log view.
data LogLevel = Debug | Info | Warn | Error
  deriving (Show, Eq, Ord)
