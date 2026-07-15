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
  LogLevel (..),
) where

import Data.Vector qualified as Vec
import Network.MPD qualified as MPD
import Types.Schemas.EQConfig (EQConfigSpecs)

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
  | UpdateSpectrum (Vec.Vector Double)
  | ImagesReady
  | UpdateConfig a
  | Halt

-- | Messages sent to the background worker and MPD layer.
data Request
  = MPDOperation [MPD.MPD ()]
  | SignalInit
  | SignalQuit
  | SignalCurrentQueue
  | LogConfig LogLevel String
  | ApplyEQ EQConfigSpecs
  | TriggerSpectrum Bool
  | GetConfig

-- | Log levels used by the in-app log view.
data LogLevel = Debug | Info | Warn | Error
  deriving (Show, Eq, Ord)
