{-# LANGUAGE TemplateHaskell #-}

{- | This module provides various support functions for
compatibility with different software such like PulseAudio.
-}
module Compat.Software (
  AudioServer (..),
  updateModuleEQId,
  restartAudioServer,
) where

import Control.Exception
import Control.Monad (void)
import Control.Monad.Except (ExceptT (ExceptT))
import Data.List.Utils qualified as List
import Language.Haskell.TH.Syntax
import System.Directory
import System.Process (readProcess)

-- | A data type to represent different audio servers.
data AudioServer
  = PipeWire -- For now, we only support PulseAudio
  deriving (Eq)

instance Show AudioServer where
  show PipeWire = "pipewire"

pipewireModuleTemplate :: String
pipewireModuleTemplate =
  $( do
       let fp = "assets/pipewire/meloid-eq.conf"
       addDependentFile fp
       content <- runIO (readFile fp)
       lift $ content
   )

{- | This function updates a module for the given audio server.
It creates the module directory if it does not exist.
It is currently only implemented for PipeWire.
-}
updateModuleEQId :: AudioServer -> String -> IO ()
updateModuleEQId PipeWire eqId = do
  homeDir <- getHomeDirectory
  configDir' <- getXdgDirectory XdgConfig "pipewire"
  let dir = configDir' <> "/pipewire.conf.d"
  createDirectoryIfMissing True dir

  let str' = List.replace "%eqId%" eqId pipewireModuleTemplate
      str = List.replace "$HOME" homeDir str'
  writeFile (dir <> "/meloid-eq.conf") str

restartAudioServer :: AudioServer -> ExceptT String IO ()
restartAudioServer PipeWire =
  -- run `systemctl --user restart pipewire pipewire-pulse wireplumber`
  ExceptT $
    tryJust @IOError (\err -> Just $ "Failed to restart audio server: \n" <> show err) $
      void $
        readProcess "systemctl" ["--user", "restart", "pipewire", "pipewire-pulse", "wireplumber"] ""