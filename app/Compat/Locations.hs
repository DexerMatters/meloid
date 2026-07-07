{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | This module provides some helper functions for
determining the locations of directories and files.
-}
module Compat.Locations (
  prepareAlbumArtCacheDir,
  prepareConfigDir,
  configFile,
) where

import Control.Exception
import Control.Monad (when)
import System.Directory
import Types.Helpers

{- | Prepare the album art cache directory.
The directory is responsible for storing album art so that
we can avoid extracting the same album art multiple times.
-}
prepareAlbumArtCacheDir :: IO FilePath
prepareAlbumArtCacheDir = do
  let fallbackDir = "/tmp/gaze-player/album-art"
  preferredDir <- getXdgDirectory XdgCache "gaze-player/album-art"
  (tryEnsure preferredDir :: IO (Either IOException ())) >>= \case
    Right () -> pure preferredDir
    Left _ -> do
      createDirectoryIfMissing True fallbackDir
      pure fallbackDir
 where
  tryEnsure = try . createDirectoryIfMissing True

{- | Prepare the configuration directory.
The directory is responsible for storing configuration files.
-}
prepareConfigDir :: IO FilePath
prepareConfigDir = do
  let fallbackDir = "/etc/gaze-player"
  preferredDir <- getXdgDirectory XdgConfig "gaze-player"
  (tryEnsure preferredDir :: IO (Either IOException ())) >>= \case
    Right () -> pure preferredDir
    Left _ -> do
      createDirectoryIfMissing True fallbackDir
      pure fallbackDir
 where
  tryEnsure = try . createDirectoryIfMissing True

-- | Prepare the configuration file.
configFile :: IO FilePath
configFile = do
  configDir <- prepareConfigDir
  let file = configDir <> "/gaze-player.yaml"
  exist <- doesFileExist file
  when (not exist) $ writeFile file defaultConfigStr
  pure file
