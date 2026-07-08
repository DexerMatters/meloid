{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{- | This module provides some helper functions for
determining the locations of directories and files.
-}
module Types.Configs (
  StoredConfigs (..),
  Configs (..),
  EQConfigs (..),
  albumArtCacheDir,
  configDir,
) where

import Control.Exception
import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Language.Haskell.TH.Syntax (addDependentFile, lift, runIO)
import System.Directory
import Types.Schemas
import Types.Schemas qualified as S

{- | A class for storing configuration files.
This class is responsible for determining the location,
reading, and saving configuration files.
-}
class
  (S.ToString (Repr a), S.FromString (Repr a)) =>
  StoredConfigs a
  where
  type Repr a
  path :: a -> ExceptT String IO FilePath
  read :: a -> ExceptT String IO (Repr a)
  read selector = do
    file <- path selector
    content <- liftIO $ readFile file
    liftEither $ S.fromString content
  save :: a -> Repr a -> ExceptT String IO ()
  save selector value = do
    file <- path selector
    liftIO $ writeFile file (S.toString value)

data Configs = Configs

data EQConfigs = EQConfigs String

{- | Prepare the configuration directory.
The directory is responsible for storing configuration files.
-}
configDir :: IO FilePath
configDir = do
  let fallbackDir = "/etc/meloid"
  preferredDir <- getXdgDirectory XdgConfig "meloid"
  (tryEnsure preferredDir :: IO (Either IOException ())) >>= \case
    Right () -> pure preferredDir
    Left _ -> do
      createDirectoryIfMissing True fallbackDir
      pure fallbackDir
 where
  tryEnsure = try . createDirectoryIfMissing True

instance StoredConfigs Configs where
  type Repr Configs = ConfigValue

  path _ = liftIO $ do
    dir <- configDir
    let file = dir <> "/config.yaml"
    exist <- doesFileExist file
    when (not exist) $
      writeFile file $
        $( do
             let fp = "assets/default-config.yaml"
             addDependentFile fp
             content <- runIO (readFile fp)
             lift content
         )
    pure file

instance StoredConfigs EQConfigs where
  type Repr EQConfigs = EQConfigValue

  path (EQConfigs eqId) = ExceptT $ do
    dir <- configDir
    let eqDir = dir <> "/eq"
    createDirectoryIfMissing True eqDir

    -- Check if the default EQ file exists
    let defaultFile = eqDir <> "/default.txt"
    defaultExists <- doesFileExist defaultFile
    when (not defaultExists) $
      writeFile defaultFile $
        $( do
             let fp = "assets/default-eq.txt"
             addDependentFile fp
             content <- runIO (readFile fp)
             lift content
         )

    -- Get the target EQ file
    let file = eqDir <> "/" <> eqId <> ".txt"
    doesFileExist file >>= \case
      True -> pure $ Right file
      False -> pure $ Left ("EQ file not found: " <> file)

{- | Prepare the album art cache directory.
The directory is responsible for storing album art so that
we can avoid extracting the same album art multiple times.
-}
albumArtCacheDir :: IO FilePath
albumArtCacheDir = do
  let fallbackDir = "/tmp/meloid/album-art"
  preferredDir <- getXdgDirectory XdgCache "meloid/album-art"
  (tryEnsure preferredDir :: IO (Either IOException ())) >>= \case
    Right () -> pure preferredDir
    Left _ -> do
      createDirectoryIfMissing True fallbackDir
      pure fallbackDir
 where
  tryEnsure = try . createDirectoryIfMissing True
