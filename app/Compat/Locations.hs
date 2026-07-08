{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | This module provides some helper functions for
determining the locations of directories and files.
-}
module Compat.Locations (
  albumArtCacheDir,
  configDir,
  configFile,
  saveConfigValue,
  readConfigValue,
  defaultConfigValue,
) where

import Control.Exception
import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString qualified as BS
import Data.Yaml qualified as YAML
import Language.Haskell.TH.Syntax
import System.Directory
import Types.Schemas (ConfigValue)

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

-- | Prepare the configuration file.
configFile :: IO FilePath
configFile = do
  dir <- configDir
  let file = dir <> "/config.yaml"
  exist <- doesFileExist file
  when (not exist) $ writeFile file defaultConfigStr
  pure file

-- | The checked-in default config text, copied verbatim on first run.
defaultConfigStr :: String
defaultConfigStr =
  $( do
       let fp = "assets/default-config.yaml"
       addDependentFile fp
       content <- runIO (readFile fp)
       lift content
   )

-- | The checked-in default config, decoded at compile time.
defaultConfigValue :: ConfigValue
defaultConfigValue =
  $( do
       let fp = "assets/default-config.yaml"
       addDependentFile fp
       content <- runIO (BS.readFile fp)
       case (YAML.decodeEither' content :: Either YAML.ParseException ConfigValue) of
         Left err ->
           fail $
             "Failed to decode "
               <> fp
               <> " as ConfigValue:\n"
               <> YAML.prettyPrintParseException err
         Right value ->
           lift value
   )

-- | Save the config file.
saveConfigValue :: ConfigValue -> IO ()
saveConfigValue value = configFile >>= flip BS.writeFile (YAML.encode value)

-- | Read the configuration file.
readConfigValue :: ExceptT String IO ConfigValue
readConfigValue =
  liftIO configFile
    >>= withExceptT YAML.prettyPrintParseException . ExceptT . YAML.decodeFileEither