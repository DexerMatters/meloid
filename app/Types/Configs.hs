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
  imageCacheDir,
  configDir,
  saveWithPanic,
) where

import Brick qualified as B
import Control.Exception
import Control.Monad (filterM, forM, forM_, when)
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as JSON
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.List (dropWhileEnd, isSuffixOf, sort)
import Data.Map qualified as Map
import Language.Haskell.TH.Syntax (addDependentFile, lift, runIO)
import Paths_meloid qualified as Paths
import System.Directory
import System.Environment (lookupEnv)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.FilePath (dropExtension, isRelative, takeDirectory, takeFileName, (</>))
import System.Process (readProcessWithExitCode)
import Types (MName, logReqError)
import Types.Model (St)
import Types.Schemas
import Types.Schemas qualified as S

{- | A class for storing configuration files.
This class is responsible for determining the location,
reading, and saving configuration files.
-}
class StoredConfigs a where
  type Repr a
  path :: a -> ExceptT String IO FilePath
  read :: a -> ExceptT String IO (Repr a)
  save :: a -> Repr a -> ExceptT String IO ()

data Configs = Configs

data EQConfigs = EQConfigs

saveWithPanic :: (StoredConfigs a) => a -> Repr a -> B.EventM (MName St) St Bool
saveWithPanic selector value =
  (liftIO . runExceptT $ save selector value) >>= \case
    Right () -> pure True
    Left err -> logReqError "save" err >> pure False

{- | Prepare the configuration directory.
The directory is responsible for storing configuration files.
-}
configDir :: IO FilePath
configDir = do
  let fallbackDir = "/etc" </> "meloid"
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
    let file = dir </> "config.yaml"
    exist <- doesFileExist file
    when (not exist) $
      writeFile file $
        $( do
             let fp = "assets" </> "default-config.yaml"
             addDependentFile fp
             content <- runIO (readFile fp)
             lift content
         )
    pure file

  read selector = do
    file <- path selector
    content <- liftIO $ readFile file
    liftEither $ S.fromString content

  save _ value = do
    file <- path Configs
    helper <- liftIO $ Paths.getDataFileName ("tools" </> "update_config_yaml.mjs")
    nodeExe <- liftIO $ maybe "node" id <$> lookupEnv "MELOID_NODE"
    let payload = BL8.unpack (JSON.encode value)
    result <-
      liftIO $
        try @IOException $
          readProcessWithExitCode nodeExe [helper, file] payload
    case result of
      Left err ->
        throwError $
          "Failed to run config.yaml updater: " <> displayException err
      Right (ExitSuccess, _, _) ->
        pure ()
      Right (ExitFailure code, _, stderr) ->
        throwError $
          "Failed to update config.yaml: "
            <> formatHelperError code stderr

instance StoredConfigs EQConfigs where
  type Repr EQConfigs = EQConfigValue

  path _ = ExceptT $ do
    dir <- configDir
    let eqDir = dir </> "eq"
    createDirectoryIfMissing True eqDir

    -- Check if the default EQ file exists
    let defaultFile = eqDir </> "default.txt"
    defaultExists <- doesFileExist defaultFile
    when (not defaultExists) $
      writeFile defaultFile $
        $( do
             let fp = "assets" </> "default-eq.txt"
             addDependentFile fp
             content <- runIO (readFile fp)
             lift content
         )

    pure $ Right defaultFile

  read _ = do
    defaultFile <- path EQConfigs
    let eqDir = takeDirectory defaultFile
    entries <- liftIO $ sort <$> listDirectory eqDir
    files <- liftIO $ filterM (doesFileExist . (eqDir </>)) entries
    EQConfigValue . Map.fromList
      <$> forM
        [ file | file <- files, ".txt" `isSuffixOf` file ]
        ( \file -> do
            content <- liftIO $ readFile (eqDir </> file)
            config <- liftEither $ S.fromString content
            pure (dropExtension file, config)
        )

  save _ (EQConfigValue configs) = do
    defaultFile <- path EQConfigs
    files <-
      forM (Map.toList configs) $ \(eqId, config) ->
        case eqConfigFileName eqId of
          Nothing -> throwError $ "Invalid EQ config ID: " <> show eqId
          Just name -> pure (takeDirectory defaultFile </> name, config)
    liftIO $ forM_ files $ \(file, config) -> writeFile file (S.toString config)

-- | Prepare the image cache directory used by every image source.
imageCacheDir :: IO FilePath
imageCacheDir = do
  temporaryDir <- getTemporaryDirectory
  let fallbackDir = temporaryDir </> "meloid" </> "images"
  cacheRoot <- getXdgDirectory XdgCache "meloid"
  let preferredDir = cacheRoot </> "images"
  (tryEnsure preferredDir :: IO (Either IOException ())) >>= \case
    Right () -> pure preferredDir
    Left _ -> do
      createDirectoryIfMissing True fallbackDir
      pure fallbackDir
 where
  tryEnsure = try . createDirectoryIfMissing True

{- | EQ IDs are filenames, never paths. This prevents a config value such as
"../other" from escaping the application's EQ configuration directory.
-}
eqConfigFileName :: String -> Maybe FilePath
eqConfigFileName eqId
  | null eqId = Nothing
  | eqId == "." || eqId == ".." = Nothing
  | not (isRelative eqId) = Nothing
  | eqId /= takeFileName eqId = Nothing
  | otherwise = Just (eqId <> ".txt")

formatHelperError :: Int -> String -> String
formatHelperError code stderr =
  case dropWhileEnd (`elem` [' ', '\n', '\r', '\t']) stderr of
    "" -> "config.yaml updater exited with code " <> show code
    msg -> msg
