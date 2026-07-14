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
  MPDConfigs (..),
  mpdMusicDirectory,
  imageCacheDir,
  configDir,
) where

import Control.Exception
import Control.Monad (filterM, when)
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as JSON
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Foldable (traverse_)
import Data.List (dropWhileEnd, sort)
import Data.Maybe (listToMaybe)
import Language.Haskell.TH.Syntax (addDependentFile, lift, runIO)
import Paths_meloid qualified as Paths
import System.Directory
import System.Environment (lookupEnv)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.FilePath (isAbsolute, isRelative, normalise, takeDirectory, takeFileName, (</>))
import System.Process (readProcessWithExitCode)
import Types.Schemas
import Types.Schemas qualified as S
import Utils (replace)

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

data MPDConfigs = MPDConfigs

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

  path (EQConfigs eqId) = ExceptT $ do
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

    case eqConfigFileName eqId of
      Nothing -> pure $ Left $ "Invalid EQ config ID: " <> show eqId
      Just name -> do
        let file = eqDir </> name
        doesFileExist file >>= \case
          True -> pure $ Right file
          False -> pure $ Left ("EQ file not found: " <> file)

instance StoredConfigs MPDConfigs where
  type Repr MPDConfigs = MPDConfigValue

  path _ = ExceptT $ do
    homeDir <- getHomeDirectory
    cfgDir <- getXdgDirectory XdgConfig "mpd"
    let candidates = [cfgDir </> "mpd.conf", homeDir </> ".mpdconf", "/etc/mpd.conf"]
    filterM doesFileExist candidates >>= \case
      file : _ -> pure $ Right file
      [] ->
        pure . Left $
          "MPD config file not found. Checked: " <> unwords candidates

  read _ = readMPDConfigs

  save _ (MPDConfigValue files) = traverse_ saveMPDConfigFile files

-- | Load the root MPD config and every config reached through `include`.
readMPDConfigs :: ExceptT String IO MPDConfigValue
readMPDConfigs = do
  homeDir <- liftIO getHomeDirectory
  root <- path MPDConfigs
  MPDConfigValue <$> collect homeDir [] [root]
 where
  collect _ _ [] = pure []
  collect homeDir seen (file : rest)
    | file `elem` seen = collect homeDir seen rest
    | otherwise = do
        config@(MPDConfigValue files) <- readMPDConfigFile file
        includes <-
          liftIO $
            concat
              <$> mapM
                (resolveInclude homeDir (takeDirectory file))
                (mpdGet ["include"] config)
        (files <>) <$> collect homeDir (file : seen) (includes <> rest)

-- | Resolve the first configured music directory across loaded MPD files.
mpdMusicDirectory :: MPDConfigValue -> IO (Maybe FilePath)
mpdMusicDirectory config = do
  homeDir <- getHomeDirectory
  pure . listToMaybe $
    [ expandMPDPath homeDir directory
    | directory <- mpdGet ["music_directory"] config
    ]

readMPDConfigFile :: FilePath -> ExceptT String IO MPDConfigValue
readMPDConfigFile file = do
  content <-
    ExceptT $
      tryJust @IOException (Just . displayException) (readFile file)
  liftEither $ parseMPDConfig file content

saveMPDConfigFile :: (FilePath, [MPDConfigLine]) -> ExceptT String IO ()
saveMPDConfigFile (file, lines') = do
  if null file
    then throwError "Cannot save an MPD config without a source path"
    else liftIO $ writeFile file (renderMPDConfig $ MPDConfigValue [(file, lines')])

resolveInclude :: FilePath -> FilePath -> String -> IO [FilePath]
resolveInclude homeDir baseDir rawPath = do
  let path' =
        normalise $
          case expandMPDPath homeDir rawPath of
            absolute | isAbsolute absolute -> absolute
            relative -> baseDir </> relative
  doesFileExist path' >>= \case
    True -> pure [path']
    False ->
      doesDirectoryExist path' >>= \case
        False -> pure []
        True -> (fmap (path' </>) . sort) <$> listDirectory path'

expandMPDPath :: FilePath -> FilePath -> FilePath
expandMPDPath homeDir path' =
  normalise $
    case replace "${HOME}" homeDir $ replace "$HOME" homeDir path' of
      "~" -> homeDir
      '~' : '/' : rest -> homeDir </> rest
      other -> other

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
