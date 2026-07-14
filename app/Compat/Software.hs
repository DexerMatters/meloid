{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | This module provides various support functions for
compatibility with different software such like PulseAudio.
-}
module Compat.Software (
  AudioServer (..),
  updateModuleEQId,
  restartMPDServer,
  restartAudioServer,
  extractExtraInfo,
  getMPDProcessId,
  getMPDSocket,
) where

import Brick qualified as B
import Control.Exception
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Aeson qualified as JSON
import Data.Aeson.KeyMap qualified as JSON
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.List (uncons)
import Data.Maybe (catMaybes)
import Data.Scientific qualified as Sci
import Data.Text qualified as Txt
import Data.Vector qualified as Vec
import GHC.IO.Exception (ExitCode (..))
import Language.Haskell.TH.Syntax
import Lens.Micro.Mtl
import Network.MPD qualified as MPD
import System.Directory
import System.FilePath (isAbsolute, isRelative, makeRelative, splitDirectories, (</>))
import System.Process qualified as Sys
import Text.Printf (printf)
import Text.Read (readEither, readMaybe)
import Types
import Utils

-- | A data type to represent different audio servers.
data AudioServer
  = PipeWire -- For now, we only support PulseAudio
  deriving (Eq)

data SocketType = IPv4 | IPv6
  deriving (Eq, Show)

instance Read SocketType where
  readsPrec _ "IPv4" = [(IPv4, "")]
  readsPrec _ "IPv6" = [(IPv6, "")]
  readsPrec _ _ = []

instance Show AudioServer where
  show PipeWire = "pipewire"

readProcess :: FilePath -> [String] -> String -> ExceptT String IO String
readProcess cmd args input =
  do
    (code, stdout, stderr) <-
      ExceptT $
        tryJust @IOException (\err -> Just $ show err) $
          Sys.readProcessWithExitCode cmd args input
    when (code /= ExitSuccess) $ throwE $ printf "%s failed with exit code %s: %s" cmd (show code) stderr
    pure stdout

-- | Get the MPD process ID
getMPDProcessId :: ExceptT String IO Int
getMPDProcessId = do
  res <- readProcess "systemctl" ["--user", "show", "mpd.service", "-p", "MainPID", "--value"] ""
  when (null res) $ throwE "Failed to get MPD process ID"
  ExceptT $ pure $ readEither res

-- | Get the MPD socket
getMPDSocket :: ExceptT String IO (SocketType, String, String)
getMPDSocket = do
  id' <- getMPDProcessId
  res <-
    catMaybes
      . fmap uncons
      . lines
      <$> readProcess "lsof" ["-Pan", "-a", "-p", show id', "-i", "-Ftn"] ""
  socket <- maybe (throwE "Failed to get MPD socket") pure $ lookup 'n' res
  ipType <- maybe (throwE "Failed to get MPD IP type") pure $ lookup 't' res
  let (ip, port) = break (== ':') socket
  pure (read ipType, ip, drop 1 port)

pipewireModuleTemplate :: String
pipewireModuleTemplate =
  $( do
       let fp = "assets" </> "pipewire" </> "meloid-eq.conf"
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
  let dir = configDir' </> "pipewire.conf.d"
  createDirectoryIfMissing True dir

  let str' = replace "%eqId%" eqId pipewireModuleTemplate
      str = replace "$HOME" homeDir str'
  writeFile (dir </> "meloid-eq.conf") str

-- | Restart the audio server.
restartAudioServer :: AudioServer -> ExceptT String IO ()
restartAudioServer PipeWire =
  void $
    readProcess "systemctl" ["--user", "restart", "pipewire", "pipewire-pulse", "wireplumber"] ""

-- | Restart the MPD server
restartMPDServer :: ExceptT String IO ()
restartMPDServer =
  -- run `systemctl --user restart mpd`
  void $
    readProcess "systemctl" ["--user", "restart", "mpd"] ""

-- | Extract extra information from a song
extractExtraInfo :: MPD.Song -> B.EventM (MName St) St (Either String SongFileExtraInfo)
extractExtraInfo MPD.Song{MPD.sgFilePath = path} = do
  musicDir <- use $ stConfig . csMusicDir
  liftIO $ runExceptT $ do
    path' <- resolveMusicFile musicDir (MPD.toString path)
    fileSize <- liftIO $ getFileSize path'
    stdout <-
      readProcess
        "ffprobe"
        [ "-v"
        , "error"
        , "-select_streams"
        , "a:0"
        , "-show_entries"
        , "stream=sample_rate,channels,bit_rate:format=bit_rate"
        , "-of"
        , "json"
        , path'
        ]
        ""
    decoded <-
      maybe
        (throwE "Failed to decode ffprobe output")
        pure
        (JSON.decode (BL8.pack stdout) :: Maybe JSON.Value)
    maybe
      (throwE "Bad ffprobe output")
      pure
      (parseSongFileExtraInfo fileSize decoded)

{- | Resolve an MPD-relative song path without allowing it to escape the
configured music directory, including through symlinks.
-}
resolveMusicFile :: FilePath -> FilePath -> ExceptT String IO FilePath
resolveMusicFile musicDir songPath
  | null musicDir = throwE "MPD music_directory is unavailable"
  | isAbsolute songPath = throwE "MPD returned an absolute song path"
  | otherwise = do
      root <- canonicalize musicDir
      candidate <- canonicalize (root </> songPath)
      unless (isDescendantOf root candidate) $
        throwE "MPD song path is outside music_directory"
      pure candidate
 where
  canonicalize path =
    ExceptT $
      tryJust @IOException (Just . displayException) (canonicalizePath path)

isDescendantOf :: FilePath -> FilePath -> Bool
isDescendantOf root path =
  let relative = makeRelative root path
   in relative /= "."
        && isRelative relative
        && ".." `notElem` splitDirectories relative

parseSongFileExtraInfo :: Integer -> JSON.Value -> Maybe SongFileExtraInfo
parseSongFileExtraInfo fileSize = \case
  JSON.Object root -> do
    JSON.Array streams <- JSON.lookup "streams" root
    (JSON.Object stream, _) <- Vec.uncons streams
    JSON.Object format <- JSON.lookup "format" root
    JSON.String sampleRate <- JSON.lookup "sample_rate" stream
    JSON.Number channels <- JSON.lookup "channels" stream
    JSON.String bitRate <- JSON.lookup "bit_rate" format
    sampleRate' <- readMaybe (Txt.unpack sampleRate)
    bitRate' <- readMaybe (Txt.unpack bitRate)
    pure $
      SongFileExtraInfo
        { songSize = formatBytes fileSize
        , songSampleRate = formatSampleRate sampleRate'
        , songChannels = case (Sci.floatingOrInteger channels :: Either Double Integer) of
            Left float -> show float
            Right int -> show int
        , songBitRate = formatBitrate bitRate'
        }
  _ ->
    Nothing
