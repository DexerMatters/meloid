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
) where

import Brick qualified as B
import Control.Exception
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Aeson qualified as JSON
import Data.Aeson.KeyMap qualified as JSON
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.List (isPrefixOf)
import Data.Scientific qualified as Sci
import Data.Text qualified as Txt
import Data.Vector qualified as Vec
import GHC.IO.Exception (ExitCode (..))
import Language.Haskell.TH.Syntax
import Lens.Micro
import Lens.Micro.Mtl
import Network.MPD qualified as MPD
import System.Directory
import System.Process (readProcess, readProcessWithExitCode)
import Text.Read (readMaybe)
import Types
import Utils

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

  let str' = replace "%eqId%" eqId pipewireModuleTemplate
      str = replace "$HOME" homeDir str'
  writeFile (dir <> "/meloid-eq.conf") str

-- | Restart the audio server.
restartAudioServer :: AudioServer -> ExceptT String IO ()
restartAudioServer PipeWire =
  -- run `systemctl --user restart pipewire pipewire-pulse wireplumber`
  ExceptT $
    tryJust @SomeException (\err -> Just $ "Failed to restart audio server: \n" <> show err) $
      void $
        readProcess "systemctl" ["--user", "restart", "pipewire", "pipewire-pulse", "wireplumber"] ""

-- | Restart the MPD server
restartMPDServer :: ExceptT String IO ()
restartMPDServer =
  -- run `systemctl --user restart mpd`
  ExceptT $
    tryJust @SomeException (\err -> Just $ "Failed to restart MPD server: \n" <> show err) $
      void $
        readProcess "systemctl" ["--user", "restart", "mpd"] ""

-- | Extract extra information from a song
extractExtraInfo :: MPD.Song -> B.EventM (MName St) St (Either String SongFileExtraInfo)
extractExtraInfo MPD.Song{MPD.sgFilePath = path} = do
  path' <- use $ stConfig . csMusicDir . to (<> "/" <> MPD.toString path)
  liftIO $ runExceptT $ do
    fileSize <- liftIO $ getFileSize path'
    (code, stdout, stderr) <-
      ExceptT $
        tryJust @IOException (\err -> Just $ "Failed to run ffprobe: \n" <> show err) $
          readProcessWithExitCode
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
    when
      (code /= ExitSuccess)
      (throwE $ "ffprobe failed with exit code " <> show code <> ": " <> stderr)
    decoded <-
      maybe
        (throwE "Failed to decode ffprobe output")
        pure
        (JSON.decode (BL8.pack stdout) :: Maybe JSON.Value)
    maybe
      (throwE "Bad ffprobe output")
      pure
      (parseSongFileExtraInfo fileSize decoded)

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

-- | Replace all occurrences of a substring in a list. Safe against empty search terms.
replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
replace [] _ xs = xs
replace old new xs = go xs
 where
  go [] = []
  go ys@(z : zs)
    | old `isPrefixOf` ys = new ++ go (drop (length old) ys)
    | otherwise = z : go zs
