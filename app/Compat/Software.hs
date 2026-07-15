{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | This module provides various support functions for
compatibility with different software such like PulseAudio.
-}
module Compat.Software (
  startEQBridge,
  stopEQBridge,
  applyEQ,
  routeMPDToEQ,
  spectrumUpdatingThread,
  extractExtraInfo,
  getMPDEndpoint,
) where

import Brick qualified as B
import Brick.BChan (BChan, writeBChan)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (TVar, atomically, readTVar)
import Control.Exception
import Control.Monad (forM_, forever, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Aeson qualified as JSON
import Data.Aeson.KeyMap qualified as JSON
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.List (isPrefixOf, sortOn, stripPrefix)
import Data.Maybe (listToMaybe)
import Data.Scientific qualified as Sci
import Data.Text qualified as Txt
import Data.Vector qualified as Vec
import Foreign.PipeWire qualified as PipeWire
import GHC.IO.Exception (ExitCode (..))
import Lens.Micro.Mtl
import Network.MPD qualified as MPD
import System.Directory
import System.Environment (lookupEnv)
import System.FilePath (isAbsolute, isRelative, makeRelative, normalise, splitDirectories, (</>))
import System.Process qualified as Sys
import Text.Printf (printf)
import Text.Read (readMaybe)
import Types
import Utils

-- | Publish the bridge's post-EQ spectrum only while the UI needs it.
spectrumUpdatingThread :: BChan Event -> TVar Bool -> IO ()
spectrumUpdatingThread evChan enabled = forever $ do
  active <- atomically $ readTVar enabled
  if not active
    then threadDelay 100000
    else PipeWire.readSpectrum >>= mapM_ (writeBChan evChan . UpdateSpectrum)
  threadDelay 50000

readProcess :: FilePath -> [String] -> String -> ExceptT String IO String
readProcess cmd args input =
  do
    (code, stdout, stderr) <-
      ExceptT $
        tryJust @IOException (\err -> Just $ show err) $
          Sys.readProcessWithExitCode cmd args input
    when (code /= ExitSuccess) $ throwE $ printf "%s failed with exit code %s: %s" cmd (show code) stderr
    pure stdout

-- | Resolve MPD's standard client endpoint without inspecting daemon sockets.
getMPDEndpoint :: IO (String, String)
getMPDEndpoint = do
  host <- endpointValue "MPD_HOST" "127.0.0.1"
  port <- endpointValue "MPD_PORT" "6600"
  pure (host, port)
 where
  endpointValue name fallback = do
    value <- lookupEnv name
    pure $ maybe fallback nonEmpty value
   where
    nonEmpty value
      | null value = fallback
      | otherwise = value

-- | Start the private PipeWire filter and load the saved EQ once.
startEQBridge :: EQConfigSpecs -> ExceptT String IO ()
startEQBridge config = ExceptT PipeWire.startEQ >> applyEQ config

-- | Restore MPD's original links before destroying the private filter.
stopEQBridge :: IO ()
stopEQBridge = void (runExceptT restoreMPDRoute) >> PipeWire.stopEQ

-- | Swap the bridge's biquad gains without rebuilding the PipeWire graph.
applyEQ :: EQConfigSpecs -> ExceptT String IO ()
applyEQ (EQConfigSpecs gains) = ExceptT $ PipeWire.setEQ gains

-- | Insert the private filter between MPD and its existing output ports.
routeMPDToEQ :: ExceptT String IO ()
routeMPDToEQ = do
  links <- pipeWireLinks <$> readProcess "pw-link" ["-l"] ""
  forM_ (filter isMPDLink links) $ \(source, target) -> do
    let channel = drop (length mpdOutputPrefix) source
        input = eqInputPrefix <> channel
        output = eqOutputPrefix <> channel
    void $ readProcess "pw-link" ["-d", source, target] ""
    void $ readProcess "pw-link" [source, input] ""
    void $ readProcess "pw-link" [output, target] ""
 where
  isMPDLink (source, target) =
    mpdOutputPrefix `isPrefixOf` source
      && not (eqInputPrefix `isPrefixOf` target)

restoreMPDRoute :: ExceptT String IO ()
restoreMPDRoute = do
  links <- pipeWireLinks <$> readProcess "pw-link" ["-l"] ""
  forM_ [link | link@(source, _) <- links, eqOutputPrefix `isPrefixOf` source] $ \(output, target) -> do
    let channel = drop (length eqOutputPrefix) output
        source = mpdOutputPrefix <> channel
        input = eqInputPrefix <> channel
    void $ readProcess "pw-link" ["-d", source, input] ""
    void $ readProcess "pw-link" ["-d", output, target] ""
    void $ readProcess "pw-link" [source, target] ""

pipeWireLinks :: String -> [(String, String)]
pipeWireLinks = go Nothing . lines
 where
  go _ [] = []
  go source (line : rest)
    | "  |-> " `isPrefixOf` line = maybe id (\output -> ((output, drop 6 line) :)) source $ go source rest
    | first : _ <- line, first /= ' ' = go (Just line) rest
    | otherwise = go source rest

mpdOutputPrefix, eqInputPrefix, eqOutputPrefix :: String
mpdOutputPrefix = "Music Player Daemon:output_"
eqInputPrefix = "meloid_eq_filter:input_"
eqOutputPrefix = "meloid_eq_filter:output_"

-- | Extract extra information from a song
extractExtraInfo :: MPD.Song -> B.EventM (MName St) St (Either String SongFileExtraInfo)
extractExtraInfo MPD.Song{MPD.sgFilePath = path} = do
  mounts <- use $ stConfig . csMusicMounts
  liftIO $ runExceptT $ do
    resolveMusicFile mounts (MPD.toString path) >>= \case
      Nothing -> pure defaultExtraInfo
      Just path' -> do
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

-- | Resolve only local MPD storage; streams and remote mounts stay MPD-only.
resolveMusicFile :: [(FilePath, FilePath)] -> FilePath -> ExceptT String IO (Maybe FilePath)
resolveMusicFile mounts songPath =
  case listToMaybe . reverse . sortOn (length . fst) $ candidates of
    Nothing -> pure Nothing
    Just (_, (root, relative))
      | not (isAbsolute root) -> pure Nothing
      | otherwise -> do
          root' <- canonicalize root
          let requested = normalise (root' </> relative)
          unless (isDescendantOf root' requested) $
            throwE "MPD song path is outside its storage mount"
          resolved <- canonicalize requested
          unless (isDescendantOf root' resolved) $
            throwE "MPD song resolves outside its storage mount"
          pure $ Just resolved
 where
  candidates = concatMap candidate mounts
  candidate (mount, root) =
    case mount of
      "" -> [(mount, (root, songPath))]
      _ -> maybe [] (\relative -> [(mount, (root, relative))]) $ stripPrefix (mount <> "/") songPath

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
