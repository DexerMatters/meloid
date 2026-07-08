{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

{- | This module is the MPD backend of the application.
It handles every request sent from Brick's main thread
to archieve the communication between the application and
the MPD server.
-}
module Sys (musicPlayerThread) where

import Brick.BChan
import Compat.Software
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (AsyncException (ThreadKilled), SomeException, throwIO, try)
import Control.Monad
import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.State (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.ByteString.UTF8 qualified as UTF8
import Data.Char (isSpace)
import Data.Function (on)
import Data.List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Vector qualified as Vec
import Lens.Micro
import Network.MPD qualified as MPD
import Network.MPD.Core qualified as Core
import System.Directory (listDirectory)
import Types hiding (panic)
import Types.Configs qualified as Stored
import Prelude hiding (log)

songProgressInterval :: Int
songProgressInterval = 200000

{- | The loop that updates the song progress every
`songProgressInterval`
-}
songProgressLoopThread :: BChan Event -> IO (MPD.Response ())
songProgressLoopThread evChan = MPD.withMPD $ forever $ do
  status <- MPD.status
  liftIO $ do
    writeBChan evChan $ UpdateTime (MPD.stTime status)
    threadDelay songProgressInterval

{- | The loop that updates the current song using `idle`
command
-}
songChangeLoopThread :: BChan Event -> IO (MPD.Response ())
songChangeLoopThread evChan = MPD.withMPD $ forever $ do
  _ <- MPD.idle [MPD.PlayerS]
  status <- MPD.status
  curSong <- MPD.currentSong
  liftIO $ do
    postEvent $ UpdateStatus status
    postEvent $ UpdateSong curSong
 where
  postEvent :: Event -> IO ()
  postEvent = writeBChan evChan

-- | The main loop of the MPD backend
musicPlayerThread :: BChan Request -> BChan Event -> IO ()
musicPlayerThread reqChan evChan = do
  res0 <- trace $ MPD.withMPD MPD.status

  case res0 of
    Left _ ->
      panic $
        unlines
          [ "MPD is not available."
          , "Do you have MPD installed and running?"
          , "You can follow the instructions at https://mpd.readthedocs.io/en/stable/user.html to install it."
          ]
    Right MPD.Status{stError = Just err} ->
      panic $ "MPD is not available. Error: \n" <> err
    Right _ ->
      log "MPD is available."

  _ <- MPD.withMPD $ MPD.rescan Nothing

  -- Initialize stored configs
  configs <- runExceptT (Stored.read Stored.Configs) >>= either panic pure
  eqConfigs <- runExceptT loadEQConfigs >>= either panic pure
  log $ "Config is loaded successfully: " <> show configs
  log $ "EQ configs are loaded successfully: " <> show eqConfigs

  -- Update modules
  -- Currently, Pipewire is the only supported audio server
  updateModuleEQId PipeWire (configs ^. cvEq)

  -- Restart audio server
  -- Currently, Pipewire is the only supported audio server
  runExceptT (restartAudioServer PipeWire) >>= either panic pure
  log $ "Audio server is restarted successfully: " <> show PipeWire

  forever $ do
    req <- readBChan reqChan

    -- `pure Nothing`: No exception, no result
    -- `pure . Just (Left err)`: A fatal error
    -- `pure . Just (Right res)`: A response (from MPD)
    res <- case req of
      LogConfig level msg ->
        logEv evChan level "Setup" msg >> pure Nothing
      SignalInit ->
        forkIO
          ( songChangeLoopThread evChan >>= \case
              Right _ -> pure ()
              Left err -> panic $ "Error while starting song change loop: \n" <> show err
          )
          >> forkIO
            ( songProgressLoopThread evChan >>= \case
                Right _ -> pure ()
                Left err -> panic $ "Error while starting song progress loop: \n" <> show err
            )
          >> pure Nothing
      SignalQuit -> do
        res <- Just <$> MPD.withMPD MPD.stop
        postEvent Halt
        pure res
      SignalCurrentQueue -> do
        snapshot <- MPD.withMPD $ do
          status <- MPD.status
          currentSong <- MPD.currentSong
          songs <- MPD.playlistInfo Nothing
          pure (status, currentSong, Vec.fromList songs)
        case snapshot of
          Left err -> pure $ Just $ Left err
          Right (status, currentSong, songs) -> do
            postEvent $ UpdateCurrentQueueState status currentSong songs
            pure Nothing
      MPDOperation op ->
        Just . void <$> MPD.withMPD (sequence op)
      -- This is matched when the app starts.
      -- It loads everything that is needed for the UI.
      GetConfig -> do
        let socket = "/run/user/1000/mpd/socket"
        result <- runExceptT $ do
          dir <- ExceptT $ MPD.withMPD_ (Just socket) Nothing getMusicDirectory
          vol <- ExceptT $ MPD.withMPD $ MPD.status <&> MPD.stVolume
          all' <- ExceptT $ MPD.withMPD $ MPD.listAllInfo ""
          let songs = [song | MPD.LsSong song <- all']
              plNames = [playlist | MPD.LsPlaylist playlist <- all']
              dirs = [dir' | MPD.LsDirectory dir' <- all']
              albums' = groupBy ((==) `on` songAlbumArtKey) $ sortOn songAlbumArtKey songs
              albums =
                albums' <&> \tracks -> case listToMaybe tracks of
                  Just cand ->
                    Album
                      { albumName = NonEmpty.head $ songMeta MPD.Album cand
                      , albumArtists = nub . concat $ NonEmpty.toList . songMeta MPD.Artist <$> tracks
                      , albumGenre = NonEmpty.head $ songMeta MPD.Genre cand
                      , albumReleaseDate = NonEmpty.head $ songMeta MPD.Date cand
                      , albumSongs = sortSongsByTrack tracks
                      }
                  Nothing ->
                    defaultAlbum
          plSongs <- mapM (ExceptT . MPD.withMPD . MPD.listPlaylistInfo) plNames
          let playlists = zipWith Playlist plNames plSongs
          liftIO $
            postEvent $
              UpdateConfig $
                ConfigSt
                  { _csVolume = fromMaybe 0 vol
                  , _csMusicDir = fromMaybe "" dir
                  , _csAllPlaylists = Vec.fromList playlists
                  , _csAllDirs = Vec.fromList (fmap MPD.toString dirs)
                  , _csAllAlbums = Vec.fromList albums
                  , _csConfigs = configs
                  , _csEQConfigs = eqConfigs
                  , _csCurrentEQ = configs ^. cvEq
                  }
        either (pure . Just . Left) (const $ pure Nothing) result

    case res of
      Just (Left x) ->
        panic $ "An error occurred with MPD:\n" <> show x
      _ ->
        pure ()
 where
  panic :: String -> IO a
  panic s = logEv evChan Error "MPD" s >> throwIO ThreadKilled

  log = logEv evChan Info "MPD"

  trace :: IO a -> IO a
  trace m =
    try @SomeException m >>= \case
      Left err ->
        panic $
          unlines
            [ "Some unexpected error occurred:"
            , show err
            , "Note: This is probably an error thrown by the internal MPD library."
            , "If you see this, please open an issue on GitHub."
            ]
      Right res -> pure res

  postEvent = writeBChan evChan

  getMusicDirectory = do
    lines_ <- Core.getResponse "config"
    pure $ lookupConfig "music_directory" (map UTF8.toString lines_)

  lookupConfig key lines_ =
    case find ((key ++ ":") `prefixOf`) lines_ of
      Nothing -> Nothing
      Just line ->
        let value =
              dropWhile isSpace $
                drop 1 $
                  dropWhile (/= ':') line
         in Just value

  prefixOf prefix s =
    take (length prefix) s == prefix

  loadEQConfigs = do
    _ <- Stored.path (Stored.EQConfigs "default")
    eqDir <- liftIO $ (<> "/eq") <$> Stored.configDir
    eqFiles <- liftIO $ listDirectory eqDir
    let eqIds =
          sort
            [ reverse (drop 4 (reverse file))
            | file <- eqFiles
            , ".txt" `isSuffixOf` file
            ]
    Map.fromList
      <$> forM
        eqIds
        ( \eqId -> do
            config <- Stored.read (Stored.EQConfigs eqId)
            pure (eqId, config)
        )
