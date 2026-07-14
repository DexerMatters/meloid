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
import Data.Function (on)
import Data.List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Vector qualified as Vec
import Lens.Micro
import Network.MPD qualified as MPD
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Types hiding (panic)
import Types.Configs qualified as Stored
import Prelude hiding (log)

withMPD :: String -> String -> MPD.MPD a -> IO (MPD.Response a)
withMPD "*" port = MPD.withMPD_ Nothing (Just port)
withMPD ip port = MPD.withMPD_ (Just ip) (Just port)

songProgressInterval :: Int
songProgressInterval = 200000

-- | Library grouping is a metadata concern, independent of image caching.
albumGroupKey :: MPD.Song -> (String, String)
albumGroupKey song =
  ( NonEmpty.head $ songMeta MPD.Artist song
  , NonEmpty.head $ songMeta MPD.Album song
  )

{- | The loop that updates the song progress every
`songProgressInterval`
-}
songProgressLoopThread :: String -> String -> BChan Event -> IO (MPD.Response ())
songProgressLoopThread ip port evChan = withMPD ip port $
  forever $ do
    status <- MPD.status
    liftIO $ do
      writeBChan evChan $ UpdateTime (MPD.stTime status)
      threadDelay songProgressInterval

{- | The loop that updates the current song using `idle`
command
-}
songChangeLoopThread :: String -> String -> BChan Event -> IO (MPD.Response ())
songChangeLoopThread ip port evChan = withMPD ip port $ forever $ do
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
  -- Get network status
  (socket, ip, port) <- runExceptT getMPDSocket >>= either panic pure
  log $
    "Detected MPD running on socket: "
      <> unlines
        [ "\n Type: " <> show socket
        , " IP: " <> ip
        , " Port: " <> port
        ]

  res0 <- trace $ withMPD ip port MPD.status
  case res0 of
    Left _ ->
      panic $
        unlines
          [ "MPD is not available."
          , "Do you have MPD installed and running?"
          , "You can follow the instructions at https://mpd.readthedocs.io/en/stable/user.html to install it."
          ]
    Right MPD.Status{stError = Just err} -> do
      log "MPD is available."
      logEv evChan Warn "MPD" $
        unlines
          [ "MPD reported an output error:"
          , err
          , "The app will continue, but playback may not work until the audio output is fixed."
          ]
    Right _ ->
      log "MPD is available."

  _ <- withMPD ip port $ MPD.rescan Nothing

  -- Initialize stored configs
  configs <- runExceptT (Stored.read Stored.Configs) >>= either panic pure
  eqConfigs <- runExceptT loadEQConfigs >>= either panic pure
  musicDir <-
    runExceptT getMPDMusicDirectory >>= \case
      Left err -> do
        logEv evChan Warn "MPD" $
          unlines
            [ "Failed to read MPD music_directory from local config files."
            , err
            , "Song file inspection will stay unavailable until a music directory is resolved."
            ]
        pure Nothing
      Right dir ->
        pure dir
  log $ "Config is loaded successfully: " <> toString configs
  log $ "EQ configs are loaded successfully: " <> show eqConfigs
  forM_ musicDir $ \dir ->
    log $ "Detected MPD music directory: " <> dir

  -- Update modules
  -- Currently, Pipewire is the only supported audio server
  updateModuleEQId PipeWire (configs ^. cvEq)

  -- Restart audio server
  -- Currently, Pipewire is the only supported audio server
  -- TODO: Restarting is still unstable
  -- runExceptT (restartAudioServer PipeWire) >>= either panic pure
  -- log $ "Audio server is restarted successfully: " <> show PipeWire

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
          ( songChangeLoopThread ip port evChan >>= \case
              Right _ -> pure ()
              Left err -> panic $ "Error while starting song change loop: \n" <> show err
          )
          >> forkIO
            ( songProgressLoopThread ip port evChan >>= \case
                Right _ -> pure ()
                Left err -> panic $ "Error while starting song progress loop: \n" <> show err
            )
          >> pure Nothing
      SignalQuit -> do
        res <- Just <$> withMPD ip port MPD.stop
        postEvent Halt
        pure res
      SignalCurrentQueue -> do
        snapshot <- withMPD ip port $ do
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
        Just . void <$> withMPD ip port (sequence op)
      UpdateEQId eqId -> do
        updateModuleEQId PipeWire eqId
        runExceptT
          (restartAudioServer PipeWire)
          >>= either panic pure
        pure Nothing
      -- This is matched when the app starts.
      -- It loads everything that is needed for the UI.
      GetConfig -> do
        result <- runExceptT $ do
          vol <- ExceptT $ withMPD ip port $ MPD.status <&> MPD.stVolume
          all' <- ExceptT $ withMPD ip port $ MPD.listAllInfo ""
          let songs = [song | MPD.LsSong song <- all']
              plNames = [playlist | MPD.LsPlaylist playlist <- all']
              dirs = [dir' | MPD.LsDirectory dir' <- all']
              albums' = groupBy ((==) `on` albumGroupKey) $ sortOn albumGroupKey songs
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
          plSongs <- mapM (ExceptT . withMPD ip port . MPD.listPlaylistInfo) plNames
          let playlists = zipWith Playlist plNames plSongs
          liftIO $
            postEvent $
              UpdateConfig $
                ConfigSt
                  { _csVolume = fromMaybe 0 vol
                  , _csMusicDir = fromMaybe "" musicDir
                  , _csAllPlaylists = Vec.fromList playlists
                  , _csAllDirs = Vec.fromList (fmap MPD.toString dirs)
                  , _csAllAlbums = Vec.fromList albums
                  , _csConfigs = configs
                  , _csEQConfigs = eqConfigs
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

  loadEQConfigs = do
    _ <- Stored.path (Stored.EQConfigs "default")
    eqDir <- liftIO $ (</> "eq") <$> Stored.configDir
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
