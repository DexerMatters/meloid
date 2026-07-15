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
import Control.Concurrent.STM (TVar, atomically, writeTVar)
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
    void $ runExceptT routeMPDToEQ
    postEvent $ UpdateStatus status
    postEvent $ UpdateSong curSong
 where
  postEvent :: Event -> IO ()
  postEvent = writeBChan evChan

-- | The main loop of the MPD backend
musicPlayerThread :: BChan Request -> BChan Event -> TVar Bool -> IO ()
musicPlayerThread reqChan evChan spectrumEnabled = do
  configs <- runExceptT (Stored.read Stored.Configs) >>= either panic pure
  eqConfigs <- runExceptT (Stored.read Stored.EQConfigs) >>= either panic pure
  log "Config is loaded successfully"
  log "EQ configs are loaded successfully"
  let EQConfigValue presets = eqConfigs
      initialEQ = Map.findWithDefault (EQConfigSpecs $ replicate (length eqFrequencies) 0) (configs ^. cvEq) presets
  runExceptT (startEQBridge initialEQ) >>= either (panic . ("Failed to start EQ bridge: " <>)) (const $ pure ())

  (ip, port) <- liftIO getMPDEndpoint
  mounts <-
    withMPD ip port MPD.listMounts >>= \case
      Right values -> pure values
      Left err -> warn ("Failed to query MPD mounts: " <> show err) >> pure []
  forM_ mounts $ \(mount, storage) ->
    log $ "Detected MPD mount " <> show mount <> ": " <> storage
  runExceptT routeMPDToEQ >>= either (warn . ("Failed to route MPD: " <>)) (const $ pure ())
  log $
    "Using MPD endpoint: "
      <> unlines
        [ "\n Host: " <> ip
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
        atomically $ writeTVar spectrumEnabled False
        liftIO stopEQBridge
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
      ApplyEQ config -> do
        runExceptT (applyEQ config) >>= either (log . ("Failed to apply EQ: " <>)) (const $ pure ())
        pure Nothing
      TriggerSpectrum enabled -> do
        atomically $ writeTVar spectrumEnabled enabled
        unless enabled $ postEvent $ UpdateSpectrum Vec.empty
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
                  , _csMusicMounts = mounts
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

  warn = logEv evChan Warn "MPD"

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
