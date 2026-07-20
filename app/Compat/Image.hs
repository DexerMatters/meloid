{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

{- | Terminal image compatibility backend.
Widgets describe image sources and clipping surfaces with 'ImageScene'.  This
module owns every effectful concern: source caching, conversion, asynchronous
rendering, extent-aware culling, and out-of-band terminal output.
-}
module Compat.Image (
  ImageService,
  startImageService,
  wrapVty,
  refreshScene,
  clearScene,
  queueRefreshImages,
  takeReadyImages,
) where

import Brick
import Brick.BChan (BChan, writeBChan)
import Brick.Main qualified as M
import Compat.Software (getMPDEndpoint)
import Compat.Term
import Compat.Term qualified as Term
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM
import Control.Exception (IOException, try)
import Control.Monad (forever, replicateM_, unless, void, when)
import Control.Monad.Except
import Control.Monad.State (liftIO)
import Control.Monad.Trans.Except
import Data.Bits (xor)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.UTF8 qualified as UTF8
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set qualified as Set
import Data.Word (Word64)
import Graphics.Vty qualified as V
import Graphics.Vty.Output qualified as Output
import Lens.Micro ((^.))
import Lens.Micro.Mtl
import Numeric (showHex)
import Network.MPD qualified as MPD
import System.Directory (doesFileExist, getTemporaryDirectory, removeFile, renameFile)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hClose, hSetBinaryMode, openBinaryTempFile)
import System.Process
import Types
import Types.Configs (imageCacheDir)
import Utils

data ImageRenderQueue = ImageRenderQueue
  { imageRenderDesired :: TVar (Maybe (Term.ImageFormat, PaintedScene))
  , imageRenderPainted :: TVar (Maybe (Term.ImageFormat, PaintedScene))
  , imageRenderOutputLock :: MVar ()
  }

{- | Global state for source bytes, pending conversion work, and the terminal
scene.  It intentionally has no knowledge of albums or concrete widgets.
-}
data ImageService = ImageService
  { imageServiceRawCacheDir :: FilePath
  , imageServiceEvents :: BChan Event
  , imageServiceRequests :: TQueue ImageRequest
  , imageServicePendingRender :: TVar (Set.Set ImageCacheKey)
  , imageServiceFailedRender :: TVar (Set.Set ImageCacheKey)
  , imageServiceReadyImages :: TVar ImageCache
  , imageServiceImagesReadyQueued :: TVar Bool
  , imageServiceRefreshQueued :: TVar Bool
  , imageServiceRenderQueue :: ImageRenderQueue
  }

startImageService :: BChan Event -> IO ImageService
startImageService evChan = do
  rawCacheDir <- imageCacheDir
  requests <- newTQueueIO
  pendingRender <- newTVarIO Set.empty
  failedRender <- newTVarIO Set.empty
  readyImages <- newTVarIO Map.empty
  imagesReadyQueued <- newTVarIO False
  refreshQueued <- newTVarIO False
  renderQueue <- newImageRenderQueue
  let service =
        ImageService
          { imageServiceRawCacheDir = rawCacheDir
          , imageServiceEvents = evChan
          , imageServiceRequests = requests
          , imageServicePendingRender = pendingRender
          , imageServiceFailedRender = failedRender
          , imageServiceReadyImages = readyImages
          , imageServiceImagesReadyQueued = imagesReadyQueued
          , imageServiceRefreshQueued = refreshQueued
          , imageServiceRenderQueue = renderQueue
          }
  replicateM_ imageLoadWorkerCount $
    void $
      forkIO $
        imageLoadThread service
  pure service

imageLoadWorkerCount :: Int
imageLoadWorkerCount = 2

newImageRenderQueue :: IO ImageRenderQueue
newImageRenderQueue =
  ImageRenderQueue
    <$> newTVarIO Nothing
    <*> newTVarIO Nothing
    <*> newMVar ()

{- | Hook scene reconciliation into Vty without exposing terminal protocol
details to the application.
-}
wrapVty :: ImageService -> V.Vty -> V.Vty
wrapVty service vty =
  vty
    { V.update = \picture -> do
        withMVar (imageRenderOutputLock queue) $ \_ ->
          renderFrame queue (V.outputIface vty) $ do
            V.update vty picture
            V.setMode (V.outputIface vty) V.Mouse True
            refreshAfterFrame service
    , V.refresh = do
        withMVar (imageRenderOutputLock queue) $ \_ ->
          renderFrame queue (V.outputIface vty) $ do
            V.refresh vty
            V.setMode (V.outputIface vty) V.Mouse True
            refreshAfterFrame service
    }
 where
  queue = imageServiceRenderQueue service

imageLoadThread :: ImageService -> IO ()
imageLoadThread service =
  forever $
    atomically (readTQueue $ imageServiceRequests service) >>= \case
      RenderImage key source -> renderRequestedImage key source
 where
  warn = logEv (imageServiceEvents service) Warn "Image"

  renderRequestedImage key source = do
    result <-
      runExceptT $
        ensureCachedRenderedImage (imageServiceRawCacheDir service) key source
    atomically $
      modifyTVar' (imageServicePendingRender service) (Set.delete key)
    case result of
      Left err -> do
        atomically $
          modifyTVar' (imageServiceFailedRender service) (Set.insert key)
        warn $ "Error while rendering image:\n" <> show err
      Right rendered ->
        publishRenderedImage service key rendered

{- | Publish completed work in batches so a warm persistent cache cannot flood
the Brick event loop with one redraw per image.
-}
publishRenderedImage :: ImageService -> ImageCacheKey -> RenderedImage -> IO ()
publishRenderedImage service key rendered = do
  shouldNotify <-
    atomically $ do
      modifyTVar' (imageServiceReadyImages service) (Map.insert key rendered)
      queued <- readTVar (imageServiceImagesReadyQueued service)
      if queued
        then pure False
        else do
          writeTVar (imageServiceImagesReadyQueued service) True
          pure True
  when shouldNotify $
    void $
      forkIO $ do
        threadDelay imageUpdateDelay
        writeBChan (imageServiceEvents service) ImagesReady

{- | Build and submit the terminal scene represented by widget declarations.
A placement is painted only when its complete extent lies inside its clip
surface and does not intersect an opaque overlay.  This conservative rule
is correct for Kitty, Sixel, and iTerm without protocol-specific crop bugs.
-}
refreshScene :: ImageService -> ImageScene (MName St) -> [MName St] -> EventM (MName St) St ()
refreshScene service (ImageScene specs) occluderNames = do
  st <- get
  let format = st ^. stEnv . envImageFormat
  desired <- buildDesiredScene service st format occluderNames specs
  liftIO $ storeDesiredScene service format desired

clearScene :: ImageService -> EventM (MName St) St ()
clearScene service = do
  format <- use (stEnv . envImageFormat)
  liftIO $ storeDesiredScene service format Map.empty

{- | Mark the image scene dirty. The Vty wrapper dispatches the refresh only
after the next completed frame, when Brick's extents match the new layout.
-}
queueRefreshImages :: ImageService -> EventM (MName St) St ()
queueRefreshImages service =
  liftIO $ do
    atomically $
      writeTVar (imageServiceRefreshQueued service) True

-- | Dispatch at most one reconciliation request for the frame that has just
-- been rendered. Clearing the flag before the event is sent preserves any
-- new geometry change that occurs while the request is waiting in the queue.
refreshAfterFrame :: ImageService -> IO ()
refreshAfterFrame service = do
  shouldRefresh <-
    atomically $ do
      requested <- readTVar (imageServiceRefreshQueued service)
      writeTVar (imageServiceRefreshQueued service) False
      pure requested
  when shouldRefresh $
    writeBChan (imageServiceEvents service) RefreshImages

imageUpdateDelay :: Int
imageUpdateDelay = 10000

-- | Drain all completed conversions for one main-thread state update.
takeReadyImages :: ImageService -> EventM (MName St) St ImageCache
takeReadyImages service =
  liftIO . atomically $ do
    images <- readTVar (imageServiceReadyImages service)
    writeTVar (imageServiceReadyImages service) Map.empty
    writeTVar (imageServiceImagesReadyQueued service) False
    pure images

buildDesiredScene ::
  ImageService ->
  St ->
  Term.ImageFormat ->
  [MName St] ->
  [ImageSpec (MName St)] ->
  EventM (MName St) St PaintedScene
buildDesiredScene service st format occluderNames specs =
  Map.fromList . concat <$> traverse paintableImage specs
 where
  paintableImage spec =
    lookupVisibleExtent spec occluderNames >>= \case
      Nothing ->
        pure []
      Just extent
        | Just fixedSize <- imageSpecFixedSize spec
        , fixedSize /= extentSize extent ->
            pure []
        | otherwise -> do
            let renderSize = fromMaybe (extentSize extent) (imageSpecFixedSize spec)
                key = ImageCacheKey (imageSpecSource spec) format renderSize
            unless (Map.member key $ st ^. stImageCache) $
              liftIO $
                enqueueRender service key
            pure $
              case Map.lookup key (st ^. stImageCache) of
                Just art@(TerminalGraphic _ _) -> [(imageSpecName spec, (extent, art))]
                _ -> []

{- | Resolve the one extent which is safe to paint for a spec.  Reported
extents are the source of truth; no list-row arithmetic is required.
-}
lookupVisibleExtent ::
  ImageSpec (MName St) ->
  [MName St] ->
  EventM (MName St) St (Maybe (Extent (MName St)))
lookupVisibleExtent spec occluderNames =
  M.lookupExtent (imageSpecName spec) >>= \case
    Nothing ->
      pure Nothing
    Just extent
      | not (validExtent extent) -> pure Nothing
      | otherwise -> do
          clip <- maybe (pure Nothing) M.lookupExtent (imageSpecClip spec)
          occluders <- traverse M.lookupExtent occluderNames
          pure $
            if maybe True (`containsExtent` extent) clip
              && all (not . intersectsExtent extent) (catMaybes occluders)
              then Just extent
              else Nothing

storeDesiredScene :: ImageService -> Term.ImageFormat -> PaintedScene -> IO ()
storeDesiredScene service format scene =
  atomically $ writeTVar (imageRenderDesired queue) (Just (format, scene))
 where
  queue = imageServiceRenderQueue service

enqueueRender :: ImageService -> ImageCacheKey -> IO ()
enqueueRender service key =
  atomically $ do
    pending <- readTVar (imageServicePendingRender service)
    failed <- readTVar (imageServiceFailedRender service)
    unless (Set.member key pending || Set.member key failed) $ do
      modifyTVar' (imageServicePendingRender service) (Set.insert key)
      writeTQueue (imageServiceRequests service) (RenderImage key $ imageSource key)

{- | Removing stale graphics before Vty draws prevents terminal erasure from
blanking text that has just replaced an image.
-}
renderFrame :: ImageRenderQueue -> Output.Output -> IO () -> IO ()
renderFrame queue output drawFrame = do
  maybeDesired <- readTVarIO (imageRenderDesired queue)
  maybePainted <- readTVarIO (imageRenderPainted queue)
  case maybeDesired of
    Just (format, desired) -> do
      case maybePainted of
        Just (paintedFormat, painted)
          | paintedFormat == format && paintedSceneMatches painted desired ->
              drawFrame
          | paintedFormat == format
          , null (staleEntries painted desired) -> do
              drawFrame
              mapM_ (renderPainted output) (changedEntries painted desired)
              atomically $ writeTVar (imageRenderPainted queue) (Just (format, desired))
          | otherwise ->
              repaint paintedFormat painted format desired
        Nothing ->
          repaint format Map.empty format desired
    Nothing ->
      drawFrame
 where
  repaint paintedFormat painted format desired = do
    clearPaintedScene output paintedFormat painted
    drawFrame
    mapM_ (renderPainted output) (Map.elems desired)
    atomically $ writeTVar (imageRenderPainted queue) (Just (format, desired))

paintedSceneMatches :: PaintedScene -> PaintedScene -> Bool
paintedSceneMatches previous desired =
  Map.keysSet previous == Map.keysSet desired
    && and (Map.elems $ Map.intersectionWith paintedEntryMatches previous desired)

staleEntries :: PaintedScene -> PaintedScene -> [(Extent (MName St), RenderedImage)]
staleEntries previous desired =
  Map.elems $
    Map.differenceWith
      (\old new -> if paintedEntryMatches old new then Nothing else Just old)
      previous
      desired

changedEntries :: PaintedScene -> PaintedScene -> [(Extent (MName St), RenderedImage)]
changedEntries previous desired =
  Map.elems $
    Map.differenceWith
      (\new old -> if paintedEntryMatches old new then Nothing else Just new)
      desired
      previous

clearPaintedScene :: Output.Output -> Term.ImageFormat -> PaintedScene -> IO ()
clearPaintedScene output format painted =
  case format of
    Term.Symbols -> pure ()
    Term.Kitty -> emitBytes output (BS8.pack "\ESC_Ga=d\ESC\\")
    _ -> mapM_ (clearExtent output . fst) (Map.elems painted)

paintedEntryMatches :: (Extent (MName St), RenderedImage) -> (Extent (MName St), RenderedImage) -> Bool
paintedEntryMatches (oldExtent, oldImage) (newExtent, newImage) =
  oldImage == newImage
    && extentUpperLeft oldExtent == extentUpperLeft newExtent
    && extentSize oldExtent == extentSize newExtent

renderPainted :: Output.Output -> (Extent (MName St), RenderedImage) -> IO ()
renderPainted output (extent, art) =
  case art of
    TerminalGraphic _ payload ->
      emitBytes output $
        BS.concat [saveCursor, moveCursor (extentUpperLeft extent), payload, restoreCursor]
    InlineSymbols{} ->
      pure ()

clearExtent :: Output.Output -> Extent (MName St) -> IO ()
clearExtent output extent =
  emitBytes output $
    BS.concat $
      [saveCursor]
        <> fmap clearRow [0 .. h - 1]
        <> [restoreCursor]
 where
  Location (x, y) = extentUpperLeft extent
  (w, h) = extentSize extent
  blankRow = BS8.pack (replicate w ' ')
  clearRow row = moveCursor (Location (x, y + row)) <> blankRow

-- Source cache and conversion -------------------------------------------------

ensureCachedImageBytes :: FilePath -> ImageSource -> ExceptT IOException IO BS.ByteString
ensureCachedImageBytes cacheDir source =
  readCachedImageBytes cacheDir source >>= \case
    Just bytes -> pure bytes
    Nothing -> do
      bytes <- readImageBytes source
      writeCachedImageBytes cacheDir source bytes
      pure bytes

readCachedImageBytes :: FilePath -> ImageSource -> ExceptT IOException IO (Maybe BS.ByteString)
readCachedImageBytes cacheDir source = do
  let path = sourceCachePath cacheDir source
  exists <- ExceptT . try $ doesFileExist path
  if exists
    then Just <$> ExceptT (try $ BS.readFile path)
    else pure Nothing

writeCachedImageBytes :: FilePath -> ImageSource -> BS.ByteString -> ExceptT IOException IO ()
writeCachedImageBytes cacheDir source =
  writeCachedBytes cacheDir (sourceCachePath cacheDir source)

writeCachedBytes :: FilePath -> FilePath -> BS.ByteString -> ExceptT IOException IO ()
writeCachedBytes cacheDir path bytes = do
  alreadyCached <- ExceptT . try $ doesFileExist path
  unless alreadyCached $ do
    (tmpPath, handle) <- ExceptT . try $ openBinaryTempFile cacheDir "meloid-image"
    liftIO $ hSetBinaryMode handle True
    ExceptT (try $ BS.hPut handle bytes >> hClose handle) `catchE` \err -> do
      cleanupTempImage tmpPath
      throwE err
    writtenMeanwhile <- ExceptT . try $ doesFileExist path
    if writtenMeanwhile
      then cleanupTempImage tmpPath
      else
        ExceptT (try $ renameFile tmpPath path) `catchE` \err -> do
          cleanupTempImage tmpPath
          throwE err

sourceCachePath :: FilePath -> ImageSource -> FilePath
sourceCachePath cacheDir source =
  cacheDir </> (sourceCacheName source <> ".bin")

ensureCachedRenderedImage :: FilePath -> ImageCacheKey -> ImageSource -> ExceptT IOException IO RenderedImage
ensureCachedRenderedImage cacheDir key source =
  readCachedRenderedImage cacheDir key >>= \case
    Just rendered ->
      pure rendered
    Nothing -> do
      bytes <- ensureCachedImageBytes cacheDir source
      rendered <- renderImageBytes (imageFormat key) (imageSize key) bytes
      writeCachedRenderedImage cacheDir key rendered
      pure rendered

readCachedRenderedImage :: FilePath -> ImageCacheKey -> ExceptT IOException IO (Maybe RenderedImage)
readCachedRenderedImage cacheDir key = do
  let path = renderCachePath cacheDir key
  exists <- ExceptT . try $ doesFileExist path
  if not exists
    then pure Nothing
    else do
      bytes <- ExceptT $ try $ BS.readFile path
      pure . Just $
        case imageFormat key of
          Term.Symbols -> InlineSymbols $ UTF8.toString bytes
          format -> TerminalGraphic format bytes

writeCachedRenderedImage :: FilePath -> ImageCacheKey -> RenderedImage -> ExceptT IOException IO ()
writeCachedRenderedImage cacheDir key =
  writeCachedBytes cacheDir (renderCachePath cacheDir key) . renderedBytes
 where
  renderedBytes = \case
    InlineSymbols text -> UTF8.fromString text
    TerminalGraphic _ bytes -> bytes

renderCachePath :: FilePath -> ImageCacheKey -> FilePath
renderCachePath cacheDir key =
  cacheDir
    </> ( sourceCacheName (imageSource key)
           <> "-"
           <> Term.formatArg (imageFormat key)
           <> "-"
           <> show width
           <> "x"
           <> show height
           <> ".render"
       )
 where
  (width, height) = imageSize key

sourceCacheName :: ImageSource -> FilePath
sourceCacheName = (`showHex` "") . foldl step fnvOffset . BS.unpack . UTF8.fromString . show
 where
  -- The complete source remains the in-memory cache key. The on-disk name is
  -- compact so deep music-library paths cannot exceed filename limits.
  fnvOffset :: Word64
  fnvOffset = 14695981039346656037

  fnvPrime :: Word64
  fnvPrime = 1099511628211

  step hash byte = (hash `xor` fromIntegral byte) * fnvPrime

readImageBytes :: ImageSource -> ExceptT IOException IO BS.ByteString
readImageBytes = \case
  ImageFile path -> ExceptT $ try $ BS.readFile path
  MpdEmbeddedArt uri -> do
    (host, port) <- liftIO getMPDEndpoint
    let readArt request = ExceptT $ fmap (either (Left . userError) Right) $ request host port uri
    readArt MPD.readPicture `catchE` const (readArt MPD.albumArt)

renderImageBytes :: Term.ImageFormat -> ImageSize -> BS.ByteString -> ExceptT IOException IO RenderedImage
renderImageBytes format size bytes = do
  rendered <- chafaOutput format size bytes
  pure $
    case format of
      Term.Symbols -> InlineSymbols (UTF8.toString rendered)
      _ -> TerminalGraphic format rendered

chafaOutput :: Term.ImageFormat -> ImageSize -> BS.ByteString -> ExceptT IOException IO BS.ByteString
chafaOutput format (w, h) bytes = do
  let sizeArg = show w <> "x" <> show h
  tmpPath <- writeTempImage bytes
  output <-
    runChafa format sizeArg tmpPath `catchE` const do
      pngPath <- convertToPng tmpPath
      cleanupTempImage tmpPath
      retried <- runChafa format sizeArg pngPath
      cleanupTempImage pngPath
      pure retried
  cleanupTempImage tmpPath
  pure output

writeTempImage :: BS.ByteString -> ExceptT IOException IO FilePath
writeTempImage bytes = do
  temporaryDir <- liftIO getTemporaryDirectory
  (tmpPath, handle) <- ExceptT . try $ openBinaryTempFile temporaryDir "meloid-image-input"
  liftIO $ hSetBinaryMode handle True
  ExceptT (try $ BS.hPut handle bytes >> hClose handle) `catchE` \err -> do
    cleanupTempImage tmpPath
    throwE err
  pure tmpPath

convertToPng :: FilePath -> ExceptT IOException IO FilePath
convertToPng inputPath = do
  temporaryDir <- liftIO getTemporaryDirectory
  (tmpPath, handle) <- ExceptT . try $ openBinaryTempFile temporaryDir "meloid-image-converted.png"
  liftIO $ hClose handle
  readRawProcess "magick" [inputPath, tmpPath] (handleMagickResult tmpPath) `catchE` \err -> do
    cleanupTempImage tmpPath
    throwE err
 where
  handleMagickResult tmpPath = \case
    (ExitSuccess, _, _) -> pure tmpPath
    (_, _, err) -> do
      cleanupTempImage tmpPath
      throwE $ userError $ UTF8.toString err

runChafa :: Term.ImageFormat -> String -> FilePath -> ExceptT IOException IO BS.ByteString
runChafa format sizeArg imagePath =
  readRawProcess "chafa" (chafaArgs format sizeArg imagePath) $ \case
    (ExitSuccess, out, _) -> pure $ sanitizeChafaOutput format out
    (_, _, err) -> throwE $ userError $ UTF8.toString err

chafaArgs :: Term.ImageFormat -> String -> FilePath -> [String]
chafaArgs format sizeArg imagePath =
  ["-s", sizeArg, "-f", Term.formatArg format, imagePath]

readRawProcess ::
  String ->
  [String] ->
  ((ExitCode, BS.ByteString, BS.ByteString) -> ExceptT IOException IO a) ->
  ExceptT IOException IO a
readRawProcess prog args handleResult = rawProcess prog args >>= handleResult

rawProcess :: String -> [String] -> ExceptT IOException IO (ExitCode, BS.ByteString, BS.ByteString)
rawProcess prog args =
  ( ExceptT . try $
      createProcess
        (proc prog args)
          { std_in = NoStream
          , std_out = CreatePipe
          , std_err = CreatePipe
          }
  )
    >>= \case
      (_, Just hout, Just herr, processHandle) -> do
        out <- liftIO $ BS.hGetContents hout
        err <- liftIO $ BS.hGetContents herr
        code <- liftIO $ waitForProcess processHandle
        pure (code, out, err)
      _ -> throwE $ userError "failed to run process: unexpected process pipe setup"

sanitizeChafaOutput :: Term.ImageFormat -> BS.ByteString -> BS.ByteString
sanitizeChafaOutput Term.Symbols = trimTrailingLineBreaks
sanitizeChafaOutput Term.Kitty = trimTrailingLineBreaks . pinKittyPlacement . extractKittyGraphics
sanitizeChafaOutput _ = trimTrailingLineBreaks

trimTrailingLineBreaks :: BS.ByteString -> BS.ByteString
trimTrailingLineBreaks = BS.reverse . BS.dropWhile isLineBreak . BS.reverse
 where
  isLineBreak byte = byte == 10 || byte == 13

pinKittyPlacement :: BS.ByteString -> BS.ByteString
pinKittyPlacement bytes =
  case BS.breakSubstring marker bytes of
    (prefix, rest)
      | BS.null rest -> bytes
      | otherwise -> prefix <> marker <> cursorStatic <> BS.drop (BS.length marker) rest
 where
  marker = BS.pack [27, 95, 71, 97, 61, 84, 44]
  cursorStatic = BS.pack [67, 61, 49, 44]

extractKittyGraphics :: BS.ByteString -> BS.ByteString
extractKittyGraphics = BS.concat . go
 where
  start = BS.pack [27, 95, 71]
  endST = BS.pack [27, 92]

  go bytes =
    let (_, rest) = BS.breakSubstring start bytes
     in if BS.null rest
          then []
          else
            let (chunk, remaining) = takeKittySequence rest
             in chunk : go remaining

  takeKittySequence bytes =
    case kittySequenceEnd bytes of
      Nothing -> (bytes, BS.empty)
      Just end -> (BS.take end bytes, BS.drop end bytes)

  kittySequenceEnd bytes = earliest (stEnd bytes) (belEnd bytes)

  stEnd bytes =
    let (before, after) = BS.breakSubstring endST bytes
     in if BS.null after then Nothing else Just (BS.length before + BS.length endST)

  belEnd bytes = (+ 1) <$> BS.elemIndex 7 bytes

  earliest Nothing y = y
  earliest x Nothing = x
  earliest (Just x) (Just y) = Just (min x y)

cleanupTempImage :: FilePath -> ExceptT IOException IO ()
cleanupTempImage path =
  liftIO $ void (try (removeFile path) :: IO (Either IOException ()))
