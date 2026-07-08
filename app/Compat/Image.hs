{-# LANGUAGE LambdaCase #-}

{- | This is a module that provides some useful functions for
managing the album art cache.
-}
module Compat.Image (
  ImageService,
  prefetchAlbumArtCatalog,
  clearScene,
  refreshScene,
  startImageService,
  wrapVty,
  queueRefreshImages,
)
where

import Brick
import Brick.BChan (BChan, writeBChan, writeBChanNonBlocking)
import Types.Configs
import Compat.Term
import Compat.Term qualified as Term
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM
import Control.Exception (IOException)
import Control.Monad (forM, forever, replicateM_, unless, void)
import Control.Monad.State (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Foldable (for_)
import Data.List (nub)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Vector qualified as Vec
import Graphics.Vty qualified as V
import Graphics.Vty.Output qualified as Output
import Image qualified as ImageProcessor
import Lens.Micro ((^.))
import Lens.Micro.Mtl
import Network.MPD qualified as MPD
import Types
import Widgets qualified as Names

data ImageRenderQueue = ImageRenderQueue
  { imageRenderDesired :: TVar (Maybe (Term.ImageFormat, PaintedScene))
  , imageRenderPainted :: TVar (Maybe (Term.ImageFormat, PaintedScene))
  , imageRenderOutputLock :: MVar ()
  }

{- | ImageService is to manage global state for the image cache.
It contains the raw cache directory, the requests queue, and the
pending render queue.
-}
data ImageService = ImageService
  { imageServiceRawCacheDir :: FilePath
  , imageServiceRequests :: TQueue ImageRequest
  , imageServicePendingRender :: TVar (Set.Set (AlbumArtKey, Term.ImageFormat))
  , imageServiceRenderQueue :: ImageRenderQueue
  }

-- | Start the image service with workers for rendering album art.
startImageService :: BChan Event -> IO ImageService
startImageService evChan = do
  rawCacheDir <- albumArtCacheDir
  requests <- newTQueueIO
  pendingRender <- newTVarIO Set.empty
  renderQueue <- newImageRenderQueue
  let service =
        ImageService
          { imageServiceRawCacheDir = rawCacheDir
          , imageServiceRequests = requests
          , imageServicePendingRender = pendingRender
          , imageServiceRenderQueue = renderQueue
          }
  replicateM_ imageLoadWorkerCount $
    void $
      forkIO $
        imageLoadThread service evChan
  pure service

imageLoadWorkerCount :: Int
imageLoadWorkerCount = 4

newImageRenderQueue :: IO ImageRenderQueue
newImageRenderQueue =
  ImageRenderQueue
    <$> newTVarIO Nothing
    <*> newTVarIO Nothing
    <*> newMVar ()

{- | Wraps a Vty to make it render album art.
The function hooks the image rendering functionality into the Vty
output interface.
-}
wrapVty :: ImageService -> V.Vty -> V.Vty
wrapVty service vty =
  vty
    { V.update = \picture -> do
        withMVar (imageRenderOutputLock queue) $ \_ ->
          V.update vty picture
            >> V.setMode (V.outputIface vty) V.Mouse True
            >> renderLatestScene queue (V.outputIface vty)
    , V.refresh = do
        withMVar (imageRenderOutputLock queue) $ \_ ->
          V.refresh vty
            >> V.setMode (V.outputIface vty) V.Mouse True
            >> renderLatestScene queue (V.outputIface vty)
    }
 where
  queue = imageServiceRenderQueue service

imageLoadThread :: ImageService -> BChan Event -> IO ()
imageLoadThread service evChan =
  forever $
    atomically (readTQueue $ imageServiceRequests service) >>= \case
      RenderAlbumArt key uri format ->
        renderAlbumArt key uri format
 where
  warn = logEv evChan Warn "Image"
  postEvent :: Event -> IO ()
  postEvent = writeBChan evChan

  renderSizes =
    nub [albumArtPlayingSize, albumArtThumbSize]

  renderAlbumArt :: AlbumArtKey -> FilePath -> Term.ImageFormat -> IO ()
  renderAlbumArt key uri format = do
    result <- runExceptT $ do
      bytes <- ImageProcessor.ensureCachedAlbumArtBytes (imageServiceRawCacheDir service) key uri
      arts <-
        forM renderSizes $ \size -> do
          art <- ImageProcessor.renderAlbumArt format size bytes
          pure (size, art)
      liftIO $ postEvent (LoadAlbumArt (key, Map.fromList arts))
    atomically $
      modifyTVar' (imageServicePendingRender service) (Set.delete (key, format))
    either (warn . ("Error while rendering album art:\n" <>) . show) (const $ pure ()) (result :: Either IOException ())

renderLatestScene :: ImageRenderQueue -> Output.Output -> IO ()
renderLatestScene queue output = do
  maybeDesired <- readTVarIO (imageRenderDesired queue)
  maybePainted <- readTVarIO (imageRenderPainted queue)
  case maybeDesired of
    Just (format, desired) -> do
      case maybePainted of
        Just (paintedFormat, painted)
          | paintedFormat /= format ->
              clearPaintedScene output paintedFormat painted
        _ ->
          pure ()
      let previous = maybe Map.empty sndSameFormat maybePainted
          sndSameFormat (paintedFormat, painted)
            | paintedFormat == format = painted
            | otherwise = Map.empty
      syncOutOfBandScene output format previous desired
      atomically $ writeTVar (imageRenderPainted queue) (Just (format, desired))
    _ ->
      pure ()

storeDesiredScene :: ImageService -> Term.ImageFormat -> PaintedScene -> IO ()
storeDesiredScene service format scene =
  atomically $ writeTVar (imageRenderDesired queue) (Just (format, scene))
 where
  queue = imageServiceRenderQueue service

{- | Send RefreshImages event to the main thread, telling it to
re-render the album art.
-}
queueRefreshImages :: BChan Event -> EventM (MName St) St ()
queueRefreshImages chan =
  liftIO $ void $ writeBChanNonBlocking chan RefreshImages

-- | Load album art for the whole catalog in list order.
prefetchAlbumArtCatalog :: ImageService -> Vec.Vector Album -> EventM (MName St) St ()
prefetchAlbumArtCatalog service albums = do
  format <- use (stEnv . envImageFormat)
  cachedArt <- use stPicCache
  liftIO $
    mapM_
      (queueAlbum format cachedArt)
      (Vec.toList albums)
 where
  queueAlbum format cachedArt album =
    for_ (albumSongs album Vec.!? 0) $ \song ->
      unless (Map.member (albumArtKey album) cachedArt) $
        enqueueRender service (albumArtKey album) (MPD.toString $ MPD.sgFilePath song) format

enqueueRender :: ImageService -> AlbumArtKey -> FilePath -> Term.ImageFormat -> IO ()
enqueueRender service key uri format =
  atomically $ do
    let requestKey = (key, format)
    pending <- readTVar (imageServicePendingRender service)
    unless (Set.member requestKey pending) $ do
      modifyTVar' (imageServicePendingRender service) (Set.insert requestKey)
      writeTQueue (imageServiceRequests service) (RenderAlbumArt key uri format)

-- | Clear the current scene.
clearScene :: ImageService -> EventM (MName St) St ()
clearScene service = do
  format <- use (stEnv . envImageFormat)
  liftIO $ storeDesiredScene service format Map.empty

-- | Refresh the current scene.
refreshScene :: ImageService -> EventM (MName St) St ()
refreshScene service = do
  format <- use (stEnv . envImageFormat)
  if not (Term.isOutOfBandFormat format)
    then pure ()
    else do
      desired <- buildDesiredScene
      liftIO $ storeDesiredScene service format desired

buildDesiredScene :: EventM (MName St) St PaintedScene
buildDesiredScene = do
  st <- get
  let occluderNames = Names.activeOccluderNames st
  playingEntries <-
    case st ^. stCurrentAlbum of
      Nothing -> pure []
      Just _ ->
        paintableImage st occluderNames (mName Names.AlbumArtPlaying) albumArtPlayingSize

  visibleThumbs <- visibleAlbumThumbs st
  thumbs <-
    fmap concat . forM visibleThumbs $ \(i, _) -> do
      let name = mName (Names.AlbumArtThumb i)
      paintableImage st occluderNames name albumArtThumbSize

  pure $ Map.fromList (playingEntries <> thumbs)
 where
  visibleAlbumThumbs currentSt = do
    let albums = currentSt ^. stConfig . csAllAlbums
        totalAlbums = Vec.length albums
        rowHeight = max 1 (snd albumArtThumbSize)
    lookupViewport (mName Names.AllAlbumList) >>= \case
      Nothing ->
        pure []
      Just viewport' ->
        let top = viewport' ^. vpTop
            visibleHeight = V.regionHeight $ viewport' ^. vpSize
            start = max 0 (top `div` rowHeight)
            end = min totalAlbums $ (top + visibleHeight + rowHeight - 1) `div` rowHeight
         in pure
              [ (i, album)
              | i <- [start .. end - 1]
              , Just album <- [albums Vec.!? i]
              ]

  paintableImage :: St -> [MName St] -> MName St -> ImageSize -> EventM (MName St) St [(MName St, (Extent (MName St), RenderedImage))]
  paintableImage currentSt occluderNames name size =
    Names.lookupStrictExtent name occluderNames >>= \case
      Just (Names.StrictExtent extent []) ->
        case Names.lookupRenderedImage currentSt name size of
          Just art@(TerminalGraphic _ _) ->
            pure [(name, (extent, art))]
          Just InlineSymbols{} ->
            pure []
          Nothing ->
            pure []
      _ ->
        pure []

syncOutOfBandScene :: Output.Output -> Term.ImageFormat -> PaintedScene -> PaintedScene -> IO ()
syncOutOfBandScene output format previous desired
  | paintedSceneMatches previous desired = pure ()
  | format == Term.Kitty && null stale = do
      mapM_ (renderPainted output) (Map.elems changed)
  | format == Term.Kitty = do
      clearPaintedScene output format previous
      mapM_ (renderPainted output) (Map.elems desired)
  | otherwise = do
      clearStale output format previous desired
      mapM_ (renderPainted output) (Map.elems changed)
 where
  stale =
    staleEntries previous desired
  changed =
    Map.differenceWith
      (\new old -> if paintedEntryMatches old new then Nothing else Just new)
      desired
      previous

paintedSceneMatches :: PaintedScene -> PaintedScene -> Bool
paintedSceneMatches previous desired =
  Map.keysSet previous == Map.keysSet desired && Map.null changed
 where
  changed =
    Map.differenceWith
      (\old new -> if paintedEntryMatches old new then Nothing else Just old)
      previous
      desired

clearPaintedScene :: Output.Output -> Term.ImageFormat -> PaintedScene -> IO ()
clearPaintedScene output format painted =
  case format of
    Term.Symbols -> pure ()
    Term.Kitty -> emitBytes output (BS8.pack "\ESC_Ga=d\ESC\\")
    _ -> mapM_ (clearExtent output . fst) (Map.elems painted)

clearStale :: Output.Output -> Term.ImageFormat -> PaintedScene -> PaintedScene -> IO ()
clearStale output format previous desired =
  case format of
    Term.Symbols -> pure ()
    Term.Kitty -> clearPaintedScene output format previous
    _ -> mapM_ (clearExtent output . fst) (staleEntries previous desired)

staleEntries :: PaintedScene -> PaintedScene -> [(Extent (MName St), RenderedImage)]
staleEntries previous desired =
  Map.elems $
    Map.differenceWith
      (\old new -> if paintedEntryMatches old new then Nothing else Just old)
      previous
      desired

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
