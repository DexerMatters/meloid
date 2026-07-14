{-# LANGUAGE LambdaCase #-}

{- | Generic image widgets and the image declarations used by the UI.
The widget itself is independent of album art: it renders any 'ImageSource'
at the cell size allocated by Brick.  The two album helpers merely describe
where this application obtains those particular images.
-}
module Widgets.Image (
  ImageSlot (..),
  AlbumImageClip (..),
  albumThumbnailSize,
  drawImage,
  drawPlayingImage,
  drawAlbumThumbnail,
  imageScene,
) where

import Brick qualified as B
import Brick.Widgets.Core qualified as W
import Compat.Ansi qualified as Ansi
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Vector qualified as Vec
import Graphics.Vty qualified as V
import Lens.Micro ((^.))
import Network.MPD qualified as MPD
import Types
import Widgets.Common (scrollViewportBy)
import Widgets.Elements.Common (ElementNode (..), ElementPath, childPaths, pathVariant)

-- | Image placements owned by this view.
data ImageSlot
  = PlayingImage
  | AlbumThumbnail ElementPath Int
  deriving (Eq, Show)

-- | The album viewport is a clipping surface for its thumbnail images.
data AlbumImageClip = AlbumImageClip ElementPath

instance Drawable St ImageSlot where
  draw _ _ = W.emptyWidget
  variant PlayingImage = 0
  variant (AlbumThumbnail _ index) = index
  parent PlayingImage = Just (ParentView MainView)
  parent (AlbumThumbnail path _) = Just (ParentName $ mName $ AlbumImageClip path)

instance Drawable St AlbumImageClip where
  draw _ _ = W.emptyWidget
  onMouseScrollUp (AlbumImageClip path) = Just $ scrollViewportBy (mName $ AlbumImageClip path) (negate $ snd albumThumbnailSize)
  onMouseScrollDown (AlbumImageClip path) = Just $ scrollViewportBy (mName $ AlbumImageClip path) (snd albumThumbnailSize)
  parent (AlbumImageClip path) = Just . ParentName . mName $ ElementNode path
  variant (AlbumImageClip path) = pathVariant path

{- | Render an image using its actual Brick allocation.  The same declaration
is later consumed by 'Compat.Image' to prepare out-of-band graphics.
-}
drawImage :: St -> ImageSpec (MName St) -> B.Widget (MName St)
drawImage st spec =
  B.reportExtent name $
    constrain fixedSize $
      B.Widget B.Greedy B.Greedy $ do
        ctx <- B.getContext
        let allocatedSize = (max 1 $ ctx ^. B.availWidthL, max 1 $ ctx ^. B.availHeightL)
            renderSize = fromMaybe allocatedSize fixedSize
            key = ImageCacheKey (imageSpecSource spec) (st ^. stEnv . envImageFormat) renderSize
        B.render $
          case Map.lookup key (st ^. stImageCache) of
            Just (InlineSymbols art) ->
              W.vLimit (snd renderSize) . W.hLimit (fst renderSize) $ Ansi.rawAnsi art
            _ ->
              W.fill ' '
 where
  name = imageSpecName spec
  fixedSize = imageSpecFixedSize spec
  constrain = maybe id $ \(w, h) -> W.hLimit w . W.vLimit h

albumThumbnailSize :: ImageSize
albumThumbnailSize = (6, 3)

-- | The now-playing image is embedded in the current track, when available.
drawPlayingImage :: St -> B.Widget (MName St)
drawPlayingImage st =
  maybe W.emptyWidget (drawImage st) (playingImageSpec st)

-- | Render one image in the album viewport.
drawAlbumThumbnail :: St -> ElementPath -> Int -> B.Widget (MName St)
drawAlbumThumbnail st path index =
  B.Widget B.Fixed B.Fixed $ do
    viewport <- W.unsafeLookupViewport (mName $ AlbumImageClip path)
    B.render $
      case albumThumbnailSpec st path index of
        Just spec
          | maybe False (contains index . visibleThumbnailRange) viewport ->
              drawImage st spec
        Just _ ->
          W.hLimit width . W.vLimit height $ W.fill ' '
        Nothing ->
          W.emptyWidget
 where
  (width, height) = albumThumbnailSize
  contains value (start, end) = value >= start && value < end

{- | The complete declarative image scene.  Missing, hidden, or clipped
widgets are discarded by the compatibility backend after extent lookup.
-}
imageScene :: St -> B.EventM (MName St) St (ImageScene (MName St))
imageScene st = do
  thumbnailIndices <- traverse (visibleAlbumThumbnailIndices st) (albumListPaths st)
  pure . ImageScene . catMaybes $
    playingImageSpec st
      : [albumThumbnailSpec st path index | (path, indices) <- thumbnailIndices, index <- indices]

playingImageSpec :: St -> Maybe (ImageSpec (MName St))
playingImageSpec st = do
  song <- st ^. stPlaying . psCurrentSong
  pure $
    ImageSpec
      { imageSpecName = mName PlayingImage
      , imageSpecSource = MpdEmbeddedArt $ MPD.toString $ MPD.sgFilePath song
      , imageSpecClip = Nothing
      , imageSpecFixedSize = Nothing
      }

albumThumbnailSpec :: St -> ElementPath -> Int -> Maybe (ImageSpec (MName St))
albumThumbnailSpec st path index = do
  album <- (st ^. stConfig . csAllAlbums) Vec.!? index
  song <- albumSongs album Vec.!? 0
  pure $
    ImageSpec
      { imageSpecName = mName $ AlbumThumbnail path index
      , imageSpecSource = MpdEmbeddedArt $ MPD.toString $ MPD.sgFilePath song
      , imageSpecClip = Just $ mName $ AlbumImageClip path
      , imageSpecFixedSize = Just albumThumbnailSize
      }

{- | Only visible rows need terminal-image conversion.  This avoids scanning
and scheduling the whole album catalog on every Brick refresh.
-}
visibleAlbumThumbnailIndices :: St -> ElementPath -> B.EventM (MName St) St (ElementPath, [Int])
visibleAlbumThumbnailIndices st path =
  B.lookupViewport (mName $ AlbumImageClip path) >>= \case
    Nothing ->
      pure (path, [])
    Just viewport ->
      pure (path, [start' .. end' - 1])
     where
      totalAlbums = Vec.length (st ^. stConfig . csAllAlbums)
      (start, end) = visibleThumbnailRange viewport
      start' = max 0 start
      end' = min totalAlbums end

-- | Find every configured album-list node. Hidden tabs and collapsed nodes
-- have no viewport, so they naturally contribute no image requests.
albumListPaths :: St -> [ElementPath]
albumListPaths st = go [] (st ^. stConfig . csConfigs . cvLayout)
 where
  go path = \case
    EAlbumList -> [path]
    EHBox _ children -> descend path children
    EVBox _ children -> descend path children
    ETabs children -> descend path children
    _ -> []

  descend path children = concatMap (uncurry go) (zip (childPaths children path) children)

visibleThumbnailRange :: B.Viewport -> (Int, Int)
visibleThumbnailRange viewport =
  (top `div` height, (top + visibleHeight + height - 1) `div` height)
 where
  top = viewport ^. B.vpTop
  visibleHeight = V.regionHeight $ viewport ^. B.vpSize
  height = snd albumThumbnailSize
