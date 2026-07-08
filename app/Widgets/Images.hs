{-# LANGUAGE LambdaCase #-}

{- | This module provides widgets to display an image.
There is the way to wrap the album art into the Brick's
widget. However, when the image format supports high
resolution, the widget is just a placeholder.
-}
module Widgets.Images (
  AlbumArtPlaying (..),
  lookupAlbumThumbRenderedImage,
  lookupPlayingRenderedImage,
  renderImage,
) where

import Brick
import Brick.Widgets.Core qualified as W
import Compat.Ansi qualified as Ansi
import Data.Map qualified as Map
import Data.Vector qualified as Vec
import Lens.Micro (to, (^.))
import Types

data AlbumArtPlaying = AlbumArtPlaying

{- | This widget is a placeholder for album art when the image
format supports high resolution. It falls back to an ANSI
symbol with a best color palette provided by Chafa.
-}
renderImage :: ImageSize -> Maybe RenderedImage -> Widget n
renderImage (w, h) = \case
  Just (InlineSymbols art) ->
    W.vLimit h . W.hLimit w $ Ansi.rawAnsi art
  _ ->
    W.vLimit h . W.hLimit w $ W.fill ' '

-- | Lookup the image of the playing album by size
lookupPlayingRenderedImage :: St -> ImageSize -> Maybe RenderedImage
lookupPlayingRenderedImage st size =
  st ^. stCurrentAlbumArt >>= (Map.!? size)

-- | Lookup the image of an album in the album list by size
lookupAlbumThumbRenderedImage :: St -> Int -> ImageSize -> Maybe RenderedImage
lookupAlbumThumbRenderedImage st i size = do
  album <- st ^. stConfig . csAllAlbums . to (Vec.!? i)
  art <- st ^. stPicCache . to (Map.!? albumArtKey album)
  art Map.!? size

instance Drawable St AlbumArtPlaying where
  draw _ st =
    renderImage albumArtPlayingSize $
      lookupPlayingRenderedImage st albumArtPlayingSize
  willReportExtent _ = True
  parent _ = Just (ParentView MainView)
