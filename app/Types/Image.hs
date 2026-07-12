{- | Types shared by the image widget and the terminal image backend.
This module deliberately has no dependency on application state or Brick.  It
keeps the image pipeline cycle-free while allowing widgets to describe an
image scene declaratively.
-}
module Types.Image (
  ImageSource (..),
  ImageSize,
  ImageCacheKey (..),
  RenderedImage (..),
  ImageCache,
  ImageSpec (..),
  ImageScene (..),
  ImageRequest (..),
) where

import Compat.Term (ImageFormat)
import Data.ByteString (ByteString)
import Data.Map qualified as Map

-- | Where the backend obtains image bytes.
data ImageSource
  = ImageFile FilePath
  | MpdEmbeddedArt FilePath
  deriving (Eq, Ord, Show)

-- | Width and height measured in terminal cells.
type ImageSize = (Int, Int)

-- | A rendered variant is specific to a source, protocol, and cell extent.
data ImageCacheKey = ImageCacheKey
  { imageSource :: ImageSource
  , imageFormat :: ImageFormat
  , imageSize :: ImageSize
  }
  deriving (Eq, Ord, Show)

-- | Either Brick-renderable symbols or an out-of-band terminal payload.
data RenderedImage
  = InlineSymbols String
  | TerminalGraphic ImageFormat ByteString
  deriving (Eq, Show)

-- | The immutable render snapshot published to the Brick state.
type ImageCache = Map.Map ImageCacheKey RenderedImage

{- | A widget's image declaration.  The optional name is the viewport or
container that must fully contain the image before it can be painted.
-}
data ImageSpec n = ImageSpec
  { imageSpecName :: n
  , imageSpecSource :: ImageSource
  , imageSpecClip :: Maybe n
  , imageSpecFixedSize :: Maybe ImageSize
  }

-- | All images that may occur in the current Brick scene.
newtype ImageScene n = ImageScene
  { imageSceneSpecs :: [ImageSpec n]
  }

-- | A background conversion request.
data ImageRequest = RenderImage ImageCacheKey ImageSource
