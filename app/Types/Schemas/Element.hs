{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Layout elements stored in the application configuration.
module Types.Schemas.Element (
  LayoutElement (..),
  placeholderLayout,
  formatElementName,
) where

import Control.Monad (when)
import Data.Aeson qualified as JSON
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Text.Megaparsec ((<|>))

data LayoutElement
  = EHBox (Maybe [Double]) [LayoutElement]
  | EVBox (Maybe [Double]) [LayoutElement]
  | ETabs [LayoutElement]
  | EAlbumList
  | ETrackList
  | EPlaylistList
  | ECurrentQueue
  | EEqualizer
  | ESpectrum
  | ESongInfo
  | EPlaceholder
  deriving (Eq, Show, Generic)

-- | Format a layout element as a string.
formatElementName :: LayoutElement -> String
formatElementName (EHBox _ _) = "hBox"
formatElementName (EVBox _ _) = "vBox"
formatElementName EAlbumList = "albumList"
formatElementName ETrackList = "trackList"
formatElementName EPlaylistList = "playlistList"
formatElementName ECurrentQueue = "currentQueue"
formatElementName EEqualizer = "equalizer"
formatElementName ESpectrum = "spectrum"
formatElementName ESongInfo = "songInfo"
formatElementName (ETabs _) = "tabs"
formatElementName EPlaceholder = "placeholder"

parseElementName :: String -> Maybe LayoutElement
parseElementName "albumList" = Just EAlbumList
parseElementName "trackList" = Just ETrackList
parseElementName "playlistList" = Just EPlaylistList
parseElementName "currentQueue" = Just ECurrentQueue
parseElementName "equalizer" = Just EEqualizer
parseElementName "spectrum" = Just ESpectrum
parseElementName "songInfo" = Just ESongInfo
parseElementName "placeholder" = Just EPlaceholder
parseElementName _ = Nothing

placeholderLayout :: LayoutElement
placeholderLayout = EPlaceholder

instance JSON.FromJSON LayoutElement where
  parseJSON value =
    JSON.withText "LayoutElement" parseElementText value
      <|> JSON.withObject "LayoutElement" parseElementObject value

instance JSON.ToJSON LayoutElement where
  toJSON element =
    case element of
      EHBox weights elements -> JSON.object ["hBox" JSON..= encodeBoxItems weights elements]
      EVBox weights elements -> JSON.object ["vBox" JSON..= encodeBoxItems weights elements]
      ETabs elements -> JSON.object ["tabs" JSON..= fmap JSON.toJSON elements]
      leaf -> JSON.String (Text.pack (formatElementName leaf))

  toEncoding element =
    case element of
      EHBox weights elements -> JSON.pairs ("hBox" JSON..= encodeBoxItems weights elements)
      EVBox weights elements -> JSON.pairs ("vBox" JSON..= encodeBoxItems weights elements)
      ETabs elements -> JSON.pairs ("tabs" JSON..= fmap JSON.toJSON elements)
      leaf -> JSON.toEncoding (formatElementName leaf)

encodeBoxItems :: Maybe [Double] -> [LayoutElement] -> [JSON.Value]
encodeBoxItems weights elements =
  case weights of
    Nothing -> fmap JSON.toJSON elements
    Just ws -> JSON.object ["weights" JSON..= ws] : fmap JSON.toJSON elements

parseElementText :: Text.Text -> Parser LayoutElement
parseElementText text =
  case parseElementName (Text.unpack text) of
    Just element -> pure element
    Nothing -> fail $ "Unknown layout element name: " <> Text.unpack text

parseElementObject :: JSON.Object -> Parser LayoutElement
parseElementObject kv
  | KeyMap.size kv /= 1 =
      fail "Layout element objects must contain exactly one key."
  | otherwise =
      case KeyMap.toList kv of
        [(name, value)] ->
          case Key.toString name of
            "hBox" -> parseHBox value
            "vBox" -> parseVBox value
            "tabs" -> parseTabs value
            other -> fail $ "Unknown layout element container: " <> other
        _ -> fail "Layout element objects must contain exactly one key."
 where
  parseHBox value = do
    (weights, elements) <- parseWeightedBoxItems "hBox" value
    pure $ EHBox weights elements

  parseVBox value = do
    (weights, elements) <- parseWeightedBoxItems "vBox" value
    pure $ EVBox weights elements

  parseTabs value = ETabs <$> JSON.parseJSON value

  parseWeightedBoxItems boxName value = JSON.parseJSON value >>= parseBoxItems boxName

  parseBoxItems _ [] = pure (Nothing, [])
  parseBoxItems boxName (firstItem : rest) =
    case firstItem of
      JSON.Object obj
        | Just weightsValue <- KeyMap.lookup "weights" obj -> do
            when
              (KeyMap.size obj /= 1)
              (fail $ boxName <> " weights item must only contain the 'weights' key.")
            weights <- JSON.parseJSON weightsValue
            elements <- traverse JSON.parseJSON rest
            pure (Just weights, elements)
      _ -> do
        elements <- traverse JSON.parseJSON (firstItem : rest)
        pure (Nothing, elements)
