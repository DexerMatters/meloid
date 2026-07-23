{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | User-editable application configuration.
module Types.Schemas.Config (
  ToString (..),
  FromString (..),
  ConfigValue (..),
  cvShowWelcome,
  cvColorMode,
  cvEq,
  cvLayout,
) where

import Attrs (ColorMode)
import Data.Aeson qualified as JSON
import Data.ByteString.UTF8 qualified as UTF8
import Data.Char (toLower)
import Data.List (stripPrefix)
import Data.Yaml qualified as YAML
import GHC.Generics (Generic)
import Lens.Micro.TH (makeLenses)
import Types.Schemas.Element (LayoutElement, placeholderLayout)

class ToString a where
  toString :: a -> String

class FromString a where
  fromString :: String -> Either String a

data ConfigValue = ConfigValue
  { _cvShowWelcome :: Bool
  , _cvColorMode :: ColorMode
  , _cvEq :: String
  , _cvLayout :: LayoutElement
  }
  deriving (Eq, Show, Generic)

makeLenses ''ConfigValue

configValueJsonOptions :: JSON.Options
configValueJsonOptions =
  JSON.defaultOptions
    { JSON.fieldLabelModifier = lowerHead . maybe "" id . stripPrefix "_cv"
    }
 where
  lowerHead [] = []
  lowerHead (x : xs) = toLower x : xs

instance JSON.FromJSON ConfigValue where
  parseJSON = JSON.withObject "ConfigValue" $ \obj ->
    ConfigValue
      <$> obj JSON..: "showWelcome"
      <*> obj JSON..: "colorMode"
      <*> obj JSON..: "eq"
      <*> obj JSON..:? "layout" JSON..!= placeholderLayout

instance JSON.ToJSON ConfigValue where
  toJSON = JSON.genericToJSON configValueJsonOptions
  toEncoding = JSON.genericToEncoding configValueJsonOptions

instance ToString ConfigValue where
  toString = UTF8.toString . YAML.encode

instance FromString ConfigValue where
  fromString input =
    case YAML.decodeEither' (UTF8.fromString input) of
      Left err -> Left (YAML.prettyPrintParseException err)
      Right value -> Right value
