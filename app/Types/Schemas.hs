{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | This model provides serializable structures for
configuring the application. It also provides parsing and
serialization helpers.
-}
module Types.Schemas (
  ConfigValue (..),
  cvShowWelcome,
  cvColorMode,
) where

import Data.Aeson qualified as JSON
import Data.Char (toLower)
import Data.List (stripPrefix)
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (Lift)
import Lens.Micro.TH (makeLenses)

-- | User-editable configuration loaded from the YAML file.
data ConfigValue = ConfigValue
  { _cvShowWelcome :: Bool
  , _cvColorMode :: String
  }
  deriving (Eq, Show, Generic, Lift)

makeLenses ''ConfigValue

configValueJsonOptions :: JSON.Options
configValueJsonOptions =
  JSON.defaultOptions
    { JSON.fieldLabelModifier =
        lowerHead . maybe "" id . stripPrefix "_cv"
    }
 where
  lowerHead [] = []
  lowerHead (x : xs) = toLower x : xs

instance JSON.FromJSON ConfigValue where
  parseJSON = JSON.genericParseJSON configValueJsonOptions

instance JSON.ToJSON ConfigValue where
  toJSON = JSON.genericToJSON configValueJsonOptions
  toEncoding = JSON.genericToEncoding configValueJsonOptions
