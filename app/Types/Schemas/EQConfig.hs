{-# LANGUAGE TemplateHaskell #-}

-- | Fixed-band EQ presets stored as one gain value per line.
module Types.Schemas.EQConfig (
  EQConfigValue (..),
  EQConfigSpecs (..),
  eqGains,
  eqFrequencies,
) where

import Data.List (intercalate)
import Data.Map qualified as Map
import Lens.Micro.TH (makeLenses)
import Text.Read (readMaybe)
import Types.Schemas.Config (FromString (..), ToString (..))

data EQConfigValue = EQConfigValue (Map.Map String EQConfigSpecs)
  deriving (Eq, Show)

newtype EQConfigSpecs = EQConfigSpecs
  { _eqGains :: [Double]
  }
  deriving (Eq, Show)

makeLenses ''EQConfigSpecs

-- | The fixed frequencies shared by the editor, curve, and PipeWire graph.
eqFrequencies :: [Double]
eqFrequencies = [55, 77, 110, 156, 220, 311, 440, 622, 880, 1200, 1800, 2500, 3500, 5000, 7000, 10000, 14000, 20000]

instance ToString EQConfigSpecs where
  toString (EQConfigSpecs gains) = intercalate "\n" $ show <$> gains

instance FromString EQConfigSpecs where
  fromString input =
    case traverse readMaybe (lines input) of
      Just gains
        | length gains == length eqFrequencies -> Right $ EQConfigSpecs gains
      _ -> Left $ "Expected " <> show (length eqFrequencies) <> " EQ gains, one per line"
