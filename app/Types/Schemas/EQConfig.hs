{-# LANGUAGE TemplateHaskell #-}

-- | Fixed-band EQ presets stored as one gain value per line.
module Types.Schemas.EQConfig (
  EQConfigValue (..),
  EQConfigSpecs (..),
  eqGains,
  eqFrequencies,
  eqGainLimitDb,
  validateEQGains,
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

-- | Maximum gain accepted from both stored presets and the native DSP.
eqGainLimitDb :: Double
eqGainLimitDb = 20

validateEQGains :: [Double] -> Either String ()
validateEQGains gains
  | length gains /= length eqFrequencies =
      Left $ "Expected " <> show (length eqFrequencies) <> " EQ gains, one per line"
  | any (\gain -> isNaN gain || isInfinite gain) gains =
      Left "EQ gains must be finite numbers"
  | any ((> eqGainLimitDb) . abs) gains =
      Left $ "EQ gains must be between -" <> show eqGainLimitDb <> " and +" <> show eqGainLimitDb <> " dB"
  | otherwise = Right ()

instance ToString EQConfigSpecs where
  toString (EQConfigSpecs gains) = intercalate "\n" $ show <$> gains

instance FromString EQConfigSpecs where
  fromString input =
    case traverse readMaybe (lines input) of
      Just gains -> validateEQGains gains >> Right (EQConfigSpecs gains)
      Nothing -> Left "Every EQ gain must be a number"
