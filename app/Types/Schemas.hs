{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | This model provides serializable structures for
configuring the application. It also provides parsing and
serialization helpers.
-}
module Types.Schemas (
  ToString (..),
  FromString (..),
  ConfigValue (..),
  EQConfigValue (..),
  EQBand (..),
  EQFilterType (..),
  cvShowWelcome,
  cvColorMode,
  cvEq,
  eqPreampDb,
  eqBands,
  eqBandFilterType,
  eqBandFrequencyHz,
  eqBandGainDb,
  eqBandQ,
) where

import Control.Monad (when)
import Data.Aeson qualified as JSON
import Data.ByteString.UTF8 qualified as UTF8
import Data.Char (toLower)
import Data.List (dropWhileEnd, stripPrefix)
import Data.Void (Void)
import Data.Yaml qualified as YAML
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (Lift)
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Numeric (showFFloat)
import Text.Megaparsec (
  Parsec,
  eof,
  errorBundlePretty,
  many,
  optional,
  parse,
  try,
  (<|>),
 )
import Text.Megaparsec.Char (eol, string)
import Text.Megaparsec.Char.Lexer qualified as L

class ToString a where
  toString :: a -> String

class FromString a where
  fromString :: String -> Either String a

-- | User-editable configuration loaded from the YAML file.
data ConfigValue = ConfigValue
  { _cvShowWelcome :: Bool
  , _cvColorMode :: String
  , _cvEq :: String
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

instance ToString ConfigValue where
  toString = UTF8.toString . YAML.encode

instance FromString ConfigValue where
  fromString input =
    case YAML.decodeEither' (UTF8.fromString input) of
      Left err -> Left (YAML.prettyPrintParseException err)
      Right value -> Right value

data EQConfigValue = EQConfigValue
  { _eqPreampDb :: Double
  , _eqBands :: [EQBand]
  }
  deriving (Eq, Show)

data EQBand = EQBand
  { _eqBandFilterType :: EQFilterType
  , _eqBandFrequencyHz :: Double
  , _eqBandGainDb :: Double
  , _eqBandQ :: Double
  }
  deriving (Eq, Show)

data EQFilterType
  = EQPeak
  | EQLowShelf
  | EQHighShelf
  deriving (Eq, Show)

makeLenses ''EQConfigValue
makeLenses ''EQBand

type EQParser = Parsec Void String

instance ToString EQConfigValue where
  toString = renderEQConfig

instance FromString EQConfigValue where
  fromString = parseEQConfig

-- | Parse an EQ configuration from a string.
parseEQConfig :: String -> Either String EQConfigValue
parseEQConfig input =
  case parse eqConfigParser "PipeWire EQ" input of
    Left err -> Left (errorBundlePretty err)
    Right value -> Right value

-- | Render an EQ configuration to a string.
renderEQConfig :: EQConfigValue -> String
renderEQConfig config =
  unlinesWithoutTrailingNewline $
    renderPreampLine (config ^. eqPreampDb)
      : zipWith renderBandLine [1 :: Int ..] (config ^. eqBands)

eqConfigParser :: EQParser EQConfigValue
eqConfigParser = do
  preampDb <- preampLineParser
  indexedBands <- many (eol *> eqBandLineParser)
  _ <- optional eol
  eof
  let actualIndexes = map fst indexedBands
      expectedIndexes = [1 .. length indexedBands]
  when
    (actualIndexes /= expectedIndexes)
    (fail "Filter numbering must be contiguous and start at 1")
  pure (EQConfigValue preampDb (map snd indexedBands))

preampLineParser :: EQParser Double
preampLineParser = do
  _ <- string "Preamp: "
  value <- signedDoubleParser
  _ <- string " dB"
  pure value

eqBandLineParser :: EQParser (Int, EQBand)
eqBandLineParser = do
  _ <- string "Filter "
  index <- L.decimal
  _ <- string ": ON "
  filterType <- eqFilterTypeParser
  _ <- string " Fc "
  frequencyHz <- signedDoubleParser
  _ <- string " Hz Gain "
  gainDb <- signedDoubleParser
  _ <- string " dB Q "
  qValue <- signedDoubleParser
  pure
    ( index
    , EQBand
        { _eqBandFilterType = filterType
        , _eqBandFrequencyHz = frequencyHz
        , _eqBandGainDb = gainDb
        , _eqBandQ = qValue
        }
    )

eqFilterTypeParser :: EQParser EQFilterType
eqFilterTypeParser =
  (string "PK" *> pure EQPeak)
    <|> (string "LSC" *> pure EQLowShelf)
    <|> (string "HSC" *> pure EQHighShelf)

signedDoubleParser :: EQParser Double
signedDoubleParser =
  L.signed (pure ()) (try L.float <|> (fromInteger <$> L.decimal))

renderPreampLine :: Double -> String
renderPreampLine preampDb =
  "Preamp: " <> formatCanonicalNumber preampDb <> " dB"

renderBandLine :: Int -> EQBand -> String
renderBandLine index band =
  "Filter "
    <> show index
    <> ": ON "
    <> renderEQFilterType (band ^. eqBandFilterType)
    <> " Fc "
    <> formatCanonicalNumber (band ^. eqBandFrequencyHz)
    <> " Hz Gain "
    <> formatCanonicalNumber (band ^. eqBandGainDb)
    <> " dB Q "
    <> showFFloat (Just 3) (band ^. eqBandQ) ""

renderEQFilterType :: EQFilterType -> String
renderEQFilterType EQPeak = "PK"
renderEQFilterType EQLowShelf = "LSC"
renderEQFilterType EQHighShelf = "HSC"

formatCanonicalNumber :: Double -> String
formatCanonicalNumber value =
  case stripTrailingZeros (showFFloat Nothing value "") of
    "-0" -> "0"
    normalized -> normalized

stripTrailingZeros :: String -> String
stripTrailingZeros value
  | '.' `elem` value =
      let trimmedZeros = dropWhileEnd (== '0') value
       in case reverse trimmedZeros of
            '.' : rest -> reverse rest
            _ -> trimmedZeros
  | otherwise = value

unlinesWithoutTrailingNewline :: [String] -> String
unlinesWithoutTrailingNewline [] = ""
unlinesWithoutTrailingNewline lines' = foldr1 (\a b -> a <> "\n" <> b) lines'
