{-# LANGUAGE TemplateHaskell #-}

-- | PipeWire equalizer configuration.
module Types.Schemas.EQConfig (
  EQConfigValue (..),
  EQBand (..),
  EQFilterType (..),
  eqPreampDb,
  eqBands,
  eqBandFilterType,
  eqBandFrequencyHz,
  eqBandGainDb,
  eqBandQ,
) where

import Control.Monad (when)
import Data.List (dropWhileEnd)
import Data.Void (Void)
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Numeric (showFFloat)
import Text.Megaparsec (Parsec, eof, errorBundlePretty, many, optional, parse, try, (<|>))
import Text.Megaparsec.Char (eol, string)
import Text.Megaparsec.Char.Lexer qualified as L
import Types.Schemas.Config (FromString (..), ToString (..))

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

parseEQConfig :: String -> Either String EQConfigValue
parseEQConfig input =
  case parse eqConfigParser "PipeWire EQ" input of
    Left err -> Left (errorBundlePretty err)
    Right value -> Right value

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
signedDoubleParser = L.signed (pure ()) (try L.float <|> (fromInteger <$> L.decimal))

renderPreampLine :: Double -> String
renderPreampLine preampDb = "Preamp: " <> formatCanonicalNumber preampDb <> " dB"

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
