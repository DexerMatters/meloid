-- | Lossless MPD configuration parsing and rendering.
module Types.Schemas.MPDConfig (
  MPDConfigValue (..),
  MPDConfigLine,
  mpdGet,
  mpdModify,
  parseMPDConfig,
  renderMPDConfig,
) where

import Data.Bifunctor (first)
import Data.Char (isSpace)
import Data.Void (Void)
import Text.Megaparsec (
  Parsec,
  anySingle,
  eof,
  errorBundlePretty,
  many,
  manyTill,
  optional,
  parse,
  satisfy,
  some,
  takeWhileP,
  try,
  (<|>),
 )
import Text.Megaparsec.Char (char, string)
import Types.Schemas.Config (FromString (..), ToString (..))

-- | Every loaded MPD config document, keyed by its source path.
data MPDConfigValue = MPDConfigValue [(FilePath, [MPDConfigLine])]
  deriving (Eq, Show)

data MPDConfigLine
  = MPDDirectiveLine [String] String String String String String
  | MPDTriviaLine String
  deriving (Eq, Show)

-- | Return every value at an MPD configuration path.
mpdGet :: [String] -> MPDConfigValue -> [String]
mpdGet path (MPDConfigValue files) =
  [ value
  | (_, lines') <- files
  , MPDDirectiveLine linePath _ _ _ value _ <- lines'
  , path == linePath
  ]

{- | Apply a change to every value at an MPD configuration path. If the path
is absent, create its last entry inside an existing parent block.
-}
mpdModify :: [String] -> (String -> String) -> MPDConfigValue -> MPDConfigValue
mpdModify [] _ config = config
mpdModify path change (MPDConfigValue files)
  | any (any matches . snd) files = MPDConfigValue $ fmap modifyFile files
  | otherwise = MPDConfigValue $ insert files
 where
  key = last path
  parent = init path
  value = change ""
  newLine = MPDDirectiveLine path (key <> " ") key (renderValue "" value) value "\n"

  matches (MPDDirectiveLine linePath _ _ _ _ _) = path == linePath
  matches _ = False

  modifyFile (file, lines') = (file, map modify lines')

  modify line@(MPDDirectiveLine linePath prefix key' rawValue value' suffix)
    | path == linePath =
        let newValue = change value'
         in if newValue == value'
              then line
              else MPDDirectiveLine linePath prefix key' (renderValue rawValue newValue) newValue suffix
  modify line = line

  insert [] = []
  insert ((file, lines') : rest)
    | null parent = (file, append lines') : rest
    | otherwise =
        case beforeClosing lines' of
          Just lines'' -> (file, lines'') : rest
          Nothing -> (file, lines') : insert rest

  append [] = [newLine]
  append lines'
    | endsWithLineEnding (last lines') = lines' <> [newLine]
    | otherwise = lines' <> [MPDTriviaLine "\n", newLine]

  beforeClosing [] = Nothing
  beforeClosing (line@(MPDDirectiveLine linePath _ "}" "" _ _) : rest)
    | linePath == parent = Just (newLine : line : rest)
  beforeClosing (line : rest) = (line :) <$> beforeClosing rest

  endsWithLineEnding (MPDDirectiveLine _ _ _ _ _ suffix) = not (null suffix) && last suffix `elem` ['\n', '\r']
  endsWithLineEnding (MPDTriviaLine source) = not (null source) && last source `elem` ['\n', '\r']

  renderValue ('"' : _) value' = quoted value'
  renderValue _ value'
    | null value' || any (\c -> isSpace c || c == '#') value' = quoted value'
    | otherwise = value'

  quoted value' = '"' : concatMap escape value' <> "\""
  escape '\\' = "\\\\"
  escape '"' = "\\\""
  escape char' = [char']

instance ToString MPDConfigValue where
  toString = renderMPDConfig

instance FromString MPDConfigValue where
  fromString = parseMPDConfig ""

parseMPDConfig :: FilePath -> String -> Either String MPDConfigValue
parseMPDConfig file input = first errorBundlePretty $ parse parser file input
 where
  parser :: Parsec Void String MPDConfigValue
  parser = do
    lines' <- many line
    eof
    pure $ MPDConfigValue [(file, annotate [] lines')]

  line = do
    content <- takeWhileP Nothing (\c -> c /= '\r' && c /= '\n')
    ending <- optional (try (string "\r\n") <|> string "\n" <|> string "\r")
    case ending of
      Nothing | null content -> fail "end of MPD config"
      _ -> pure $ classify content (maybe "" id ending)

  classify content ending =
    case parse directive "MPD config directive" content of
      Left _ -> MPDTriviaLine (content <> ending)
      Right (prefix, key, rawValue, value, suffix) ->
        MPDDirectiveLine [] prefix key rawValue value (suffix <> ending)

  directive :: Parsec Void String (String, String, String, String, String)
  directive = do
    leading <- horizontalSpace
    key <- some (satisfy (\c -> not (isSpace c) && c /= '#'))
    between <- horizontalSpace
    value <-
      try quoted
        <|> ((\raw -> (raw, raw)) <$> some (satisfy (\c -> not (isSpace c) && c /= '#')))
        <|> pure ("", "")
    trailing <- horizontalSpace
    comment <- optional (char '#' *> takeWhileP Nothing (\c -> c /= '\r' && c /= '\n'))
    eof
    pure (leading <> key <> between, key, fst value, snd value, trailing <> maybe "" ("#" <>) comment)
   where
    quoted = do
      _ <- char '"'
      chars <- manyTill quotedCharacter (char '"')
      pure ('"' : concatMap fst chars <> "\"", map snd chars)

    quotedCharacter =
      try (do escaped <- char '\\' *> anySingle; pure ("\\" <> [escaped], escaped))
        <|> ((\char' -> ([char'], char')) <$> satisfy (\char' -> char' /= '"' && char' /= '\r' && char' /= '\n'))

  horizontalSpace = takeWhileP Nothing (\c -> c == ' ' || c == '\t')

  annotate _ [] = []
  annotate path (line' : rest) =
    case line' of
      MPDDirectiveLine _ prefix "}" "" value suffix ->
        MPDDirectiveLine path prefix "}" "" value suffix
          : annotate (reverse $ drop 1 $ reverse path) rest
      MPDDirectiveLine _ prefix key "{" value suffix ->
        MPDDirectiveLine (path <> [key]) prefix key "{" value suffix
          : annotate (path <> [key]) rest
      MPDDirectiveLine _ prefix key rawValue value suffix ->
        MPDDirectiveLine (path <> [key]) prefix key rawValue value suffix
          : annotate path rest
      MPDTriviaLine source -> MPDTriviaLine source : annotate path rest

renderMPDConfig :: MPDConfigValue -> String
renderMPDConfig (MPDConfigValue files) = concatMap (concatMap raw . snd) files
 where
  raw (MPDDirectiveLine _ prefix _ value _ suffix) = prefix <> value <> suffix
  raw (MPDTriviaLine source) = source
