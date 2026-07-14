-- | MPD directives needed by Meloid from an MPD configuration file.
module Types.Schemas.MPDConfig (
  MPDConfig (..),
  parseMPDConfig,
) where

import Data.Char (isSpace)
import Data.Maybe (mapMaybe)
import Utils

data MPDConfig = MPDConfig
  { mpdMusicDirectory :: Maybe FilePath
  , mpdIncludes :: [FilePath]
  }
  deriving (Eq, Show)

-- | Parse the directives Meloid needs while ignoring unrelated MPD settings.
parseMPDConfig :: String -> MPDConfig
parseMPDConfig = foldr addDirective (MPDConfig Nothing []) . mapMaybe parseDirective . lines
 where
  addDirective ("music_directory", value) config =
    config{mpdMusicDirectory = Just value}
  addDirective ("include", value) config =
    config{mpdIncludes = value : mpdIncludes config}
  addDirective _ config = config

parseDirective :: String -> Maybe (String, String)
parseDirective rawLine =
  case break isSpace $ trim (stripComment rawLine) of
    ("", _) -> Nothing
    (key, rest) -> Just (key, unquote $ trim rest)

stripComment :: String -> String
stripComment = go False
 where
  go _ [] = []
  go inQuote ('"' : xs) = '"' : go (not inQuote) xs
  go False ('#' : _) = []
  go inQuote (x : xs) = x : go inQuote xs
