{- | A module that provides data types and functions to determine
the terminal environment.
It checks which image format to use based on the terminal type.
-}
module Compat.Term (
  ImageFormat (..),
  TermType (..),
  deduceTerminalType,
  deduceTerminalColorMode,
  deduceFormat,
  isOutOfBandFormat,
  formatArg,
  -- Raw terminal helpers
  emitBytes,
  moveCursor,
  saveCursor,
  restoreCursor,
)
where

import Attrs (ColorMode (CMDark, CMLight))
import Brick
import Control.Exception (IOException, bracket, bracket_, try)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Char (isHexDigit)
import Data.List
import Data.Maybe
import Data.Ord (comparing)
import Graphics.Vty.Output qualified as Output
import Numeric (readHex)
import System.Environment (lookupEnv)
import System.Posix.IO (OpenMode (ReadWrite), closeFd, defaultFileFlags, openFd)
import System.Posix.IO.ByteString qualified as PosixBS
import System.Posix.Terminal (
  TerminalMode (EnableEcho, ProcessInput),
  TerminalState (Immediately),
  getTerminalAttributes,
  setTerminalAttributes,
  withMinInput,
  withoutMode,
 )
import System.Timeout (timeout)
import Text.Read (readMaybe)

{- | A data type to represent different terminal types.
This is mainly used to determine which image format to use.
-}
data TermType
  = Tmux
  | GNUScreen
  | Zellj
  | KittyTerm
  | Foot
  | ITerm2
  | MLTerm
  | WezTerm
  | Alaacritty
  | Ghostty
  | Konsole
  | Gnome
  | Tilix
  | XTerm
  | Unknown
  deriving (Eq, Show)

-- | A data type to represent different image formats
data ImageFormat
  = Kitty
  | Sixel
  | ITerm
  | Symbols
  deriving (Eq, Ord, Show)

{- | Determine which image format to use based on the terminal type.
In the current implementation, high-res formats are supported only
for kitty and foot terminals. For other terminals, symbols are
used to paint the image.
-}
deduceFormat :: TermType -> ImageFormat
deduceFormat t
  | t `elem` [KittyTerm, Ghostty] = Kitty
  | t `elem` [WezTerm, Foot, MLTerm, Konsole, Zellj] = Sixel
  | t == ITerm2 = ITerm
  | otherwise = Symbols

-- | Check if the format is out-of-band.
isOutOfBandFormat :: ImageFormat -> Bool
isOutOfBandFormat Symbols = False
isOutOfBandFormat _ = True

-- | Get the argument for the image format, used by chafa.
formatArg :: ImageFormat -> String
formatArg Compat.Term.Kitty = "kitty"
formatArg Sixel = "sixel"
formatArg ITerm = "iterm"
formatArg Symbols = "symbols"

{- | Determine the terminal type heuristically by checking environ-
ment variables. If no match is found, 'Unknown' is returned.
-}
deduceTerminalType :: IO TermType
deduceTerminalType =
  fromMaybe Unknown . selectMost . catMaybes
    <$> sequence
      [ lookupEnv "TMUX" &&> Tmux
      , lookupEnv "STY" &&> GNUScreen
      , lookupEnv "ZELlj" &&> Zellj
      , lookupEnv "KITTY_WINDOW_ID" &&> KittyTerm
      , assertEnv "TERM" "foot" &&> Foot
      , assertEnv "TERM_PROGRAM" "iTerm.app" &&> ITerm2
      , assertEnv "TERM" "mlterm" &&> MLTerm
      , lookupEnv "WEZTERM_PANE" &&> WezTerm
      , lookupEnv "ALACRITTY_WINDOW_ID" &&> Alaacritty
      , lookupEnv "GHOSTTY_RESOURCE_DIR" &&> Ghostty
      , lookupEnv "KONSOLE_VERSION" &&> Konsole
      , lookupEnv "GNOME_TERMINAL_SCREEN" &&> Gnome
      , lookupEnv "TILIX_ID" &&> Tilix
      , lookupEnv "XTERM_VERSION" &&> XTerm
      ]
 where
  m &&> v = fmap (fmap (const v)) m

  assertEnv env val = do
    v <- lookupEnv env
    pure $
      if v == Just val
        then v
        else Nothing

  -- Pick the terminal type with the most evidence from the
  -- environment variables
  selectMost [] = Nothing
  selectMost xs = listToMaybe $ maximumBy (comparing length) $ group xs

{- | Determine whether the terminal is using a light or dark background.

The primary mechanism is OSC 11, which asks the terminal for its actual
background colour. A terminal that does not support the query, is not
attached to a TTY, or does not answer within 200ms falls back to the
conventional @COLORFGBG@ environment variable. If neither source is useful,
meloid uses its default dark appearance.
-}
deduceTerminalColorMode :: IO ColorMode
deduceTerminalColorMode = do
  queriedColor <- try queryTerminalBackground :: IO (Either IOException (Maybe ColorMode))
  case queriedColor of
    Right (Just colorMode) -> pure colorMode
    _ -> maybe CMDark colorModeFromColorFGBG <$> lookupEnv "COLORFGBG"
 where
  -- OSC 11 reports an RGB colour as, for example,
  -- "ESC ] 11 ; rgb:0000/0000/0000 ESC \\".
  queryTerminalBackground =
    bracket
      (openFd "/dev/tty" ReadWrite defaultFileFlags)
      closeFd
      (\terminal -> do
          originalAttributes <- getTerminalAttributes terminal
          let rawAttributes =
                withMinInput
                  ( withoutMode
                      (withoutMode originalAttributes ProcessInput)
                      EnableEcho
                  )
                  1
          bracket_
            (setTerminalAttributes terminal rawAttributes Immediately)
            (setTerminalAttributes terminal originalAttributes Immediately)
            (do
                _ <- PosixBS.fdWrite terminal (BS8.pack "\ESC]11;?\ESC\\")
                response <- timeout 200000 (readOscResponse terminal)
                pure (response >>= colorModeFromOsc11)
            )
      )

  readOscResponse terminal = go 0 ""
   where
    maxResponseLength :: Int
    maxResponseLength = 128

    go :: Int -> String -> IO String
    go lengthSoFar response
      | lengthSoFar >= maxResponseLength = pure response
      | otherwise = do
          chunk <- PosixBS.fdRead terminal 1
          let response' = response <> BS8.unpack chunk
          if BS.null chunk || BS8.elem '\BEL' chunk || "\ESC\\" `isSuffixOf` response'
            then pure response'
            else go (lengthSoFar + BS.length chunk) response'

  colorModeFromOsc11 :: String -> Maybe ColorMode
  colorModeFromOsc11 response = do
    rgb <- stripPrefix "\ESC]11;rgb:" response
    [red, green, blue] <- mapM parseChannel (take 3 (splitOn '/' rgb))
    let luminance = 0.2126 * red + 0.7152 * green + 0.0722 * blue
    pure $ if luminance > 0.5 then CMLight else CMDark

  parseChannel :: String -> Maybe Double
  parseChannel channel = do
    let digits = takeWhile isHexDigit channel
    if null digits
      then Nothing
      else do
        (value, "") <- listToMaybe (readHex digits :: [(Integer, String)])
        let maxValue :: Integer
            maxValue = 16 ^ length digits - 1
        pure (fromIntegral value / fromIntegral maxValue)

  colorModeFromColorFGBG colorFGBG =
    case lastMay (splitOn ';' colorFGBG) >>= readMaybe of
      -- The standard ANSI palette reserves 0--6 for dark colours and
      -- 7--15 for light/bright colours. Backgrounds outside that range
      -- are normally 256-colour palette entries; treating them as dark is
      -- the conservative fallback.
      Just backgroundIndex
        | backgroundIndex >= (7 :: Int) && backgroundIndex <= 15 -> CMLight
      _ -> CMDark

  lastMay [] = Nothing
  lastMay xs = Just (last xs)

  splitOn _ [] = []
  splitOn separator value =
    let (part, rest) = break (== separator) value
     in part : case rest of
          [] -> []
          _ : remaining -> splitOn separator remaining

emitBytes :: Output.Output -> BS.ByteString -> IO ()
emitBytes output =
  Output.outputByteBuffer output

moveCursor :: Location -> BS.ByteString
moveCursor (Location (x, y)) =
  BS8.pack $ "\ESC[" <> show (y + 1) <> ";" <> show (x + 1) <> "H"

saveCursor :: BS.ByteString
saveCursor = BS8.pack "\ESC7"

restoreCursor :: BS.ByteString
restoreCursor = BS8.pack "\ESC8"
