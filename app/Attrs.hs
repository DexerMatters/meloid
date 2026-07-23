{-# LANGUAGE OverloadedStrings #-}

{- | This module provides attributes used in the UI.
Attributes are colors and styles that can be applied to a
widget. It complies with the Brick's theme system.
In the future, the application may support loading custom
themes from files, which is provided by Brick.
-}
module Attrs (
  ColorMode (..),
  defaultTheme,
) where

import Brick
import Brick.Themes qualified as T
import Data.Yaml qualified as YAML
import Graphics.Vty hiding (ColorMode)

data ColorMode = CMLight | CMDark | CMAuto
  deriving (Show, Eq)

instance YAML.ToJSON ColorMode where
  toJSON CMLight = YAML.String "light"
  toJSON CMDark = YAML.String "dark"
  toJSON CMAuto = YAML.String "auto"

instance YAML.FromJSON ColorMode where
  parseJSON (YAML.String "light") = pure CMLight
  parseJSON (YAML.String "dark") = pure CMDark
  parseJSON (YAML.String "auto") = pure CMAuto
  parseJSON _ = fail "Invalid ColorMode"

a :: String -> AttrName
a = attrName

-- The default theme.
defaultTheme :: ColorMode -> T.Theme
defaultTheme mode =
  T.newTheme
    (fg primary)
    [ (a "button", defAttr `withForeColor` primary `withStyle` underline)
    , (a "iconButton", defAttr `withForeColor` primary `withStyle` bold)
    , (a "focus", contrastText `on` focusBackground)
    , (a "button" <> a "pressed", canvas `on` primary)
    , (a "iconButton" <> a "pressed", contrastText `on` accentBackground)
    , (a "focused", primary `on` secondary)
    , (a "unsaved", defAttr `withStyle` italic)
    , (a "dialog", defAttr)
    , (a "header", currentAttr `withForeColor` primary `withStyle` bold)
    , (a "label", contrastText `on` accentBackground)
    , (a "bottomLabel", (contrastText `on` accentBackground) `withStyle` bold)
    , (a "meta", currentAttr `withForeColor` accent `withStyle` italic)
    , (a "text", defAttr `withForeColor` accent)
    , (a "textOnTabs", defAttr `withForeColor` accent `withStyle` underline)
    , (a "scrollBarThumb", currentAttr `withForeColor` accent `withStyle` bold)
    , (a "scrollBarTrack", currentAttr)
    , (a "progressBarIncomplete", canvas `on` secondary)
    , (a "progressBarComplete", primary `on` secondary)
    , -- Equalizer
      (a "eqDefault", currentAttr)
    , (a "eqMuted", fg muted)
    , (a "eqAccent", currentAttr `withForeColor` accent)
    , (a "eqAccentBold", currentAttr `withForeColor` accent `withStyle` bold)
    , (a "eqPrimaryBold", currentAttr `withForeColor` primary `withStyle` bold)
    , -- Spectrum
      (a "spectrumLow", fg accent2)
    , (a "spectrumAccent", currentAttr `withForeColor` accent)
    , (a "spectrumPeak", currentAttr `withForeColor` peak `withStyle` bold)
    , (a "spectrumAxis", fg muted)
    , (a "spectrumLabel", currentAttr `withForeColor` accent `withStyle` bold)
    , -- Log
      (a "debugLog", fg muted)
    , (a "infoLog", fg primary)
    , (a "warnLog", fg warning)
    , (a "errorLog", fg errorColor)
    , -- Markdown
      (a "mkHeader", fg primary `withStyle` bold)
    , (a "mkQuote", fg accent `withStyle` italic)
    , (a "mkStrong", fg accent2 `withStyle` bold)
    ]
 where
  isLight = mode == CMLight

  primary
    | isLight = hex2RGB 0x202124
    | otherwise = white

  canvas
    | isLight = white
    | otherwise = black

  contrastText = black

  secondary
    | isLight = hex2RGB 0xC8C8C8
    | otherwise = hex2RGB 0x6F6F6F

  accent
    | isLight = hex2RGB 0x6750A4
    | otherwise = hex2RGB 0xCCBBCC

  accentBackground
    | isLight = hex2RGB 0xE9DDF7
    | otherwise = hex2RGB 0xCCBBCC

  accent2
    | isLight = hex2RGB 0x4F6F46
    | otherwise = hex2RGB 0xB8CFAF

  focusBackground
    | isLight = hex2RGB 0xC7DFC0
    | otherwise = hex2RGB 0xB8CFAF

  muted
    | isLight = hex2RGB 0x6F6F6F
    | otherwise = brightBlack

  peak
    | isLight = hex2RGB 0xB54708
    | otherwise = brightYellow

  warning
    | isLight = hex2RGB 0x9A6700
    | otherwise = yellow

  errorColor
    | isLight = hex2RGB 0xCF222E
    | otherwise = red

hex2RGB :: Int -> Color
hex2RGB i =
  let r = (i `div` 65536) `mod` 256
      g = (i `div` 256) `mod` 256
      b = i `mod` 256
   in srgbColor r g b
