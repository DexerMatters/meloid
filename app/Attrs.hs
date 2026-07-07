{- | This module provides attributes used in the UI.
Attributes are colors and styles that can be applied to a
widget. It complies with the Brick's theme system.
In the future, the application may support loading custom
themes from files, which is provided by Brick.
-}
module Attrs (
  defaultTheme,
) where

import Brick
import Brick.Themes qualified as T
import Graphics.Vty

a :: String -> AttrName
a = attrName

primary :: Color
primary = white

secondary :: Color
secondary = hex2RGB 0x6F6F6F

accent :: Color
accent = hex2RGB 0xCCBBCC

-- The default theme.
defaultTheme :: T.Theme
defaultTheme =
  T.newTheme
    (fg primary)
    [ (a "button", currentAttr `withForeColor` primary `withStyle` underline)
    , (a "iconButton", currentAttr `withForeColor` primary `withStyle` bold)
    , (a "button" <> a "pressed", black `on` primary)
    , (a "iconButton" <> a "pressed", black `on` accent)
    , (a "focused", white `on` secondary)
    , (a "dialog", primary `on` secondary)
    , (a "header", currentAttr `withForeColor` primary `withStyle` bold)
    , (a "label", black `on` accent)
    , (a "bottomLabel", (black `on` accent) `withStyle` bold)
    , (a "meta", currentAttr `withForeColor` accent `withStyle` italic)
    , (a "text", currentAttr `withForeColor` accent)
    , (a "scrollBarThumb", currentAttr `withForeColor` accent `withStyle` bold)
    , (a "scrollBarTrack", currentAttr)
    , (a "progressBarIncomplete", black `on` secondary)
    , (a "progressBarComplete", primary `on` secondary)
    , -- Log
      (a "debugLog", fg $ brightBlack)
    , (a "infoLog", fg $ white)
    , (a "warnLog", fg $ yellow)
    , (a "errorLog", fg $ red)
    ]

hex2RGB :: Int -> Color
hex2RGB i =
  let r = (i `div` 65536) `mod` 256
      g = (i `div` 256) `mod` 256
      b = i `mod` 256
  in srgbColor r g b


