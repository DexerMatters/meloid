{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The entry point of the program
module Main (main) where

import Attrs (defaultTheme)
import Brick qualified as B
import Brick.BChan
import Brick.Main as M
import Brick.Themes qualified as T
import Brick.Types (
  Widget,
 )
import Brick.Widgets.Edit qualified as E
import Compat.Image qualified as Image
import Compat.Term qualified as Term
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Monad.State (execState)
import Data.Map qualified as Map
import Data.Vector qualified as Vec
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform qualified as Vty
import Handle
import Lens.Micro ((^.))
import Lens.Micro.Mtl
import Sys qualified
import Types
import Widgets (MenuList (MenuList))
import Widgets.Edits

{- | Draw the UI into three layers:
1. The menu list
2. The dialog view
3. The current view
-}
drawUI :: St -> [Widget (MName St)]
drawUI st =
  drawNamed st MenuList
    : maybe [] (pure . drawNamed st) (st ^. stDialogView)
      <> maybe [] (pure . drawNamed st) (st ^. stCurrentView)

app :: BChan Event -> Image.ImageService -> B.AttrMap -> M.App St Event (MName St)
app chan imageService attrMap =
  M.App
    { M.appDraw = drawUI
    , M.appStartEvent = handleStartEvent
    , M.appChooseCursor = M.showFirstCursor
    , M.appAttrMap = const attrMap
    , M.appHandleEvent = handleEvent chan imageService
    }

main :: IO ()
main = do
  -- Event channel
  chan <- newBChan 2048
  -- Request channel (send requests to the MPD backend)
  requestChan <- newBChan 2048
  -- Image service (the backend for album art)
  imageService <- Image.startImageService chan
  void $ forkIO $ Sys.musicPlayerThread requestChan chan

  -- Make a Vty interface with mouse and image overlay support
  let mkVty = do
        vty <- Vty.mkVty V.defaultConfig
        V.setMode (V.outputIface vty) V.Mouse True
        pure $ Image.wrapVty imageService vty
  vty <- mkVty

  -- Initialize the state
  let st = flip execState defaultSt $ do
        stChannel .= Just requestChan
        stCurrentView .= Just MainView
        stLastView .= Just MainView
  void $
    M.customMain
      vty
      mkVty
      (Just chan)
      (app chan imageService (T.themeToAttrMap defaultTheme))
      st

-- | The initial state
defaultSt :: St
defaultSt =
  St
    { _stEdits =
        EditSt
          { _esCommand = (E.editor (mName CommandEditor) Nothing "")
          }
    , _stInlineOutput = (Info, "")
    , _stPressed = Nothing
    , _stCommandStages =
        CommandProceedingSt
          { _cpsCommand = ""
          , _cpsStages = Nothing
          , _cpsParameters = []
          }
    , _stSongProgressPreview = Nothing
    , _stLastRightPressed = Nothing
    , _stCurrentView = Nothing
    , _stLastView = Nothing
    , _stDialog = Nothing
    , _stDialogView = Nothing
    , _stMenu = Nothing
    , _stMode = NormalMode
    , _stSelectedAlbum = Nothing
    , _stSelectedPlaylist = 0
    , _stConfig =
        ConfigSt
          { _csVolume = 0
          , _csMusicDir = ""
          , _csAllPlaylists = Vec.empty
          , _csAllDirs = Vec.empty
          , _csAllAlbums = Vec.empty
          }
    , _stPlaying =
        PlayingSt
          { _psCurrentSong = Nothing
          , _psCurrentTime = Nothing
          , _psCurrentQueue = Vec.empty
          , _psPaused = False
          }
    , _stLogs = []
    , _stChannel = Nothing
    , _stPicCache = Map.empty
    , _stPanic = False
    , _stEnv =
        Environment
          { _envTermType = Term.Unknown
          , _envImageFormat = Term.Symbols
          }
    }
