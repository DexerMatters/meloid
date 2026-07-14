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
import Data.Set qualified as Set
import Data.Vector qualified as Vec
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform qualified as Vty
import Handle
import Lens.Micro.Mtl
import Sys qualified
import Types
import Widgets qualified as Names
import Widgets.Edits

-- | Draw the active top-level layers from top to bottom.
drawUI :: St -> [Widget (MName St)]
drawUI st =
  map (drawMName st) (Names.activeLayerNames st)

app :: Image.ImageService -> B.AttrMap -> M.App St Event (MName St)
app imageService attrMap =
  M.App
    { M.appDraw = drawUI
    , M.appStartEvent = handleStartEvent
    , M.appChooseCursor = M.showFirstCursor
    , M.appAttrMap = const attrMap
    , M.appHandleEvent = handleEvent imageService
    }

main :: IO ()
main = do
  -- Event channel
  chan <- newBChan 2048
  -- Request channel (send requests to the MPD backend)
  requestChan <- newBChan 2048
  -- Image service (the terminal image backend)
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
  _finalSt <-
    M.customMain
      vty
      mkVty
      (Just chan)
      (app imageService (T.themeToAttrMap defaultTheme))
      st
  return ()

-- | The initial state
defaultSt :: St
defaultSt =
  St
    { _stEdits =
        EditSt
          { _esCommand = (E.editor (mName CommandEditor) Nothing "")
          }
    , _stPressed = Nothing
    , _stLastLeftClick = Nothing
    , _stTriggeredNames = Set.empty
    , _stTabStates = Map.empty
    , _stSongProgressPreview = Nothing
    , _stLastRightPressed = Nothing
    , _stCurrentView = Nothing
    , _stLastView = Nothing
    , _stDialog = Nothing
    , _stDialogView = Nothing
    , _stMenu = MenuSt [] placeholderName
    , _stMode = NormalMode
    , _stSelectedAlbum = Nothing
    , _stSelectedPlaylist = 0
    , _stSelectedSong = Nothing
    , _stConfig =
        ConfigSt
          { _csVolume = 0
          , _csMusicDir = ""
          , _csAllPlaylists = Vec.empty
          , _csAllDirs = Vec.empty
          , _csAllAlbums = Vec.empty
          , _csConfigs =
              ConfigValue
                { _cvColorMode = "auto"
                , _cvShowWelcome = True
                , _cvEq = "default"
                , _cvLayout = placeholderLayout
                }
          , _csEQConfigs = Map.empty
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
    , _stImageCache = Map.empty
    , _stLayoutResize = Nothing
    , _stPanic = False
    , _stEnv =
        Environment
          { _envTermType = Term.Unknown
          , _envImageFormat = Term.Symbols
          }
    }
