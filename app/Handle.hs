{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{- | This module is responsible for handling various events
Events are basically classified into three categories:

- Global events: events that affect the entire application
- Local events: events that affect a specific widget
- App events: custom events defined by the application
-}
module Handle (
  handleEvent,
  handleStartEvent,
) where

import Brick qualified as B
import Brick qualified as W
import Brick.Main as M
import Brick.Types (
  BrickEvent (..),
  EventM,
 )
import Brick.Widgets.Edit qualified as E
import Cmd (execCmd)
import Compat.Image qualified as Image
import Compat.Markdown (parseMarkdownText)
import Compat.Term qualified as Term
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get)
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.List (find)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Data.Text qualified as Text
import Data.Text.Zipper qualified as TZ
import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)
import Graphics.Vty qualified as V
import Language.Haskell.TH.Syntax (Lift (lift), addDependentFile, runIO)
import Lens.Micro
import Lens.Micro.Mtl
import Network.MPD qualified as MPD
import System.FilePath ((</>))
import Types
import Types.Configs (Configs (..), saveWithPanic)
import Utils (split)
import Widgets.Focus (dismissKeyboardFocus, handleFocusEvent, reconcileFocus)
import Widgets.Image (imageScene)
import Widgets.Layer (activeOccluderNames)
import Widgets.Lists (MenuEntry (..))

-- | The entrance point for handling events
handleEvent :: Image.ImageService -> BrickEvent (MName St) Event -> EventM (MName St) St ()
handleEvent imageService ev = do
  dismissFocusForPointer ev
  handled <- handleGlobalEvent imageService ev
  unless handled $
    handleFocusEvent ev >>= \focusHandled ->
      unless focusHandled $
        use stMode >>= \case
          CommandMode ->
            zoom (stEdits . esCommand) $ E.handleEditorEvent ev
          _ ->
            handleEvent' imageService ev
  reconcileFocus
  -- Geometry can change without a widget-specific mouse handler, for example
  -- when a command opens a dialog. Refresh after the following Brick draw.
  case ev of
    AppEvent _ -> pure ()
    _ -> queueMainViewRefresh imageService

{- | Pointer input leaves keyboard navigation without stealing the ring for
the clicked widget. Blank terminal cells arrive as raw Vty mouse events.
-}
dismissFocusForPointer :: BrickEvent (MName St) Event -> EventM (MName St) St ()
dismissFocusForPointer = \case
  MouseDown{} -> dismissKeyboardFocus
  MouseUp{} -> dismissKeyboardFocus
  VtyEvent V.EvMouseDown{} -> dismissKeyboardFocus
  _ -> pure ()

{- | The function that handles the local events. This handles
events defined in the type class `Drawable`
-}
handleEvent' :: Image.ImageService -> BrickEvent (MName St) Event -> EventM (MName St) St ()
handleEvent' imageService = \case
  MouseDown name V.BScrollDown modifiers _ ->
    dismissMenuOutside name >>= \dismissed ->
      unless dismissed . void $
        dispatchToFirst name $
          if V.MCtrl `elem` modifiers
            then named onMouseScrollDown'
            else named onMouseScrollDown
  MouseDown name V.BScrollUp modifiers _ ->
    dismissMenuOutside name >>= \dismissed ->
      unless dismissed . void $
        dispatchToFirst name $
          if V.MCtrl `elem` modifiers
            then named onMouseScrollUp'
            else named onMouseScrollUp
  MouseDown name V.BLeft _ location ->
    dismissMenuOutside name >>= \dismissed ->
      unless dismissed $ handleLeftMouseDown name location
  MouseUp name (Just V.BLeft) location ->
    handleLeftMouseUp name location
  MouseUp name (Just V.BRight) location ->
    dismissMenuOutside name >>= \dismissed ->
      unless dismissed $ handleRightMouseUp name location
  AppEvent appEvent ->
    handleAppEvent imageService appEvent
  _ ->
    pure ()

{- | The function that handles the global events. When it
returns true, it consumes the event without passing it
to the local or app event handlers
-}
handleGlobalEvent :: Image.ImageService -> BrickEvent (MName St) Event -> EventM (MName St) St Bool
handleGlobalEvent imageService = \case
  -- Blank cells in a relative popup remain raw Vty mouse events.
  VtyEvent V.EvMouseDown{} -> dismissOpenMenu
  -- Ctrl + C to quit
  VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl]) ->
    handleQuitShortcut $> True
  -- Trigger when resize
  VtyEvent (V.EvResize _ _) -> do
    repositionMenu
    queueMainViewRefresh imageService $> True
  -- Toggle debug view
  VtyEvent (V.EvKey (V.KChar 'd') [V.MCtrl]) -> do
    closeMenu >> toggleDebugView imageService $> True
  -- Toggle modes. See `Mode` for details
  VtyEvent (V.EvKey (V.KChar '`') []) ->
    closeMenu >> clearCommandEdit >> switchMode $> True
  -- Submit command only while its editor owns input.  In normal mode Enter is
  -- routed to the keyboard-focus controller instead.
  VtyEvent (V.EvKey V.KEnter []) ->
    (,) <$> use stMode <*> use stDialog >>= \case
      (CommandMode, Nothing) -> do
        menuOpen <- use (stMenu . msWidgets . to (not . null))
        if menuOpen
          then pure False
          else submitCommandEdit >> clearCommandEdit $> True
      _ -> pure False
  _ ->
    pure False
 where
  handleQuitShortcut =
    use stPanic >>= \case
      True -> M.halt
      False ->
        use stDialog >>= \case
          Just _ -> pure ()
          Nothing ->
            use (stUnsaved . usLayout) >>= \case
              True -> do
                closeMenu
                openSimpleDialog
                  "Save layout?"
                  ( W.strWrap
                      "You have unsaved layout changes. Save them before quitting?"
                  )
                  quitWithoutSaving
                  saveLayoutAndQuit
              False -> requestQuit

  quitWithoutSaving = stUnsaved . usLayout .= False >> requestQuit

  saveLayoutAndQuit = do
    config <- use (stConfig . csConfigs)
    saveWithPanic Configs config >>= \case
      True -> stUnsaved . usLayout .= False >> requestQuit
      False -> pure ()

  requestQuit = sendRequest SignalQuit

  submitCommandEdit =
    use (stEdits . esCommand . to (TZ.currentLine . E.editContents)) >>= execCmd

  clearCommandEdit =
    stEdits . esCommand %= E.applyEdit (const (TZ.stringZipper [] Nothing))

{- | Close an open menu for a pointer event outside the popup and consume that
event so it cannot also activate the underlying widget.
-}
dismissMenuOutside :: MName St -> EventM (MName St) St Bool
dismissMenuOutside name
  | isJust (castMName name :: Maybe MenuEntry) = pure False
  | otherwise = dismissOpenMenu

-- | Close the menu if present and report whether the event was consumed.
dismissOpenMenu :: EventM (MName St) St Bool
dismissOpenMenu = do
  isOpen <- use (stMenu . msWidgets . to (not . null))
  if isOpen
    then closeMenu $> True
    else pure False

-- | The function that handles the app events.
handleAppEvent :: Image.ImageService -> Event -> EventM (MName St) St ()
handleAppEvent imageService = \case
  -- Log events. Logs are printed to the `DebugViewport`
  Log entry -> do
    when (fst entry == Error) $
      panic >> switchViewAndSyncImages imageService DebugView
    stLogs %= (entry :)
  -- Refresh images. It is important when images are in a
  -- dynamic widget such like a scrollable viewport
  RefreshImages -> do
    whenMainView $
      refreshImages imageService
  -- For now, we only get the volume from MPD status
  UpdateStatus status ->
    applyPlaybackStatus status
  -- Update the current song.
  UpdateSong song ->
    applyCurrentSong imageService song
  -- Update the current time. (How much time elapsed when
  -- the song is playing)
  UpdateTime dur ->
    stPlaying . psCurrentTime .= dur
  -- Update the current queue state. This updates both the
  -- volume and the current queue.
  UpdateCurrentQueueState status song songs -> do
    applyPlaybackStatus status
    stPlaying . psCurrentQueue .= songs
    applyCurrentSong imageService song
  -- Update the post-EQ PipeWire spectrum without involving MPD state.
  UpdateSpectrum levels ->
    stSpectrum . ssLevels .= levels
  -- Update the config. The config includes things to be
  -- accquired when the program starts
  UpdateConfig config -> do
    stConfig .= config
    use stSelectedPlaylist >>= \case
      Just selected
        | not $ any ((== selected) . playlistName) (config ^. csAllPlaylists) ->
            stSelectedPlaylist .= Nothing
      _ -> pure ()
    stSelectedEQConfig .= Just (config ^. csConfigs . cvEq)
    queueMainViewRefresh imageService
  -- Drain all worker completions in one state update and refresh once.
  ImagesReady -> do
    images <- Image.takeReadyImages imageService
    stImageCache %= Map.union images
    queueMainViewRefresh imageService
  -- Halt
  Halt ->
    M.halt

handleLeftMouseDown :: MName St -> B.Location -> EventM (MName St) St ()
handleLeftMouseDown name location = do
  stPressed .= Just name
  void $
    dispatchToFirst
      name
      (\case MName a -> ($ location) <$> onMouseLeftDown a)

handleLeftMouseUp :: MName St -> B.Location -> EventM (MName St) St ()
handleLeftMouseUp name location = do
  use stPressed >>= \pressed ->
    when (pressed == Just name) $ do
      void $
        dispatchToFirst
          name
          (\case MName a -> ($ location) <$> onMouseLeftUp a)
      now <- liftIO getCurrentTime
      use stLastLeftClick >>= \case
        Just (lastName, lastTime)
          | lastName == name
          , diffUTCTime now lastTime <= doubleClickThreshold -> do
              void $
                dispatchToFirst
                  name
                  (\case MName a -> ($ location) <$> onMouseDoubleClick a)
              stLastLeftClick .= Nothing
        _ ->
          stLastLeftClick .= Just (name, now)
  stSongProgressPreview .= Nothing
  stPressed .= Nothing
  stLayoutResize .= Nothing

handleRightMouseUp :: MName St -> B.Location -> EventM (MName St) St ()
handleRightMouseUp name location = do
  dispatchToFirst
    name
    (\case MName a -> ($ location) <$> onMouseRightUp a)
    >>= mapM_ (\target -> stLastRightPressed .= Just target)

applyCurrentSong :: Image.ImageService -> Maybe MPD.Song -> EventM (MName St) St ()
applyCurrentSong imageService song = do
  stPlaying . psCurrentSong .= song
  queueMainViewRefresh imageService

applyPlaybackStatus :: MPD.Status -> EventM (MName St) St ()
applyPlaybackStatus status = do
  stConfig . csVolume ?.= MPD.stVolume status
  stPlaying . psCurrentTime .= MPD.stTime status
  case MPD.stState status of
    MPD.Playing -> do
      stPlaying . psPaused .= False
      stPlaying . psStopped .= False
    MPD.Paused -> do
      stPlaying . psPaused .= True
      stPlaying . psStopped .= False
    MPD.Stopped -> do
      stPlaying . psPaused .= True
      stPlaying . psStopped .= True

toggleDebugView :: Image.ImageService -> EventM (MName St) St ()
toggleDebugView imageService =
  (,) <$> use stCurrentView <*> use stPanic >>= \case
    (Just currentView, False)
      | currentView == DebugView ->
          use stLastView >>= mapM_ (switchViewAndSyncImages imageService)
    (Just currentView, True)
      | currentView == DebugView ->
          pure ()
    _ ->
      switchViewAndSyncImages imageService DebugView

queueMainViewRefresh :: Image.ImageService -> EventM (MName St) St ()
queueMainViewRefresh imageService =
  whenMainView $ Image.queueRefreshImages imageService

switchViewAndSyncImages :: Image.ImageService -> ViewName -> EventM (MName St) St ()
switchViewAndSyncImages imageService nextView = do
  previousView <- use stCurrentView
  switchView nextView
  currentView <- use stCurrentView
  when (previousView == Just MainView && currentView /= Just MainView) $
    Image.clearScene imageService
  when (currentView == Just MainView) $
    Image.queueRefreshImages imageService

refreshImages :: Image.ImageService -> EventM (MName St) St ()
refreshImages imageService = do
  st <- get
  scene <- imageScene st
  Image.refreshScene imageService scene (activeOccluderNames st)

dispatchToFirst ::
  MName St ->
  (MName St -> Maybe (EventM (MName St) St ())) ->
  EventM (MName St) St (Maybe (MName St))
dispatchToFirst name handler = do
  let target = find (isJust . handler) (nameAncestry name)
  maybe (pure ()) id (target >>= handler)
  pure target

whenMainView :: EventM (MName St) St () -> EventM (MName St) St ()
whenMainView action = do
  currentView <- use stCurrentView
  when (currentView == Just MainView) action

doubleClickThreshold :: NominalDiffTime
doubleClickThreshold = 0.3

(?.=) :: ASetter' St a -> Maybe a -> EventM (MName St) St ()
field ?.= maybeValue =
  for_ maybeValue $ \value -> field .= value

infix 4 ?.=

handleStartEvent :: EventM (MName St) St ()
handleStartEvent = do
  termType <- liftIO Term.deduceTerminalType
  stEnv .= Environment termType (Term.deduceFormat termType)
  sendRequest SignalInit
  sendRequest $ TriggerSpectrum True
  sendRequest GetConfig
  sendRequest SignalCurrentQueue
  sendRequest . LogConfig Info $
    "Terminal environment: \n"
      <> "- Terminal type: "
      <> show termType
      <> "\n"
      <> "- Image format: "
      <> show (Term.deduceFormat termType)
  reconcileFocus

  -- Show the welcome dialog (if any)
  welcome <- use $ stConfig . csConfigs . cvShowWelcome
  let text =
        Text.pack
          <$> split
            "%%"
            $( do
                 let fp = "assets" </> "docs" </> "welcome.md"
                 addDependentFile fp
                 content <- runIO (readFile fp)
                 lift content
             )
  when welcome
    $ openPagedDialog
      "Welcome"
    $ NonEmpty.fromList
    $ parseMarkdownText <$> text
