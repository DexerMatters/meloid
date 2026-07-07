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
import Brick.BChan
import Brick.Main as M
import Brick.Types (
  BrickEvent (..),
  EventM,
 )
import Brick.Widgets.Edit qualified as E
import Cmds
import Compat.Image qualified as Image
import Compat.Term qualified as Term
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.List (find)
import Data.Map qualified as Map
import Graphics.Vty qualified as V
import Lens.Micro
import Lens.Micro.Mtl
import Network.MPD qualified as MPD
import Types

-- | The entrance point for handling events
handleEvent :: BChan Event -> Image.ImageService -> BrickEvent (MName St) Event -> EventM (MName St) St ()
handleEvent chan imageService ev = do
  handled <- handleGlobalEvent chan imageService ev
  unless handled $
    use stMode >>= \case
      CommandMode ->
        zoom (stEdits . esCommand) $ E.handleEditorEvent ev
      _ ->
        handleEvent' chan imageService ev

{- | The function that handles the local events. This handles
events defined in the type class `Drawable`
-}
handleEvent' :: BChan Event -> Image.ImageService -> BrickEvent (MName St) Event -> EventM (MName St) St ()
handleEvent' chan imageService = \case
  MouseDown name V.BScrollDown _ _ ->
    handleScrollMouse chan name (named handlesMouseScrollDown) (named onMouseScrollDown)
  MouseDown name V.BScrollUp _ _ ->
    handleScrollMouse chan name (named handlesMouseScrollUp) (named onMouseScrollUp)
  MouseDown name V.BLeft _ location ->
    handleLeftMouseDown chan name location
  MouseUp name (Just V.BLeft) location ->
    handleLeftMouseUp chan name location
  MouseUp name (Just V.BRight) location ->
    handleRightMouseUp chan name location
  AppEvent appEvent ->
    handleAppEvent chan imageService appEvent
  VtyEvent (V.EvKey key mods) -> do
    mode <- use stMode
    when (mode == NormalMode) $
      handleNormalModeKey key mods
  _ ->
    pure ()

{- | The function that handles the global events. When it
returns true, it consumes the event without passing it
to the local or app event handlers
-}
handleGlobalEvent :: BChan Event -> Image.ImageService -> BrickEvent (MName St) Event -> EventM (MName St) St Bool
handleGlobalEvent chan imageService = \case
  -- Ctrl + C to quit
  VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl]) ->
    sendRequest SignalQuit $> True
  -- Trigger when resize
  VtyEvent (V.EvResize _ _) -> do
    queueMainViewRefresh chan $> True
  -- Toggle debug view
  VtyEvent (V.EvKey (V.KChar 'd') [V.MCtrl]) -> do
    toggleDebugView chan imageService $> True
  -- Toggle modes. See `Mode` for details
  VtyEvent (V.EvKey (V.KChar '`') []) ->
    clearCommandEdit >> switchMode $> True
  -- Escape to return to NormalMode from CommandMode or EditMode
  VtyEvent (V.EvKey V.KEsc []) -> do
    mode <- use stMode
    if mode /= NormalMode
      then do
        stMode .= NormalMode
        clearCommandEdit
        clearCommandStages
        pure True
      else pure False
  -- Colon to enter CommandMode from NormalMode
  VtyEvent (V.EvKey (V.KChar ':') []) -> do
    mode <- use stMode
    if mode == NormalMode
      then do
        stMode .= CommandMode
        clearCommandEdit
        clearCommandStages
        pure True
      else pure False
  -- Submit command
  VtyEvent (V.EvKey V.KEnter []) -> do
    mode <- use stMode
    if mode == CommandMode
      then do
        isProceeding <- use stIsProceedingCmd
        if isProceeding
          then
            nextCommandStage $> True
          else do
            command <- use $ stEditorContent esCommand
            applyCommand command
            clearCommandEdit
            use stCurrentStage >>= \case
              Just (ExecutionStage _) ->
                nextCommandStage $> True
              Just (InputStage _ _ _) ->
                pure True
              _ ->
                pure True
      else pure False
  _ ->
    pure False

-- | The function that handles the app events.
handleAppEvent :: BChan Event -> Image.ImageService -> Event -> EventM (MName St) St ()
handleAppEvent chan imageService = \case
  -- Log events. Logs are printed to the `DebugViewport`
  Log entry -> do
    when (fst entry == Error) $
      panic >> switchViewAndSyncImages chan imageService DebugView
    stLogs %= (entry :)
  InlineOutput output ->
    stInlineOutput .= output
  -- Refresh images. It is important when images are in a
  -- dynamic widget such like a scrollable viewport
  RefreshImages ->
    whenMainView $
      Image.refreshScene imageService
  -- For now, we only get the volume from MPD status
  UpdateStatus status ->
    stConfig . csVolume ?.= MPD.stVolume status
  -- Update the current song.
  UpdateSong song ->
    applyCurrentSong chan song
  -- Update the current time. (How much time elapsed when
  -- the song is playing)
  UpdateTime dur ->
    stPlaying . psCurrentTime .= dur
  -- Update the current queue state. This updates both the
  -- volume and the current queue.
  UpdateCurrentQueueState status song songs -> do
    stConfig . csVolume ?.= MPD.stVolume status
    stPlaying . psCurrentQueue .= songs
    applyCurrentSong chan song
  -- Update the config. The config includes things to be
  -- accquired when the program starts
  UpdateConfig config -> do
    stConfig .= config
    Image.prefetchAlbumArtCatalog imageService (config ^. csAllAlbums)
    queueMainViewRefresh chan
  -- Load album art. The render thread tells the handler
  -- that the image is ready to be displayed
  LoadAlbumArt (key, art) -> do
    stPicCache %= Map.insertWith (flip Map.union) key art
    whenMainView $
      Image.refreshScene imageService
  -- Halt
  Halt ->
    M.halt

handleScrollMouse ::
  BChan Event ->
  MName St ->
  (MName St -> Bool) ->
  (MName St -> EventM (MName St) St ()) ->
  EventM (MName St) St ()
handleScrollMouse chan name supports action = do
  void $ dispatchToFirst name supports action
  queueMainViewRefresh chan

handleLeftMouseDown :: BChan Event -> MName St -> B.Location -> EventM (MName St) St ()
handleLeftMouseDown chan name location = do
  stPressed .= Just name
  void $
    dispatchToFirst
      name
      (named handlesMouseLeftDown)
      (\case MName a -> onMouseLeftDown a location)
  queueMainViewRefresh chan

handleLeftMouseUp :: BChan Event -> MName St -> B.Location -> EventM (MName St) St ()
handleLeftMouseUp chan name location = do
  use stPressed >>= \pressed ->
    when (pressed == Just name) $
      void $
        dispatchToFirst
          name
          (named handlesMouseLeftUp)
          (\case MName a -> onMouseLeftUp a location)
  stSongProgressPreview .= Nothing
  stPressed .= Nothing
  queueMainViewRefresh chan

handleRightMouseUp :: BChan Event -> MName St -> B.Location -> EventM (MName St) St ()
handleRightMouseUp chan name location = do
  dispatchToFirst
    name
    (\case MName a -> handlesMouseRightUp a)
    (\case MName a -> onMouseRightUp a location)
    >>= mapM_ (\target -> stLastRightPressed .= Just target)
  queueMainViewRefresh chan

applyCurrentSong :: BChan Event -> Maybe MPD.Song -> EventM (MName St) St ()
applyCurrentSong chan song = do
  stPlaying . psCurrentSong .= song
  queueMainViewRefresh chan

toggleDebugView :: BChan Event -> Image.ImageService -> EventM (MName St) St ()
toggleDebugView chan imageService =
  (,) <$> use stCurrentView <*> use stPanic >>= \case
    (Just currentView, False)
      | currentView == DebugView ->
          use stLastView >>= mapM_ (switchViewAndSyncImages chan imageService)
    (Just currentView, True)
      | currentView == DebugView ->
          pure ()
    _ ->
      switchViewAndSyncImages chan imageService DebugView

queueMainViewRefresh :: BChan Event -> EventM (MName St) St ()
queueMainViewRefresh = whenMainView . Image.queueRefreshImages

switchViewAndSyncImages :: BChan Event -> Image.ImageService -> ViewName -> EventM (MName St) St ()
switchViewAndSyncImages chan imageService nextView = do
  previousView <- use stCurrentView
  switchView nextView
  currentView <- use stCurrentView
  when (previousView == Just MainView && currentView /= Just MainView) $
    Image.clearScene imageService
  when (currentView == Just MainView) $
    Image.queueRefreshImages chan

dispatchToFirst ::
  MName St ->
  (MName St -> Bool) ->
  (MName St -> EventM (MName St) St ()) ->
  EventM (MName St) St (Maybe (MName St))
dispatchToFirst name supports action = do
  let target = find supports (nameAncestry name)
  maybe (pure ()) action target
  pure target

whenMainView :: EventM (MName St) St () -> EventM (MName St) St ()
whenMainView action = do
  currentView <- use stCurrentView
  when (currentView == Just MainView) action

(?.=) :: ASetter' St a -> Maybe a -> EventM (MName St) St ()
field ?.= maybeValue =
  for_ maybeValue $ \value -> field .= value

infix 4 ?.=

handleStartEvent :: EventM (MName St) St ()
handleStartEvent = do
  termType <- liftIO Term.deduceTerminalType
  stEnv .= Environment termType (Term.deduceFormat termType)
  sendRequest SignalInit
  sendRequest GetConfig
  sendRequest SignalCurrentQueue
  sendRequest . LogConfig Info $
    "Terminal environment: \n"
      <> "- Terminal type: "
      <> show termType
      <> "\n"
      <> "- Image format: "
      <> show (Term.deduceFormat termType)

handleNormalModeKey :: V.Key -> [V.Modifier] -> EventM (MName St) St ()
handleNormalModeKey key mods = case (key, mods) of
  (V.KChar ' ', []) -> togglePlay
  (V.KChar 'p', []) -> togglePlay
  (V.KChar 'n', []) -> nextSong
  (V.KChar 'b', []) -> prevSong
  (V.KChar '+', []) -> changeVolume 5
  (V.KChar '-', []) -> changeVolume (-5)
  (V.KChar 's', []) -> shuffleQueue
  (V.KChar 'c', []) -> clearQueue
  _ -> pure ()
 where
  togglePlay = do
    stPlaying . psPaused %= not
    paused <- use $ stPlaying . psPaused
    sendRequest . MPDOperation . pure $ MPD.pause paused

  nextSong = do
    stPlaying . psPaused .= False
    sendRequest $ MPDOperation [MPD.next]

  prevSong = do
    stPlaying . psPaused .= False
    sendRequest $ MPDOperation [MPD.previous]

  changeVolume diff = do
    currentVol <- use $ stConfig . csVolume
    let newVol = max 0 (min 100 (fromIntegral currentVol + diff))
    stConfig . csVolume .= fromIntegral newVol
    sendRequest $ MPDOperation [MPD.setVolume (fromIntegral newVol)]

  shuffleQueue = do
    sendRequest $ MPDOperation [MPD.shuffle Nothing]
    sendRequest SignalCurrentQueue

  clearQueue = do
    sendRequest $ MPDOperation [MPD.clear]
    sendRequest SignalCurrentQueue
