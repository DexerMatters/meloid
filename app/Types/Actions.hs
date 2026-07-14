{-# LANGUAGE LambdaCase #-}

{- | State-changing actions that are shared across
the application.
These helpers keep mutation in one place instead of spreading
it through the view and widget code.
-}
module Types.Actions (
  panic,
  closeDialog,
  openSimpleDialog,
  switchView,
  switchMode,
  returnToLastView,
  sendRequest,
  trigger,
  unTrigger,
  openMenu,
  repositionMenu,
  closeMenu,
) where

import Brick (Extent (..), Location (..))
import Brick.BChan (writeBChan)
import Brick.Main qualified as M
import Brick.Types (EventM)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Set qualified as Set
import Graphics.Vty qualified as V
import Lens.Micro (to)
import Lens.Micro.Mtl
import Types.Core
import Types.Identity (MName, ViewName, placeholderName)
import Types.Model

-- | Mark the application as panicked so the outer loop can stop.
panic :: EventM (MName St) St ()
panic = stPanic .= True

-- | Close the active dialog and clear its view marker.
closeDialog :: EventM (MName St) St ()
closeDialog = do
  stDialog .= Nothing
  stDialogView .= Nothing

-- | Open a simple text dialog.
openSimpleDialog :: ViewName -> String -> EventM (MName St) St ()
openSimpleDialog dialogName text = do
  stDialog .= Just (DialogSt 0 text)
  stDialogView .= Just dialogName

-- | Switch to a different top-level view.
switchView :: ViewName -> EventM (MName St) St ()
switchView v = do
  current <- use stCurrentView
  unless (current == Just v) $ do
    stLastView .= current
    stCurrentView .= Just v

-- | Cycle through the application modes.
switchMode :: EventM (MName St) St ()
switchMode =
  stMode %= \case
    NormalMode -> CommandMode
    CommandMode -> EditMode
    EditMode -> NormalMode

-- | Return to the previous view, if one exists.
returnToLastView :: EventM (MName St) St ()
returnToLastView = use stLastView >>= mapM_ switchView

-- | Send a request to the background worker if the channel exists.
sendRequest :: Request -> EventM (MName St) St ()
sendRequest r = do
  chan <- use stChannel
  case chan of
    Nothing -> pure ()
    Just c -> liftIO $ writeBChan c r

{- | Trigger a widget by its name.
This means inserting the widget into the stTriggeredNames set.
-}
trigger :: MName St -> EventM (MName St) St ()
trigger name = stTriggeredNames %= Set.insert name

{- | Untrigger a widget by its name.
This means removing the widget from the stTriggeredNames set.
-}
unTrigger :: MName St -> EventM (MName St) St ()
unTrigger name = stTriggeredNames %= Set.delete name

-- | Opens a menu relative to a stable widget name.
openMenu :: MName St -> [MenuWidget] -> EventM (MName St) St ()
openMenu location widgets = do
  vty <- M.getVtyHandle
  windowSize <- liftIO $ V.displayBounds (V.outputIface vty)
  let size = menuSize windowSize
  offset <-
    M.lookupExtent location >>= \case
      Nothing -> pure $ Location (0, 0)
      Just extent -> pure $ menuOffset extent windowSize size
  stMenu .= MenuSt widgets location offset size
 where
  menuSize (windowWidth, windowHeight) =
    ( max 1 $ min 18 windowWidth
    , max 1 $ min (length widgets + 2) windowHeight
    )
  menuOffset extent (windowWidth, windowHeight) (menuWidth, menuHeight) =
    Location
      ( max (-anchorX) $ min 0 (windowWidth - menuWidth - anchorX)
      , max (-anchorY) $ min 0 (windowHeight - menuHeight - anchorY)
      )
   where
    Location (anchorX, anchorY) = extentUpperLeft extent

-- | Recalculate an open menu after terminal geometry changes.
repositionMenu :: EventM (MName St) St ()
repositionMenu = do
  MenuSt widgets location _ _ <- use stMenu
  unless (null widgets) $ openMenu location widgets

-- | Closes the currently open menu
closeMenu :: EventM (MName St) St ()
closeMenu = do
  isOpenned <- use (stMenu . msWidgets . to (not . null))
  when isOpenned $ stMenu .= MenuSt [] placeholderName (Location (0, 0)) (0, 0)
