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
  inlineOutput,
  clearCommandEdit,
  nextCommandStage,
) where

import Brick.BChan (writeBChan)
import Brick.Types (EventM)
import Brick.Widgets.Edit qualified as E
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Text.Zipper qualified as TZ
import Lens.Micro
import Lens.Micro.Mtl
import Types.Core
import Types.Helpers
import Types.Identity (MName, ViewName)
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

inlineOutput :: LogLevel -> String -> EventM (MName St) St ()
inlineOutput l s = do
  clearCommandEdit
  clearCommandStages
  stMode .= NormalMode
  stInlineOutput .= (l, s)

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

-- | Clear the current command edit
clearCommandEdit :: EventM (MName St) St ()
clearCommandEdit =
  stEdits . esCommand %= E.applyEdit (const (TZ.stringZipper [] Nothing))

-- | Clear the command stages
clearCommandStages :: EventM (MName St) St ()
clearCommandStages = zoom stCommandStages $ do
  cpsStages .= Nothing
  cpsParameters .= []
  cpsCommand .= ""

-- | Proceed to the next stage of a command
nextCommandStage :: EventM (MName St) St ()
nextCommandStage = do
  currentStage <- use stCurrentStage
  case currentStage of
    Just s -> handleStage s
    _ -> pure ()
 where
  handleStage = \case
    ExecutionStage e -> do
      result <- use $ stCommandStages . cpsParameters . to e
      result >>= \case
        Left err -> inlineOutput Error $ "Execution Error: " <> err
        Right info -> inlineOutput Info info
    InputStage _ p next -> do
      input <- use $ stEditorContent esCommand
      case p input of
        Left err -> inlineOutput Error $ "Expected input: " <> err
        Right _ -> do
          stCommandStages . cpsParameters %= (++ [input])
          stCommandStages . cpsStages .= Just next
          clearCommandEdit
          case next of
            ExecutionStage _ -> nextCommandStage
            InputStage {} -> pure ()
