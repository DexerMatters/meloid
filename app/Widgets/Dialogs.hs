{-# LANGUAGE LambdaCase #-}

-- | Rendering and controls shared by every dialog description.
module Widgets.Dialogs (
  DialogButton (..),
  drawDialog,
  dialogFocusChildren,
) where

import Brick
import Brick qualified as B
import Brick.Main qualified as M
import Brick.Widgets.Border qualified as Bd
import Brick.Widgets.Core qualified as W
import Lens.Micro ((^.))
import Lens.Micro.Mtl
import Types
import Widgets.Common (drawButton, scrollTransaction, scrollViewportBy, viewportWithBar)

data DialogButton
  = DialogPreviousButton
  | DialogNextButton
  | DialogFinishButton
  | DialogCancelButton
  | DialogNoButton
  | DialogYesButton

data DialogViewport = DialogViewport

dialogMaxWidth :: Int
dialogMaxWidth = 64

dialogMaxHeight :: Int
dialogMaxHeight = 20

dialogBodyMaxHeight :: Int
dialogBodyMaxHeight = 14

-- | Draw the active dialog's shared frame around its supplied content.
drawDialog :: St -> Widget (MName St)
drawDialog st =
  maybe W.emptyWidget (drawDialogState st) (st ^. stDialog)

drawDialogState :: St -> DialogState St -> Widget (MName St)
drawDialogState st dialog =
  W.withAttr (attrName "dialog") . W.hLimit dialogMaxWidth . W.vLimit dialogMaxHeight $
    Bd.borderWithLabel title $
      W.padAll 1 . W.vBox $
        [ W.vLimit dialogBodyMaxHeight $ drawNamed st DialogViewport
        , W.padTop (W.Pad 1) $ drawDialogButtons st dialog
        ]
 where
  title = W.withAttr (attrName "header") . W.str $ " " <> dialogTitle dialog <> " "

drawDialogButtons :: St -> DialogState St -> Widget (MName St)
drawDialogButtons st dialog
  | dialogIsSimple dialog =
      W.hBox
        [ W.padRight (W.Pad 1) $ drawNamed st DialogCancelButton
        , W.padLeft W.Max $
            W.hBox
              [ drawNamed st DialogNoButton
              , W.padLeft (W.Pad 1) $ drawNamed st DialogYesButton
              ]
        ]
  | otherwise =
      W.hBox
        [ previous
        , W.padLeft W.Max nextOrFinish
        ]
 where
  previous
    | dialogCanGoBack dialog = drawNamed st DialogPreviousButton
    | otherwise = W.emptyWidget

  nextOrFinish
    | dialogCanGoForward dialog = drawNamed st DialogNextButton
    | otherwise = drawNamed st DialogFinishButton

-- | Focus only the controls that are visible for the active dialog page.
dialogFocusChildren :: St -> [MName St]
dialogFocusChildren st =
  maybe [] ((mName DialogViewport :) . focusButtons) (st ^. stDialog)
 where
  focusButtons dialog
    | dialogIsSimple dialog = [mName DialogCancelButton, mName DialogNoButton, mName DialogYesButton]
    | otherwise = previous <> [nextOrFinish]
   where
    previous
      | dialogCanGoBack dialog = [mName DialogPreviousButton]
      | otherwise = []
    nextOrFinish
      | dialogCanGoForward dialog = mName DialogNextButton
      | otherwise = mName DialogFinishButton

instance Drawable St DialogButton where
  draw button st = drawButton st (mName button) label
   where
    label =
      case button of
        DialogPreviousButton -> "   PREV   "
        DialogNextButton -> "   NEXT   "
        DialogFinishButton -> "  FINISH  "
        DialogCancelButton -> " CANCEL "
        DialogNoButton -> "   NO   "
        DialogYesButton -> "  YES   "
  onMouseLeftUp button = Just $ \_ -> activateDialogButton button
  variant = \case
    DialogPreviousButton -> 0
    DialogNextButton -> 1
    DialogFinishButton -> 2
    DialogCancelButton -> 3
    DialogNoButton -> 4
    DialogYesButton -> 5

activateDialogButton :: DialogButton -> EventM (MName St) St ()
activateDialogButton = \case
  DialogPreviousButton -> changeDialogPage dialogPreviousPage
  DialogNextButton -> changeDialogPage dialogNextPage
  DialogFinishButton -> closeDialog
  DialogCancelButton -> closeDialog
  DialogNoButton -> runSimpleAction False
  DialogYesButton -> runSimpleAction True

instance Drawable St DialogViewport where
  draw _ st =
    viewportWithBar st (mName DialogViewport) $
      maybe W.emptyWidget dialogPageWidget (st ^. stDialog)
  onMouseScrollUp _ = Just $ scrollViewportBy (mName DialogViewport) (-1)
  onMouseScrollDown _ = Just $ scrollViewportBy (mName DialogViewport) 1
  focusBinding _ _ = Just $ FocusAdjust $ scrollTransaction (mName DialogViewport)

changeDialogPage :: (DialogState St -> DialogState St) -> EventM (MName St) St ()
changeDialogPage update = do
  stDialog %= fmap update
  M.vScrollToBeginning $ B.viewportScroll (mName DialogViewport)

runSimpleAction :: Bool -> EventM (MName St) St ()
runSimpleAction chooseYes =
  use stDialog >>= \case
    Just dialog -> maybe (pure ()) (closeDialog >>) $ dialogSimpleAction chooseYes dialog
    Nothing -> pure ()
