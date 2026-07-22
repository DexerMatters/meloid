{- | Reusable dialog descriptions and their small amount of navigation state.

Dialog content is ordinary Brick widgets; the rendering layer supplies the
shared frame and controls. This keeps dialog call sites declarative while
leaving their effects in the action that opens them.
-}
module Types.Dialog (
  Dialog (..),
  DialogState,
  dialogState,
  dialogTitle,
  dialogPageWidgets,
  dialogCanGoBack,
  dialogCanGoForward,
  dialogPreviousPage,
  dialogNextPage,
  dialogIsSimple,
  dialogSimpleAction,
) where

import Brick.Types (EventM, Widget)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Types.Identity (MName)

-- | A multi-page dialog or a single confirmation dialog.
data Dialog st
  = PagedDialog String (NonEmpty [Widget (MName st)])
  | SimpleDialog
      String
      (Widget (MName st))
      (EventM (MName st) st ())
      (EventM (MName st) st ())

-- | Runtime state shared by all dialogs. Only paged dialogs use the index.
data DialogState st = DialogState (Dialog st) Int

dialogState :: Dialog st -> DialogState st
dialogState dialog = DialogState dialog 0

dialogTitle :: DialogState st -> String
dialogTitle (DialogState dialog _) =
  case dialog of
    PagedDialog title _ -> title
    SimpleDialog title _ _ _ -> title

dialogPageWidgets :: DialogState st -> [Widget (MName st)]
dialogPageWidgets (DialogState dialog page) =
  case dialog of
    PagedDialog _ pages ->
      case drop page (NonEmpty.toList pages) of
        current : _ -> current
        [] -> NonEmpty.last pages
    SimpleDialog _ widget _ _ -> [widget]

dialogCanGoBack :: DialogState st -> Bool
dialogCanGoBack (DialogState PagedDialog{} page) = page > 0
dialogCanGoBack _ = False

dialogCanGoForward :: DialogState st -> Bool
dialogCanGoForward (DialogState (PagedDialog _ pages) page) = page < NonEmpty.length pages - 1
dialogCanGoForward _ = False

dialogPreviousPage :: DialogState st -> DialogState st
dialogPreviousPage state@(DialogState dialog page)
  | dialogCanGoBack state = DialogState dialog (page - 1)
  | otherwise = state

dialogNextPage :: DialogState st -> DialogState st
dialogNextPage state@(DialogState dialog page)
  | dialogCanGoForward state = DialogState dialog (page + 1)
  | otherwise = state

dialogIsSimple :: DialogState st -> Bool
dialogIsSimple (DialogState SimpleDialog{} _) = True
dialogIsSimple _ = False

-- | Select the programmable action for the NO or YES button, respectively.
dialogSimpleAction :: Bool -> DialogState st -> Maybe (EventM (MName st) st ())
dialogSimpleAction chooseYes (DialogState (SimpleDialog _ _ noAction yesAction) _) =
  Just $ if chooseYes then yesAction else noAction
dialogSimpleAction _ _ = Nothing
