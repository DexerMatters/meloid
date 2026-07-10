{- | This module provides widgets for editing.
For now, there is only a command editor. In the future, there will
be editors on playerlist names and configurations.
-}
module Widgets.Edits (
  CommandEditor (..),
) where

import Brick
import Brick.Widgets.Core qualified as W
import Brick.Widgets.Edit qualified as E
import Lens.Micro
import Types

data CommandEditor = CommandEditor

instance Drawable St CommandEditor where
  draw _ st =
    W.hLimit 10
      $ E.renderEditor
        (str . unlines)
        (st ^. stMode == CommandMode)
      $ st ^. stEdits . esCommand
  onMouseLeftDown _ = Just $ \_ -> pure ()
