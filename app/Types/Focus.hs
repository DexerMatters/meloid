{-# LANGUAGE TemplateHaskell #-}

{- | Runtime state and pure helpers for the application's Brick focus ring.

This module deliberately knows nothing about concrete widgets or views.  The
widget layer supplies the active scope and its focus tree; this layer stores
Brick's ring and the transient interaction session.
-}
module Types.Focus (
  FocusState (..),
  FocusSession (..),
  emptyFocusState,
  invalidateFocus,
  fsRing,
  fsScope,
  fsRemembered,
  fsSession,
  fsVisible,
) where

import Brick.Focus qualified as Focus
import Data.Map qualified as Map
import Lens.Micro.TH (makeLenses)
import Types.Identity

data FocusSession st
  = FocusArmed (MName st)
  | FocusAdjusting (MName st) (FocusTransaction st)

data FocusState st = FocusState
  { _fsRing :: Focus.FocusRing (MName st)
  , _fsScope :: Maybe (MName st)
  , _fsRemembered :: Map.Map (MName st) (MName st)
  , _fsSession :: Maybe (FocusSession st)
  , _fsVisible :: Bool
  }

makeLenses ''FocusState

emptyFocusState :: FocusState st
emptyFocusState = FocusState (Focus.focusRing []) Nothing Map.empty Nothing False

{- | Drop the active ring/session while retaining remembered targets for other
scopes.  Menu replacement uses this so a submenu begins at its first item.
-}
invalidateFocus :: FocusState st -> FocusState st
invalidateFocus focus =
  focus
    { _fsRing = Focus.focusRing []
    , _fsScope = Nothing
    , _fsSession = Nothing
    }
