{-# LANGUAGE LambdaCase #-}

{- | Central keyboard-focus controller.

Concrete widgets declare focus behavior through 'Drawable'.  This module owns
the Brick focus ring, scope reconciliation, spatial navigation, and the small
state machine for two-step and adjustment interactions.
-}
module Widgets.Focus (
  handleFocusEvent,
  reconcileFocus,
  dismissKeyboardFocus,
) where

import Brick (BrickEvent (..), Extent (..), Location (..))
import Brick.Focus qualified as Focus
import Brick.Main qualified as M
import Brick.Types (EventM)
import Control.Monad (when)
import Control.Monad.State (get)
import Data.List (minimumBy)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, isJust)
import Data.Ord (comparing)
import Data.Set qualified as Set
import Data.Functor (($>))
import Graphics.Vty qualified as V
import Lens.Micro ((^.))
import Lens.Micro.Mtl
import Types
import Widgets.Layer (activeFocusLayerNames)

-- | Rebuild the active Brick ring from the visible topmost layer while
-- preserving a still-valid target.  A hidden or removed target falls back to
-- the first valid target in the scope.
reconcileFocus :: EventM (MName St) St ()
reconcileFocus = do
  previous <- currentFocusTarget
  get >>= ((stFocus .=) . reconciledFocus)
  announceFocusChange previous

reconciledFocus :: St -> FocusState St
reconciledFocus st
  | not (focusEnabled st) =
      previous
        { _fsRing = Focus.focusRing []
        , _fsScope = Nothing
        , _fsRemembered = remembered
        , _fsSession = Nothing
        }
  | otherwise =
      previous
        { _fsRing = ring
        , _fsScope = activeScope
        , _fsRemembered = rememberScope activeScope ring remembered
        , _fsSession = keepLiveSession targets (previous ^. fsSession)
        }
 where
  previous = st ^. stFocus
  remembered = rememberCurrent previous
  activeScope = activeFocusScope st
  targets = maybe [] (focusOrder st) activeScope
  ring = setPreferred preferred (Focus.focusRing targets)
  preferred =
    case activeScope of
      Just scope
        | previous ^. fsScope /= Just scope -> Map.lookup scope remembered
      _ -> Focus.focusGetCurrent (previous ^. fsRing)

-- | Dispatch the keys owned by the focus system.  The caller keeps command
-- editing and the existing global shortcuts in their current priority order.
handleFocusEvent :: BrickEvent (MName St) Event -> EventM (MName St) St Bool
handleFocusEvent event = do
  st <- get
  if not (focusEnabled st)
    then pure False
    else do
      reconcileFocus
      case event of
        VtyEvent (V.EvKey (V.KChar '\t') []) -> useKeyboardFocus (finishThen focusNextTarget)
        VtyEvent (V.EvKey V.KBackTab []) -> useKeyboardFocus (finishThen focusPrevTarget)
        VtyEvent (V.EvKey V.KEnter []) -> useKeyboardFocus activateFocusedTarget
        VtyEvent (V.EvKey V.KEsc []) -> showKeyboardFocus >> cancelFocusedAdjustment
        VtyEvent (V.EvKey V.KUp []) -> useKeyboardFocus (moveOrAdjust FocusUp)
        VtyEvent (V.EvKey V.KDown []) -> useKeyboardFocus (moveOrAdjust FocusDown)
        VtyEvent (V.EvKey V.KLeft []) -> useKeyboardFocus (moveOrAdjust FocusLeft)
        VtyEvent (V.EvKey V.KRight []) -> useKeyboardFocus (moveOrAdjust FocusRight)
        _ -> pure False

useKeyboardFocus :: EventM (MName St) St () -> EventM (MName St) St Bool
useKeyboardFocus action = showKeyboardFocus >> action $> True

showKeyboardFocus :: EventM (MName St) St ()
showKeyboardFocus = stFocus . fsVisible .= True

-- | Leave keyboard navigation without changing the ring's remembered target.
-- A pointer press also commits a live adjustment so no invisible session can
-- keep consuming later arrow keys.
dismissKeyboardFocus :: EventM (MName St) St ()
dismissKeyboardFocus = do
  finishFocusSession
  stFocus . fsVisible .= False

activeFocusScope :: St -> Maybe (MName St)
activeFocusScope st =
  case activeFocusLayerNames st of
    scope : _ -> Just scope
    [] -> Nothing

focusEnabled :: St -> Bool
focusEnabled st =
  st ^. stMode == NormalMode
    || isJust (st ^. stDialogView)
    || not (null $ st ^. stMenu . msWidgets)

focusOrder :: St -> MName St -> [MName St]
focusOrder st = go Set.empty
 where
  go seen name
    | Set.member name seen = []
    | otherwise =
        let seen' = Set.insert name seen
            self
              | isJust (focusFor st name) = [name]
              | otherwise = []
            children = childrenFor st name
         in self <> concatMap (go seen') children

focusFor :: St -> MName St -> Maybe (FocusBinding St)
focusFor st = named (`focusBinding` st)

childrenFor :: St -> MName St -> [MName St]
childrenFor st = named (`focusChildren` st)

rememberCurrent :: FocusState St -> Map.Map (MName St) (MName St)
rememberCurrent focus =
  case (focus ^. fsScope, Focus.focusGetCurrent $ focus ^. fsRing) of
    (Just scope, Just current) -> Map.insert scope current (focus ^. fsRemembered)
    _ -> focus ^. fsRemembered

rememberScope :: Maybe (MName St) -> Focus.FocusRing (MName St) -> Map.Map (MName St) (MName St) -> Map.Map (MName St) (MName St)
rememberScope scope ring remembered =
  case (scope, Focus.focusGetCurrent ring) of
    (Just currentScope, Just current) -> Map.insert currentScope current remembered
    _ -> remembered

setPreferred :: Maybe (MName St) -> Focus.FocusRing (MName St) -> Focus.FocusRing (MName St)
setPreferred preferred ring
  | Just target <- preferred
  , target `elem` Focus.focusRingToList ring = Focus.focusSetCurrent target ring
  | otherwise = ring

keepLiveSession :: [MName St] -> Maybe (FocusSession St) -> Maybe (FocusSession St)
keepLiveSession targets = \case
  Just session
    | sessionTarget session `elem` targets -> Just session
  _ -> Nothing

sessionTarget :: FocusSession St -> MName St
sessionTarget = \case
  FocusArmed target -> target
  FocusAdjusting target _ -> target

finishThen :: EventM (MName St) St () -> EventM (MName St) St ()
finishThen next = do
  finishFocusSession
  reconcileFocus
  next

finishFocusSession :: EventM (MName St) St ()
finishFocusSession = do
  use (stFocus . fsSession) >>= maybe (pure ()) commitAdjustment
  stFocus . fsSession .= Nothing
 where
  commitAdjustment = \case
    FocusAdjusting _ transaction -> commitFocusAdjustment transaction
    FocusArmed{} -> pure ()

cancelFocusedAdjustment :: EventM (MName St) St Bool
cancelFocusedAdjustment = do
  use (stFocus . fsSession) >>= \case
    Just (FocusAdjusting _ transaction) -> do
      cancelFocusAdjustment transaction
      stFocus . fsSession .= Nothing
      pure True
    _ -> pure False

activateFocusedTarget :: EventM (MName St) St ()
activateFocusedTarget = do
  currentFocusTarget >>= mapM_ activate
 where
  activate target = do
    st <- get
    case focusFor st target of
      Nothing -> pure ()
      Just FocusPassive -> stFocus . fsSession .= Nothing
      Just (FocusAction action) -> do
        stFocus . fsSession .= Nothing
        action
      Just (FocusTwoStep prepare action) ->
        use (stFocus . fsSession) >>= \case
          Just (FocusArmed armedTarget)
            | armedTarget == target -> do
                stFocus . fsSession .= Nothing
                action
          _ -> do
            stFocus . fsSession .= Nothing
            prepare
            stFocus . fsSession .= Just (FocusArmed target)
      Just (FocusAdjust begin) -> do
        stFocus . fsSession .= Nothing
        transaction <- begin
        stFocus . fsSession .= Just (FocusAdjusting target transaction)

moveOrAdjust :: FocusDirection -> EventM (MName St) St ()
moveOrAdjust direction =
  use (stFocus . fsSession) >>= \case
    Just (FocusAdjusting _ transaction) -> do
      consumed <- adjustFocus transaction direction
      if consumed
        then pure ()
        else finishThen (moveSpatial direction)
    _ -> finishThen (moveSpatial direction)

focusNextTarget :: EventM (MName St) St ()
focusNextTarget = moveRing Focus.focusNext

focusPrevTarget :: EventM (MName St) St ()
focusPrevTarget = moveRing Focus.focusPrev

moveRing :: (Focus.FocusRing (MName St) -> Focus.FocusRing (MName St)) -> EventM (MName St) St ()
moveRing advance = do
  previous <- currentFocusTarget
  stFocus . fsRing %= advance
  announceFocusChange previous

moveSpatial :: FocusDirection -> EventM (MName St) St ()
moveSpatial direction = do
  currentFocusTarget >>= \case
    Nothing -> pure ()
    Just current -> do
      M.lookupExtent current >>= \case
        Nothing -> pure ()
        Just currentExtent -> do
          targets <- Focus.focusRingToList <$> use (stFocus . fsRing)
          candidates <- mapMaybeM (candidateFor current currentExtent) (zip [0 ..] targets)
          case candidates of
            [] -> pure ()
            _ -> setCurrentTarget (third $ minimumBy (comparing first) candidates)
 where
  candidateFor current currentExtent (index, target)
    | target == current = pure Nothing
    | otherwise =
        M.lookupExtent target >>= \case
          Nothing -> pure Nothing
          Just targetExtent
            | inDirection direction currentExtent targetExtent ->
                pure $ Just (spatialScore direction currentExtent targetExtent index, index, target)
            | otherwise -> pure Nothing

  first (value, _, _) = value
  third (_, _, value) = value

setCurrentTarget :: MName St -> EventM (MName St) St ()
setCurrentTarget target = do
  previous <- currentFocusTarget
  stFocus . fsRing %= Focus.focusSetCurrent target
  announceFocusChange previous

announceFocusChange :: Maybe (MName St) -> EventM (MName St) St ()
announceFocusChange previous = do
  current <- currentFocusTarget
  visible <- use (stFocus . fsVisible)
  when visible $ do
    mapM_ makeTargetVisible current
  when (visible && current /= previous) $
    mapM_ runFocusEntered current

runFocusEntered :: MName St -> EventM (MName St) St ()
runFocusEntered target = do
  st <- get
  sequence_ $ named (`onFocus` st) target

makeTargetVisible :: MName St -> EventM (MName St) St ()
makeTargetVisible = M.makeVisible

currentFocusTarget :: EventM (MName St) St (Maybe (MName St))
currentFocusTarget = Focus.focusGetCurrent <$> use (stFocus . fsRing)

inDirection :: FocusDirection -> Extent n -> Extent n -> Bool
inDirection direction source target = sourcePrimary < targetPrimary
 where
  (sourcePrimary, _) = directionalCenter direction source
  (targetPrimary, _) = directionalCenter direction target

spatialScore :: FocusDirection -> Extent n -> Extent n -> Int -> (Int, Int, Int, Int)
spatialScore direction source target index =
  ( if overlapsPerpendicular direction source target then 0 else 1
  , primaryDistance direction source target
  , perpendicularDistance direction source target
  , index
  )

extentCenter :: Extent n -> (Int, Int)
extentCenter extent =
  let Location (x, y) = extentUpperLeft extent
      (width, height) = extentSize extent
   in (x + width `div` 2, y + height `div` 2)

overlapsPerpendicular :: FocusDirection -> Extent n -> Extent n -> Bool
overlapsPerpendicular direction source target =
  case direction of
    FocusUp -> overlaps sourceX sourceWidth targetX targetWidth
    FocusDown -> overlaps sourceX sourceWidth targetX targetWidth
    FocusLeft -> overlaps sourceY sourceHeight targetY targetHeight
    FocusRight -> overlaps sourceY sourceHeight targetY targetHeight
 where
  Location (sourceX, sourceY) = extentUpperLeft source
  (sourceWidth, sourceHeight) = extentSize source
  Location (targetX, targetY) = extentUpperLeft target
  (targetWidth, targetHeight) = extentSize target
  overlaps startA lengthA startB lengthB =
    startA < startB + lengthB && startB < startA + lengthA

primaryDistance :: FocusDirection -> Extent n -> Extent n -> Int
primaryDistance direction source target = targetPrimary - sourcePrimary
 where
  (sourcePrimary, _) = directionalCenter direction source
  (targetPrimary, _) = directionalCenter direction target

perpendicularDistance :: FocusDirection -> Extent n -> Extent n -> Int
perpendicularDistance direction source target = abs (sourcePerpendicular - targetPerpendicular)
 where
  (_, sourcePerpendicular) = directionalCenter direction source
  (_, targetPerpendicular) = directionalCenter direction target

directionalCenter :: FocusDirection -> Extent n -> (Int, Int)
directionalCenter direction extent =
  case (direction, extentCenter extent) of
    (FocusUp, (x, y)) -> (-y, x)
    (FocusDown, (x, y)) -> (y, x)
    (FocusLeft, (x, y)) -> (-x, y)
    (FocusRight, (x, y)) -> (x, y)

mapMaybeM :: (a -> EventM (MName St) St (Maybe b)) -> [a] -> EventM (MName St) St [b]
mapMaybeM f = fmap catMaybes . traverse f
