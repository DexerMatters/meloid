{-# LANGUAGE LambdaCase #-}

{- | This module provides widgets to control, such like different
buttons and scroll bars.
-}
module Widgets.Controls (
  VolumeBar (..),
  SongProgressBar (..),
  PlayButton (..),
  RewindButton (..),
  ForwardButton (..),
  IncreaseVolumeButton (..),
  DecreaseVolumeButton (..),
  OkButton (..),
  SkipButton (..),
  NextButton (..),
  FinishButton (..),
  PrevButton (..),
  ReverseOrderButton (..),
  ShuffleButton (..),
  ClearButton (..),
) where

import Brick
import Brick.Main qualified as M
import Brick.Widgets.Core qualified as W
import Control.Monad (forM_, when)
import Data.Vector qualified as Vec
import Lens.Micro ((&), (^.))
import Lens.Micro.Mtl
import Network.MPD qualified as MPD
import Types
import Widgets.Common

data VolumeBar = VolumeBar

data SongProgressBar = SongProgressBar

data PlayButton = PlayButton

data RewindButton = RewindButton

data ForwardButton = ForwardButton

data IncreaseVolumeButton = IncreaseVolumeButton

data DecreaseVolumeButton = DecreaseVolumeButton

data OkButton = OkButton

data SkipButton = SkipButton

data NextButton = NextButton

data FinishButton = FinishButton

data PrevButton = PrevButton

data ReverseOrderButton = ReverseOrderButton

data ShuffleButton = ShuffleButton

data ClearButton = ClearButton

volumeBarWidth :: Int
volumeBarWidth = 21

instance Drawable St VolumeBar where
  draw _ st =
    W.withAttr (attrName "progressBarIncomplete") $
      W.hLimit volumeBarWidth $
        W.withAttr (attrName "progressBarComplete") $
          W.str $
            makeBar volumeBarWidth (st ^. stConfig . csVolume & fromIntegral) 100
  handlesMouseLeftDown _ = True
  onMouseLeftDown _ (Location (ax, ay)) = when (ay == 0) $ do
    let volume =
          max 0 . min 100 $
            if volumeBarWidth <= 1
              then 100
              else (ax * 100) `div` (volumeBarWidth - 1)
    stConfig . csVolume .= fromIntegral volume
    sendRequest $ MPDOperation [MPD.setVolume (fromIntegral volume)]
  parent _ = Just (ParentView MainView)

instance Drawable St SongProgressBar where
  draw _ st = Widget Greedy Fixed $ do
    ctx <- getContext
    let width = ctx ^. availWidthL
        (current, total) = maybe (0, 0) id (st ^. stShownCurrentTime)
        bar = makeBar' width (floor current) (floor total)
        (filled, rest) = span (/= ' ') bar
    render $
      W.hBox
        [ W.withAttr (attrName "progressBarComplete") $ W.str filled
        , W.withAttr (attrName "progressBarIncomplete") $ W.str rest
        ]
  willReportExtent _ = True
  handlesMouseLeftDown _ = True
  onMouseLeftDown _ (Location (ax, ay)) =
    when (ay == 0) $
      previewSongProgressAt ax
  handlesMouseLeftUp _ = True
  onMouseLeftUp _ (Location (ax, ay)) =
    when (ay == 0) $
      M.lookupExtent (mName SongProgressBar) >>= \case
        Just extent -> do
          currentPos <- use stCurrentSongPos
          use (stShownCurrentTime) >>= \case
            Just (_, total)
              | total > 0 ->
                  let target = songProgressTarget (fst $ extentSize extent) ax total
                   in do
                        stPlaying . psCurrentTime .= Just (target, total)
                        forM_ currentPos $ \pos ->
                          sendRequest $ MPDOperation [MPD.seek pos target]
            _ ->
              pure ()
        Nothing ->
          pure ()
  parent _ = Just (ParentView MainView)

instance Drawable St PlayButton where
  draw _ st =
    drawIconButton
      st
      (mName PlayButton)
      ( if st ^. stPlaying . psPaused
          then "|>"
          else "||"
      )
  isClickable _ = True
  handlesMouseLeftUp _ = True
  onMouseLeftUp _ _ = do
    stPlaying . psPaused %= not
    paused <- use $ stPlaying . psPaused
    sendRequest . MPDOperation . pure $
      MPD.pause paused
  parent _ = Just (ParentView MainView)

instance Drawable St RewindButton where
  draw _ st = drawIconButton st (mName RewindButton) "<<"
  isClickable _ = True
  handlesMouseLeftUp _ = True
  onMouseLeftUp _ _ = do
    current <- use $ stPlaying . psCurrentSong
    stPlaying . psPaused .= False
    sendRequest $ MPDOperation [MPD.play $ current >>= MPD.sgIndex]
  parent _ = Just (ParentView MainView)

instance Drawable St ForwardButton where
  draw _ st = drawIconButton st (mName ForwardButton) ">>"
  isClickable _ = True
  handlesMouseLeftUp _ = True
  onMouseLeftUp _ _ = do
    stPlaying . psPaused .= False
    -- Prevent exceed the playing queue
    current <- use $ stPlaying . psCurrentSong
    case current of
      Nothing -> pure ()
      Just _ -> sendRequest $ MPDOperation [MPD.next]
  parent _ = Just (ParentView MainView)

instance Drawable St IncreaseVolumeButton where
  draw _ st = drawIconButton st (mName IncreaseVolumeButton) "+ "
  handlesMouseLeftUp _ = True
  onMouseLeftUp _ _ = do
    currentVol <- use $ stConfig . csVolume
    let newVol = min 100 (currentVol + 1)
    stConfig . csVolume .= newVol
    sendRequest $ MPDOperation [MPD.setVolume (fromIntegral newVol)]
  parent _ = Just (ParentView MainView)

instance Drawable St DecreaseVolumeButton where
  draw _ st = drawIconButton st (mName DecreaseVolumeButton) "- "
  handlesMouseLeftUp _ = True
  onMouseLeftUp _ _ = do
    currentVol <- use $ stConfig . csVolume
    let newVol = max 0 (currentVol - 1)
    stConfig . csVolume .= newVol
    sendRequest $ MPDOperation [MPD.setVolume (fromIntegral newVol)]
  parent _ = Just (ParentView MainView)

instance Drawable St OkButton where
  draw _ st = drawButton st (mName OkButton) "    OK    "
  handlesMouseLeftUp _ = True
  onMouseLeftUp _ _ = closeDialog
  parent _ = Just (ParentView SimpleDialog)

instance Drawable St SkipButton where
  draw _ st = drawButton st (mName SkipButton) "   SKIP   "
  handlesMouseLeftUp _ = True
  onMouseLeftUp _ _ = do
    closeDialog
    stDialog .? dsPage .= 1
  parent _ = Just (ParentView WelcomeDialog)

instance Drawable St NextButton where
  draw _ st = drawButton st (mName NextButton) "   NEXT   "
  handlesMouseLeftUp _ = True
  onMouseLeftUp _ _ = stDialog .? dsPage %= (+ 1)
  parent _ = Just (ParentView WelcomeDialog)

instance Drawable St FinishButton where
  draw _ st = drawButton st (mName FinishButton) "   FINISH   "
  handlesMouseLeftUp _ = True
  onMouseLeftUp _ _ = do
    closeDialog
    stDialog .? dsPage .= 1
  parent _ = Just (ParentView WelcomeDialog)

instance Drawable St PrevButton where
  draw _ st = drawButton st (mName PrevButton) "   PREV   "
  handlesMouseLeftUp _ = True
  onMouseLeftUp _ _ = stDialog .? dsPage %= subtract 1
  parent _ = Just (ParentView WelcomeDialog)

instance Drawable St ReverseOrderButton where
  draw _ st = drawIconButton st (mName ReverseOrderButton) "↑↓"
  handlesMouseLeftUp _ = True
  onMouseLeftUp _ _ = do
    queue <- use $ stPlaying . psCurrentQueue
    sendRequest . MPDOperation . pure $
      forM_ (reverseQueueMoves $ Vec.length queue) $
        uncurry MPD.move
    sendRequest $ SignalCurrentQueue
  parent _ = Just (ParentView MainView)

instance Drawable St ShuffleButton where
  draw _ st = drawIconButton st (mName ShuffleButton) "⇡⇣"
  handlesMouseLeftUp _ = True
  onMouseLeftUp _ _ = do
    sendRequest $ MPDOperation $ pure $ do
      MPD.shuffle Nothing
    sendRequest $ SignalCurrentQueue
  parent _ = Just (ParentView MainView)

instance Drawable St ClearButton where
  draw _ st = drawIconButton st (mName ClearButton) "><"
  handlesMouseLeftUp _ = True
  onMouseLeftUp _ _ = do
    sendRequest $ MPDOperation $ pure $ do
      MPD.clear
    sendRequest $ SignalCurrentQueue
  parent _ = Just (ParentView MainView)

reverseQueueMoves :: Int -> [(MPD.Position, MPD.Position)]
reverseQueueMoves queueLength =
  let lastPosition = fromIntegral (queueLength - 1)
   in [(lastPosition, fromIntegral target) | target <- [0 .. queueLength - 2]]

songProgressTarget :: Int -> Int -> Double -> Double
songProgressTarget width x total =
  max 0 . min total $
    if width <= 1
      then total
      else fromIntegral clampedX * total / fromIntegral (width - 1)
 where
  clampedX = max 0 $ min (width - 1) x

previewSongProgressAt :: Int -> EventM (MName St) St ()
previewSongProgressAt x =
  M.lookupExtent (mName SongProgressBar) >>= \case
    Just extent ->
      use stShownCurrentTime >>= \case
        Just (_, total)
          | total > 0 ->
              stSongProgressPreview .= Just (songProgressTarget (fst $ extentSize extent) x total, total)
        _ ->
          pure ()
    Nothing ->
      pure ()
