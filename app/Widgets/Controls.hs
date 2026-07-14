{-# LANGUAGE LambdaCase #-}

{- | This module provides widgets to control, such like different
buttons and scroll bars.
-}
module Widgets.Controls (
  VolumeBar (..),
  SongProgressBar (..),
  EQGainBarsViewport (..),
  EQGainBar (..),
  PlayButton (..),
  RewindButton (..),
  ForwardButton (..),
  OkButton (..),
  SkipButton (..),
  NextButton (..),
  FinishButton (..),
  PrevButton (..),
  ReverseOrderButton (..),
  ShuffleButton (..),
  ClearButton (..),
  EQSwitch (..),
) where

import Brick
import Brick.Main qualified as M
import Brick.Widgets.Core qualified as W
import Control.Monad (forM_, when)
import Data.Map qualified as Map
import Data.Maybe (listToMaybe)
import Data.Vector qualified as Vec
import Lens.Micro ((%~), (&), (.~), (^.))
import Lens.Micro.Mtl
import Network.MPD qualified as MPD
import Types
import Utils
import Widgets.Common
import Widgets.Elements.Common (ElementNode (..), ElementPath, pathVariant)

data VolumeBar = VolumeBar

data SongProgressBar = SongProgressBar

data EQGainBarsViewport = EQGainBarsViewport ElementPath

data EQGainBar = EQGainBar ElementPath Int

data PlayButton = PlayButton

data RewindButton = RewindButton

data ForwardButton = ForwardButton

data OkButton = OkButton

data SkipButton = SkipButton

data NextButton = NextButton

data FinishButton = FinishButton

data PrevButton = PrevButton

data ReverseOrderButton = ReverseOrderButton

data ShuffleButton = ShuffleButton

data ClearButton = ClearButton

data EQSwitch = EQSwitch ElementPath

volumeBarWidth :: Int
volumeBarWidth = 21

eqGainBarWidth :: Int
eqGainBarWidth = 5

eqGainBarsViewportStep :: Int
eqGainBarsViewportStep = eqGainBarWidth + 1

instance Drawable St VolumeBar where
  draw _ st =
    W.withAttr (attrName "progressBarIncomplete") $
      W.hLimit volumeBarWidth $
        W.withAttr (attrName "progressBarComplete") $
          W.str $
            makeBar volumeBarWidth (st ^. stConfig . csVolume & fromIntegral) 100
  onMouseLeftDown _ = Just $ \(Location (ax, ay)) -> when (ay == 0) $ do
    let volume =
          max 0 . min 100 $
            if volumeBarWidth <= 1
              then 100
              else (ax * 100) `div` (volumeBarWidth - 1)
    setVolumeBarValue volume
  onMouseScrollUp' _ =
    Just $
      use (stConfig . csVolume) >>= setVolumeBarValue . (+ 1) . fromIntegral
  onMouseScrollDown' _ =
    Just $
      use (stConfig . csVolume) >>= setVolumeBarValue . (subtract 1) . fromIntegral
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
  onMouseLeftDown _ = Just $ \(Location (ax, ay)) ->
    when (ay == 0) $
      previewSongProgressAt ax
  onMouseLeftUp _ = Just $ \(Location (ax, ay)) ->
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

instance Drawable St EQGainBarsViewport where
  draw (EQGainBarsViewport path) st =
    W.viewport (mName $ EQGainBarsViewport path) Horizontal $
      W.hBox
        [ W.padRight (W.Pad 1) $ drawNamed st (EQGainBar path i)
        | i <- zipWith const [0 ..] (st ^. stCurrentEQ . eqBands)
        ]
  onMouseScrollUp (EQGainBarsViewport path) =
    Just $
      M.hScrollBy (viewportScroll (mName $ EQGainBarsViewport path)) (-eqGainBarsViewportStep)
  onMouseScrollDown (EQGainBarsViewport path) =
    Just $
      M.hScrollBy (viewportScroll (mName $ EQGainBarsViewport path)) eqGainBarsViewportStep
  parent (EQGainBarsViewport path) = Just . ParentName . mName $ ElementNode path
  variant (EQGainBarsViewport path) = pathVariant path

instance Drawable St EQGainBar where
  draw (EQGainBar _ i) st =
    case listToMaybe (drop i (st ^. stCurrentEQ . eqBands)) of
      Nothing ->
        W.emptyWidget
      Just band ->
        Widget Fixed Greedy $ do
          ctx <- getContext
          let totalHeight = max 5 (ctx ^. availHeightL)
              sliderHeight = max 3 (totalHeight - 2)
              gain = band ^. eqBandGainDb
              thumbY = gainBarThumbY sliderHeight gain
              zeroY = gainBarThumbY sliderHeight 0
              freqLabel = formatFrequencyLabel (band ^. eqBandFrequencyHz)
              renderRow y
                | y == thumbY =
                    W.withAttr (attrName "progressBarComplete") $
                      W.str " ╞█╡ "
                | y == zeroY =
                    W.withAttr (attrName "progressBarComplete") $
                      W.str "  ┼  "
                | between thumbY zeroY y =
                    W.withAttr (attrName "progressBarComplete") $
                      W.str "  │  "
                | otherwise =
                    W.withAttr (attrName "progressBarIncomplete") $
                      W.str "  │  "
          render $
            W.vBox $
              [ W.withAttr (attrName "header") $
                  W.str (centerText eqGainBarWidth (formatGainDb gain))
              ]
                <> [renderRow y | y <- [0 .. sliderHeight - 1]]
                <> [ W.withAttr (attrName "meta") $
                       W.str (centerText eqGainBarWidth freqLabel)
                   ]
  willReportExtent _ = True
  onMouseLeftDown (EQGainBar path i) =
    Just $ \(Location (_, ay)) ->
      updateEQGainBarAt path i ay
  onMouseLeftUp (EQGainBar path i) =
    Just $ \(Location (_, ay)) ->
      updateEQGainBarAt path i ay
  onMouseScrollUp' (EQGainBar _ i) =
    Just $ currentEQGainBarValue i >>= setEQGainBarValue i . (+ 1)
  onMouseScrollDown' (EQGainBar _ i) =
    Just $ currentEQGainBarValue i >>= setEQGainBarValue i . (subtract 1)
  parent (EQGainBar path _) = Just (ParentName $ mName $ EQGainBarsViewport path)
  variant (EQGainBar _ i) = i

instance Drawable St PlayButton where
  draw _ st =
    drawIconButton
      st
      (mName PlayButton)
      ( if st ^. stPlaying . psPaused
          then "|>"
          else "||"
      )
  onMouseLeftUp _ = Just $ \_ -> do
    stPlaying . psPaused %= not
    paused <- use $ stPlaying . psPaused
    sendRequest . MPDOperation . pure $
      MPD.pause paused
  parent _ = Just (ParentView MainView)

instance Drawable St RewindButton where
  draw _ st = drawIconButton st (mName RewindButton) "<<"
  onMouseLeftUp _ = Just $ \_ -> do
    current <- use $ stPlaying . psCurrentSong
    stPlaying . psPaused .= False
    sendRequest $ MPDOperation [MPD.play $ current >>= MPD.sgIndex]
  parent _ = Just (ParentView MainView)

instance Drawable St ForwardButton where
  draw _ st = drawIconButton st (mName ForwardButton) ">>"
  onMouseLeftUp _ = Just $ \_ -> do
    stPlaying . psPaused .= False
    sendRequest $ MPDOperation [MPD.next]
  parent _ = Just (ParentView MainView)

instance Drawable St OkButton where
  draw _ st = drawButton st (mName OkButton) "    OK    "
  onMouseLeftUp _ = Just $ \_ -> closeDialog
  parent _ = Just (ParentView SimpleDialog)

instance Drawable St SkipButton where
  draw _ st = drawButton st (mName SkipButton) "   SKIP   "
  onMouseLeftUp _ = Just $ \_ -> do
    closeDialog
    stDialog .? dsPage .= 1
  parent _ = Just (ParentView WelcomeDialog)

instance Drawable St NextButton where
  draw _ st = drawButton st (mName NextButton) "   NEXT   "
  onMouseLeftUp _ = Just $ \_ -> stDialog .? dsPage %= (+ 1)
  parent _ = Just (ParentView WelcomeDialog)

instance Drawable St FinishButton where
  draw _ st = drawButton st (mName FinishButton) "   FINISH   "
  onMouseLeftUp _ = Just $ \_ -> do
    closeDialog
    stDialog .? dsPage .= 1
  parent _ = Just (ParentView WelcomeDialog)

instance Drawable St PrevButton where
  draw _ st = drawButton st (mName PrevButton) "   PREV   "
  onMouseLeftUp _ = Just $ \_ -> stDialog .? dsPage %= subtract 1
  parent _ = Just (ParentView WelcomeDialog)

instance Drawable St ReverseOrderButton where
  draw _ st = drawIconButton st (mName ReverseOrderButton) "↑↓"
  onMouseLeftUp _ = Just $ \_ -> do
    queue <- use $ stPlaying . psCurrentQueue
    sendRequest . MPDOperation . pure $
      forM_ (reverseQueueMoves $ Vec.length queue) $
        uncurry MPD.move
    sendRequest $ SignalCurrentQueue
  parent _ = Just (ParentView MainView)

instance Drawable St ShuffleButton where
  draw _ st = drawIconButton st (mName ShuffleButton) "⇡⇣"
  onMouseLeftUp _ = Just $ \_ -> do
    sendRequest $ MPDOperation $ pure $ do
      MPD.shuffle Nothing
    sendRequest $ SignalCurrentQueue
  parent _ = Just (ParentView MainView)

instance Drawable St ClearButton where
  draw _ st = drawIconButton st (mName ClearButton) "><"
  onMouseLeftUp _ = Just $ \_ -> do
    sendRequest $ MPDOperation $ pure $ do
      MPD.clear
    sendRequest $ SignalCurrentQueue
  parent _ = Just (ParentView MainView)

instance Drawable St EQSwitch where
  draw n@(EQSwitch _) st = drawButton st (mName n) icon
   where
    icon
      | st ^. stIsTriggered (mName n) = " GO CURVE "
      | otherwise = " GO TWEAK "
  onMouseLeftUp n = Just $ \_ ->
    use (stIsTriggered (mName n)) >>= \case
      True -> unTrigger (mName n)
      False -> trigger (mName n)
  parent (EQSwitch path) = Just . ParentName . mName $ ElementNode path
  variant (EQSwitch path) = pathVariant path

reverseQueueMoves :: Int -> [(MPD.Position, MPD.Position)]
reverseQueueMoves queueLength =
  let lastPosition = fromIntegral (queueLength - 1)
   in [(lastPosition, fromIntegral target) | target <- [0 .. queueLength - 2]]

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

setVolumeBarValue :: Int -> EventM (MName St) St ()
setVolumeBarValue volume = do
  let clampedVolume = max 0 (min 100 volume)
  stConfig . csVolume .= fromIntegral clampedVolume
  sendRequest $ MPDOperation [MPD.setVolume (fromIntegral clampedVolume)]

updateEQGainBarAt :: ElementPath -> Int -> Int -> EventM (MName St) St ()
updateEQGainBarAt path bandIndex y =
  M.lookupExtent (mName $ EQGainBar path bandIndex) >>= \case
    Just extent -> do
      let (_, height) = extentSize extent
          sliderHeight = max 3 (height - 2)
          sliderY = max 0 (min (sliderHeight - 1) (y - 1))
          gain = gainBarValue sliderHeight sliderY
      setEQGainBarValue bandIndex gain
    Nothing ->
      pure ()

currentEQGainBarValue :: Int -> EventM (MName St) St Double
currentEQGainBarValue bandIndex = do
  currentId <- use (stConfig . csConfigs . cvEq)
  configs <- use (stConfig . csEQConfigs)
  pure $
    case Map.lookup currentId configs >>= listToMaybe . drop bandIndex . (^. eqBands) of
      Just band -> band ^. eqBandGainDb
      Nothing -> 0

setEQGainBarValue :: Int -> Double -> EventM (MName St) St ()
setEQGainBarValue bandIndex gain = do
  currentId <- use (stConfig . csConfigs . cvEq)
  stConfig . csEQConfigs %= Map.adjust adjustBand currentId
 where
  snappedGain =
    snapToTenths $
      clampValue (-eqGainBarNudgeLimitDb) eqGainBarNudgeLimitDb gain
  adjustBand =
    eqBands %~ zipWith updateBand [0 :: Int ..]
  updateBand i band
    | i == bandIndex = band & eqBandGainDb .~ snappedGain
    | otherwise = band

centerText :: Int -> String -> String
centerText width s =
  let clipped = take width s
      padding = max 0 (width - length clipped)
      left = padding `div` 2
      right = padding - left
   in replicate left ' ' <> clipped <> replicate right ' '
