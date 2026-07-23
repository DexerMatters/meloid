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
  ReverseOrderButton (..),
  ShuffleButton (..),
  ClearButton (..),
  EQSwitch (..),
  EQApplyButton (..),
  EQSaveButton (..),
) where

import Brick
import Brick.Main qualified as M
import Brick.Widgets.Core qualified as W
import Control.Monad (forM_, when)
import Data.Functor (($>))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Vector qualified as Vec
import Lens.Micro ((&), (.~), (^.))
import Lens.Micro.Mtl
import Network.MPD qualified as MPD
import Types
import Types.Configs (Configs (..), EQConfigs (..), saveWithPanic)
import Utils
import Widgets.Common
import Widgets.Elements.Common (ElementNode (..), ElementPath, pathVariant)
import Widgets.Lists (stCurrentEQ, stCurrentEQIndex)

data VolumeBar = VolumeBar

data SongProgressBar = SongProgressBar

data EQGainBarsViewport = EQGainBarsViewport ElementPath

data EQGainBar = EQGainBar ElementPath Int

data PlayButton = PlayButton

data RewindButton = RewindButton

data ForwardButton = ForwardButton

data ReverseOrderButton = ReverseOrderButton

data ShuffleButton = ShuffleButton

data ClearButton = ClearButton

data EQSwitch = EQSwitch ElementPath

data EQApplyButton = EQApplyButton ElementPath

data EQSaveButton = EQSaveButton ElementPath

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
  focusBinding _ _ = Just $ FocusAdjust beginVolumeAdjustment

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
  focusBinding _ _ = Just $ FocusAdjust beginSongProgressAdjustment

instance Drawable St EQGainBarsViewport where
  draw (EQGainBarsViewport path) st =
    W.viewport (mName $ EQGainBarsViewport path) Horizontal $
      W.hBox
        [ W.padRight (W.Pad 1) $ drawNamed st (EQGainBar path i)
        | i <- zipWith const [0 ..] (st ^. stCurrentEQ . eqGains)
        ]
  onMouseScrollUp (EQGainBarsViewport path) =
    Just $
      M.hScrollBy (viewportScroll (mName $ EQGainBarsViewport path)) (-eqGainBarsViewportStep)
  onMouseScrollDown (EQGainBarsViewport path) =
    Just $
      M.hScrollBy (viewportScroll (mName $ EQGainBarsViewport path)) eqGainBarsViewportStep
  parent (EQGainBarsViewport path) = Just . ParentName . mName $ ElementNode path
  variant (EQGainBarsViewport path) = pathVariant path
  focusChildren (EQGainBarsViewport path) st =
    [ mName $ EQGainBar path i
    | i <- zipWith const [0 ..] (st ^. stCurrentEQ . eqGains)
    ]

instance Drawable St EQGainBar where
  draw (EQGainBar _ i) st =
    case listToMaybe (drop i (st ^. stCurrentEQ . eqGains)) of
      Nothing ->
        W.emptyWidget
      Just gain ->
        Widget Fixed Greedy $ do
          ctx <- getContext
          let totalHeight = max 5 (ctx ^. availHeightL)
              sliderHeight = max 3 (totalHeight - 2)
              thumbY = gainBarThumbY sliderHeight gain
              zeroY = gainBarThumbY sliderHeight 0
              freqLabel = formatFrequencyLabel (eqFrequencies !! i)
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
  focusBinding (EQGainBar _ i) _ = Just $ FocusAdjust (beginEQGainAdjustment i)

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
    stopped <- use $ stPlaying . psStopped
    if stopped
      then do
        current <- use $ stPlaying . psCurrentSong
        stPlaying . psPaused .= False
        stPlaying . psStopped .= False
        sendRequest $ MPDOperation [MPD.play $ current >>= MPD.sgIndex]
      else do
        stPlaying . psPaused %= not
        paused <- use $ stPlaying . psPaused
        sendRequest . MPDOperation . pure $ MPD.pause paused
  parent _ = Just (ParentView MainView)

instance Drawable St RewindButton where
  draw _ st = drawIconButton st (mName RewindButton) "<<"
  onMouseLeftUp _ = Just $ \_ -> do
    current <- use $ stPlaying . psCurrentSong
    stPlaying . psPaused .= False
    stPlaying . psStopped .= False
    sendRequest $ MPDOperation [MPD.play $ current >>= MPD.sgIndex]
  parent _ = Just (ParentView MainView)

instance Drawable St ForwardButton where
  draw _ st = drawIconButton st (mName ForwardButton) ">>"
  onMouseLeftUp _ = Just $ \_ -> do
    stPlaying . psPaused .= False
    stPlaying . psStopped .= False
    sendRequest $ MPDOperation [MPD.next]
  parent _ = Just (ParentView MainView)

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
  draw n st = drawButton st (mName n) icon
   where
    icon
      | st ^. stIsTriggered (mName n) = " GO CURVE "
      | otherwise = " GO TWEAK "
  onMouseLeftUp n = Just $ \_ ->
    use (stIsTriggered $ mName n) >>= \case
      True -> untrigger $ mName n
      False -> trigger $ mName n
  parent (EQSwitch path) = Just . ParentName . mName $ ElementNode path
  variant (EQSwitch path) = pathVariant path

instance Drawable St EQApplyButton where
  draw n st = drawButton st (mName n) " APPLY "
  onMouseLeftUp _ = Just $ \_ -> do
    EQConfigValue presets <- use $ stConfig . csEQConfigs
    use stCurrentEQIndex >>= \case
      Just index | index < Map.size presets -> do
        let (eqId, preset) = Map.elemAt index presets
        config <- use $ stConfig . csConfigs
        saveWithPanic Configs (config & cvEq .~ eqId) >>= \case
          True -> do
            stConfig . csConfigs . cvEq .= eqId
            sendRequest $ ApplyEQ preset
          False -> pure ()
      _ -> pure ()
  parent (EQApplyButton path) = Just . ParentName . mName $ ElementNode path
  variant (EQApplyButton path) = pathVariant path

instance Drawable St EQSaveButton where
  draw n st =
    W.withAttr (attrName "unsaved") $
      drawButton st (mName n) " SAVE "

  onMouseLeftUp _ = Just $ \_ -> do
    configs <- use (stConfig . csEQConfigs)
    saveWithPanic EQConfigs configs >>= \case
      True -> stUnsaved . usEQ .= Nothing
      False -> pure ()

  parent (EQSaveButton path) = Just . ParentName . mName $ ElementNode path
  variant (EQSaveButton path) = pathVariant path

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
currentEQGainBarValue bandIndex =
  fromMaybe 0 . listToMaybe . drop bandIndex <$> use (stCurrentEQ . eqGains)

setEQGainBarValue :: Int -> Double -> EventM (MName St) St ()
setEQGainBarValue bandIndex gain = do
  EQConfigValue configs <- use (stConfig . csEQConfigs)
  use stCurrentEQIndex >>= \case
    Just currentIx | currentIx < Map.size configs -> do
      let (currentId, EQConfigSpecs gains) = Map.elemAt currentIx configs
          updated = EQConfigSpecs $ zipWith updateGain [0 ..] gains
      stUnsaved . usEQ .= Just currentIx
      stConfig . csEQConfigs %= \(EQConfigValue configs') ->
        EQConfigValue $ Map.insert currentId updated configs'
    _ -> pure ()
 where
  snappedGain =
    snapToTenths $
      clampValue (-eqGainBarNudgeLimitDb) eqGainBarNudgeLimitDb gain
  updateGain i oldGain
    | i == bandIndex = snappedGain
    | otherwise = oldGain

beginVolumeAdjustment :: EventM (MName St) St (FocusTransaction St)
beginVolumeAdjustment = do
  initialVolume <- use (stConfig . csVolume)
  pure $
    FocusTransaction
      { adjustFocus = \case
          FocusUp -> nudge 1
          FocusRight -> nudge 1
          FocusDown -> nudge (-1)
          FocusLeft -> nudge (-1)
      , commitFocusAdjustment = pure ()
      , cancelFocusAdjustment = setVolumeBarValue (fromIntegral initialVolume)
      }
 where
  nudge delta =
    use (stConfig . csVolume) >>= \volume ->
      setVolumeBarValue (fromIntegral volume + delta) $> True

beginSongProgressAdjustment :: EventM (MName St) St (FocusTransaction St)
beginSongProgressAdjustment = do
  originalPreview <- use stSongProgressPreview
  pure $
    FocusTransaction
      { adjustFocus = adjustProgress
      , commitFocusAdjustment = commitSongProgressPreview
      , cancelFocusAdjustment = stSongProgressPreview .= originalPreview
      }
 where
  adjustProgress direction =
    use stShownCurrentTime >>= \case
      Nothing -> pure False
      Just (current, total)
        | total <= 0 -> pure False
        | otherwise -> do
            let delta = case direction of
                  FocusLeft -> -5
                  FocusRight -> 5
                  FocusUp -> 30
                  FocusDown -> -30
                next = max 0 $ min total (current + delta)
            stSongProgressPreview .= Just (next, total)
            pure True

commitSongProgressPreview :: EventM (MName St) St ()
commitSongProgressPreview = do
  preview' <- use stSongProgressPreview
  stSongProgressPreview .= Nothing
  case preview' of
    Nothing -> pure ()
    Just (target, total) -> do
      stPlaying . psCurrentTime .= Just (target, total)
      use stCurrentSongPos >>= mapM_ (\position -> sendRequest $ MPDOperation [MPD.seek position target])

beginEQGainAdjustment :: Int -> EventM (MName St) St (FocusTransaction St)
beginEQGainAdjustment bandIndex = do
  originalConfigs <- use (stConfig . csEQConfigs)
  originalUnsaved <- use (stUnsaved . usEQ)
  pure $
    FocusTransaction
      { adjustFocus = \case
          FocusUp -> nudge 1
          FocusDown -> nudge (-1)
          _ -> pure False
      , commitFocusAdjustment = pure ()
      , cancelFocusAdjustment = do
          stConfig . csEQConfigs .= originalConfigs
          stUnsaved . usEQ .= originalUnsaved
      }
 where
  nudge delta =
    currentEQGainBarValue bandIndex >>= \gain ->
      setEQGainBarValue bandIndex (gain + delta) $> True

centerText :: Int -> String -> String
centerText width s =
  let clipped = take width s
      padding = max 0 (width - length clipped)
      left = padding `div` 2
      right = padding - left
   in replicate left ' ' <> clipped <> replicate right ' '
