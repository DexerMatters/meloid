{-# LANGUAGE LambdaCase #-}

{- | This module provides scaffolds for the application.
Scaffolds are the top-level widgets of the application which displays the
layout framework. They can be dragged to resize, clicked to launch a menu
where the user can change the layout. They can only be displayed in
`Edit` mode.
-}
module Widgets.Elements.Scaffold (
  ElementScaffoldName (..),
) where

import Brick hiding (Horizontal, Vertical)
import Brick qualified as W hiding (Horizontal, Vertical)
import Brick.Main qualified as M
import Brick.Widgets.Border qualified as Bd
import Brick.Widgets.Center qualified as C
import Control.Monad (filterM)
import Data.Bool (bool)
import Data.List (unsnoc)
import Data.Maybe (isJust)
import Lens.Micro
import Lens.Micro.Mtl
import Types
import Utils (clampValue, extentHorizontalBounds, extentVerticalBounds, localToScreen, resizeRatio)
import Widgets.Elements.Common

-- | The edit-mode drawable identity of an element at a layout path.
data ElementScaffoldName = ElementScaffoldName ElementPath
  deriving (Show, Eq)

-- | No draggable child may occupy less than this share of its parent box.
minimumScaffoldShare :: Double
minimumScaffoldShare = 0.15

instance Drawable St ElementScaffoldName where
  draw (ElementScaffoldName path) st =
    case st ^. stLayoutElement path of
      Nothing -> W.emptyWidget
      Just element ->
        W.withAttr (attrName "text") $
          drawElementScaffold path element st
  parent (ElementScaffoldName path) = Just . ParentName . mName $ ElementNode path
  variant (ElementScaffoldName path) = pathVariant path
  willReportExtent _ = True
  onMouseLeftDown (ElementScaffoldName path) = Just $ dispatchMouseDown path
  onMouseLeftUp (ElementScaffoldName path) = Just $ dispatchMouseUp path

-- | Draw an outlined layout subtree for direct visual editing.
drawElementScaffold :: ElementPath -> LayoutElement -> St -> Widget (MName St)
drawElementScaffold path element st =
  case element of
    EHBox weights children ->
      drawElementContainer (formatElementName element) $
        layoutChildren Horizontal weights (gaps children) $
          fmap (\widget -> (True, widget)) $
            drawChildren children
    EVBox weights children ->
      drawElementContainer (formatElementName element) $
        layoutChildren Vertical weights (gaps children) $
          fmap (\widget -> (True, widget)) $
            drawChildren children
    ETabs children ->
      drawElementContainer (formatElementName element) $
        layoutChildren Vertical Nothing (gaps children) $
          fmap (\widget -> (True, widget)) $
            drawChildren children
    leaf ->
      drawElementPlaceholder (formatElementName leaf)
 where
  drawChildren children =
    drawNamed st . ElementScaffoldName <$> childPaths children path

  gaps children = replicate (max 0 $ length children - 1) True

  drawElementPlaceholder label =
    W.withBorderStyle scaffoldBorderStyle $
      Bd.border . C.center $
        W.str label

  drawElementContainer label =
    W.withBorderStyle scaffoldBorderStyle
      . Bd.borderWithLabel (W.str (" " <> label <> " "))

-- | Open the element menu after a click that did not resize a divider.
dispatchMouseUp :: ElementPath -> Location -> EventM (MName St) St ()
dispatchMouseUp path _ = do
  isResize <- use stLayoutResize
  element <- use (stLayoutElement path)
  case (isJust isResize, element) of
    (True, _) -> pure ()
    (False, Nothing) -> pure ()
    (False, Just e) ->
      createMenuForElement path e
        >>= openMenu
          (mName $ ElementScaffoldName path)

createSelectionMenu ::
  (LayoutElement -> Bool) ->
  (LayoutElement -> EventM (MName St) St ()) ->
  [MenuWidget]
createSelectionMenu p action =
  let layoutWidgets =
        filter p $
          [ EHBox (Just [0.5, 0.5]) [EPlaceholder, EPlaceholder]
          , EVBox (Just [0.5, 0.5]) [EPlaceholder, EPlaceholder]
          , ETabs [EPlaceholder, EPlaceholder]
          ]
   in let panelWidgets =
            filter p $
              [EAlbumList, ETrackList, ESongInfo, ECurrentQueue, EEqualizer, EPlaceholder]
       in layoutWidgets <|> MWHeader "Containers"
            <> (layoutWidgets <&> \w -> MWButton (formatElementName w) (action w))
            <> panelWidgets <|> MWHeader "Panels"
            <> (panelWidgets <&> \w -> MWButton (formatElementName w) (action w))
 where
  [] <|> _ = []
  _ <|> a = [a]

createMenuForElement :: ElementPath -> LayoutElement -> EventM (MName St) St [MenuWidget]
createMenuForElement path e =
  (MWHeader (formatElementName e) :)
    <$> concat
    <$> sequence
      [ canDelete #>> MWButton "Remove" delete
      , pure . pure $ MWSubmenu "Replace" (createSelectionMenu (const True) (replaceTo))
      , canInsert #>> MWSubmenu "Insert" (createSelectionMenu (const True) (insert True))
      , canInsert #>> MWSubmenu "Insert Last" (createSelectionMenu (const True) (insert False))
      , canExtend #>> MWSubmenu "Extend" (createSelectionMenu (const True) (extend True))
      , canExtend #>> MWSubmenu "Extend Last" (createSelectionMenu (const True) (extend False))
      ]
 where
  m #>> w = filterM (const m) [w]

  canDelete = case parentPath path of
    Nothing -> pure False
    Just p ->
      use (stLayoutElement p) >>= \case
        Just (EHBox _ children) -> pure $ length children > 2
        Just (EVBox _ children) -> pure $ length children > 2
        Just (ETabs children) -> pure $ length children > 2
        _ -> pure False

  canExtend = pure $ path /= []

  canInsert = case e of
    EHBox{} -> pure True
    EVBox{} -> pure True
    ETabs{} -> pure True
    _ -> pure False

  replaceTo dest =
    stConfig . csConfigs . cvLayout %= replaceAt path
   where
    replaceAt [] _ = dest
    replaceAt (index : rest) element =
      case element of
        EHBox weights children -> EHBox weights (replaceChild index rest children)
        EVBox weights children -> EVBox weights (replaceChild index rest children)
        ETabs children -> ETabs (replaceChild index rest children)
        leaf -> leaf

    replaceChild index rest children =
      case splitAt index children of
        (before, child : after) -> before <> (replaceAt rest child : after)
        _ -> children

  delete :: EventM (MName St) St ()
  delete =
    stConfig . csConfigs . cvLayout %= deleteAt path
   where
    deleteAt [] _ = EPlaceholder
    deleteAt [index] element = deleteChild index element
    deleteAt (index : rest) element =
      case element of
        EHBox weights children -> EHBox weights (replaceChild index rest children)
        EVBox weights children -> EVBox weights (replaceChild index rest children)
        ETabs children -> ETabs (replaceChild index rest children)
        leaf -> leaf

    deleteChild index = \case
      EHBox weights children -> EHBox (removeAt index <$> weights) (removeAt index children)
      EVBox weights children -> EVBox (removeAt index <$> weights) (removeAt index children)
      ETabs children -> ETabs (removeAt index children)
      leaf -> leaf

    replaceChild index rest children =
      case splitAt index children of
        (before, child : after) -> before <> (deleteAt rest child : after)
        _ -> children

    removeAt index values =
      case splitAt index values of
        (before, _ : after) -> before <> after
        _ -> values

  extend :: Bool -> LayoutElement -> EventM (MName St) St ()
  extend before element =
    case unsnoc path of
      Nothing -> pure ()
      Just (p, index) ->
        stConfig
          . csConfigs
          . cvLayout
          %= modifyContainerAt p (addToContainer (index + bool 1 0 before) element)

  insert :: Bool -> LayoutElement -> EventM (MName St) St ()
  insert before element =
    stConfig
      . csConfigs
      . cvLayout
      %= modifyContainerAt
        path
        ( \container ->
            addToContainer (bool (childCount container) 0 before) element container
        )

  modifyContainerAt target f = go target
   where
    go [] element = f element
    go (index : rest) element =
      case element of
        EHBox weights children -> EHBox weights (modifyChild index rest children)
        EVBox weights children -> EVBox weights (modifyChild index rest children)
        ETabs children -> ETabs (modifyChild index rest children)
        leaf -> leaf

    modifyChild index rest children =
      case splitAt index children of
        (before, child : after) -> before <> (go rest child : after)
        _ -> children

  addToContainer index element = \case
    EHBox weights children ->
      EHBox (insertWeight index weights children) (insertAt index element children)
    EVBox weights children ->
      EVBox (insertWeight index weights children) (insertAt index element children)
    ETabs children ->
      ETabs (insertAt index element children)
    leaf -> leaf

  childCount = \case
    EHBox _ children -> length children
    EVBox _ children -> length children
    ETabs children -> length children
    _ -> 0

  insertWeight index weights children =
    case weights of
      Nothing -> Nothing
      Just configured ->
        let current = layoutWeights (Just configured) (length children)
            newWeight = case current of
              [] -> 1
              _ -> sum current / fromIntegral (length current)
         in Just (insertAt index newWeight current)

  insertAt index element values =
    let (before, after) = splitAt index values
     in before <> (element : after)

-- | Start or continue a drag when the pointer is on a container divider.
dispatchMouseDown :: ElementPath -> Location -> EventM (MName St) St ()
dispatchMouseDown path location = do
  use stLayoutResize >>= \case
    Just resize -> uncurry resizeDivider resize
    Nothing ->
      borderDivider >>= \case
        Nothing -> pure ()
        Just resize -> do
          stLayoutResize .= Just resize
          uncurry resizeDivider resize
 where
  Location (x, y) = location
  horizontal (Location (coordinate, _)) = coordinate
  vertical (Location (_, coordinate)) = coordinate

  borderDivider =
    case unsnoc path of
      Nothing -> pure Nothing
      Just (parentPath', childIndex) ->
        use (stLayoutElement parentPath') >>= \case
          Just (EHBox _ children) ->
            dividerAt parentPath' childIndex children x fst
          Just (EVBox _ children) ->
            dividerAt parentPath' childIndex children y snd
          _ -> pure Nothing

  dividerAt parentPath' childIndex children coordinate extentLength =
    M.lookupExtent (mName $ ElementScaffoldName path) >>= \case
      Nothing -> pure Nothing
      Just currentExtent ->
        let spanLength = extentLength $ extentSize currentExtent
            divider
              | coordinate == 0 && childIndex > 0 = Just (childIndex - 1)
              | coordinate == spanLength - 1 && childIndex + 1 < length children = Just childIndex
              | otherwise = Nothing
         in pure $ fmap (\index -> (parentPath', index)) divider

  resizeDivider parentPath' divider =
    use (stLayoutElement parentPath') >>= \case
      Just EHBox{} -> resizeWith horizontal extentHorizontalBounds
      Just EVBox{} -> resizeWith vertical extentVerticalBounds
      _ -> pure ()
   where
    resizeWith axisCoordinate bounds =
      M.lookupExtent (mName $ ElementScaffoldName path) >>= \case
        Nothing -> pure ()
        Just currentExtent -> do
          leftExtent <- M.lookupExtent (mName $ ElementScaffoldName (parentPath' <> [divider]))
          rightExtent <- M.lookupExtent (mName $ ElementScaffoldName (parentPath' <> [divider + 1]))
          case (leftExtent, rightExtent) of
            (Just leftSibling, Just rightSibling) -> do
              let (start, leftEnd) = bounds leftSibling
                  (rightStart, end) = bounds rightSibling
                  activeCells = leftEnd - start + end - rightStart
                  ratio = resizeRatio activeCells (start, end) $ axisCoordinate (localToScreen currentExtent location)
              stConfig . csConfigs . cvLayout %= resizeLayoutDivider parentPath' divider ratio
            _ -> pure ()

resizeLayoutDivider :: [Int] -> Int -> Double -> LayoutElement -> LayoutElement
resizeLayoutDivider path divider ratio = go path
 where
  go [] = resizeBox
  go (index : rest) = \case
    EHBox weights children -> EHBox weights (modifyChild index (go rest) children)
    EVBox weights children -> EVBox weights (modifyChild index (go rest) children)
    ETabs children -> ETabs (modifyChild index (go rest) children)
    element -> element

  resizeBox = \case
    EHBox weights children -> EHBox (Just $ resizeWeights weights children) children
    EVBox weights children -> EVBox (Just $ resizeWeights weights children) children
    element -> element

  resizeWeights weights children =
    case splitAt divider currentWeights of
      (before, leftWeight : rightWeight : after)
        | minimumPairRatio > 0.5 ->
            currentWeights
        | otherwise ->
            before <> [newLeft, newRight] <> after
       where
        pairWeight = leftWeight + rightWeight
        minimumPairRatio = minimumScaffoldShare * sum currentWeights / pairWeight
        constrainedRatio = clampValue minimumPairRatio (1 - minimumPairRatio) ratio
        newLeft = pairWeight * constrainedRatio
        newRight = pairWeight - newLeft
      _ -> currentWeights
   where
    currentWeights = layoutWeights weights (length children)

  modifyChild index f children =
    case splitAt index children of
      (before, child : after) -> before <> (f child : after)
      _ -> children
