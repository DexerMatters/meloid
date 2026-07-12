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
import Data.List (unsnoc)
import Lens.Micro
import Lens.Micro.Mtl
import Types
import Utils (extentHorizontalBounds, extentVerticalBounds, localToScreen, resizeRatio)
import Widgets.Elements.Common

data ElementScaffoldName = ElementScaffoldName ElementPath
  deriving (Show, Eq)

instance Drawable St ElementScaffoldName where
  draw (ElementScaffoldName path) st =
    case st ^. stLayoutElement path of
      Nothing -> W.emptyWidget
      Just element -> drawElementScaffold path element st
  parent (ElementScaffoldName path) = case path of
    [] -> Just (ParentView MainView)
    is -> ParentName . mName . ElementScaffoldName . fst <$> unsnoc is
  variant (ElementScaffoldName path) = pathVariant path
  willReportExtent _ = True
  onMouseLeftDown (ElementScaffoldName path) = Just $ resizeScaffoldBorder path

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
    Bd.border $ C.center $ W.str label

  drawElementContainer label =
    Bd.borderWithLabel (W.str (" " <> label <> " "))

resizeScaffoldBorder :: ElementPath -> Location -> EventM (MName St) St ()
resizeScaffoldBorder path location = do
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
      Just (parentPath, childIndex) ->
        use (stLayoutElement parentPath) >>= \case
          Just (EHBox _ children) ->
            dividerAt parentPath childIndex children x fst
          Just (EVBox _ children) ->
            dividerAt parentPath childIndex children y snd
          _ -> pure Nothing

  dividerAt parentPath childIndex children coordinate extentLength =
    M.lookupExtent (mName $ ElementScaffoldName path) >>= \case
      Nothing -> pure Nothing
      Just currentExtent ->
        let spanLength = extentLength $ extentSize currentExtent
            divider
              | coordinate == 0 && childIndex > 0 = Just (childIndex - 1)
              | coordinate == spanLength - 1 && childIndex + 1 < length children = Just childIndex
              | otherwise = Nothing
         in pure $ fmap (\index -> (parentPath, index)) divider

  resizeDivider parentPath divider =
    use (stLayoutElement parentPath) >>= \case
      Just EHBox{} -> resizeWith horizontal extentHorizontalBounds
      Just EVBox{} -> resizeWith vertical extentVerticalBounds
      _ -> pure ()
   where
    resizeWith axisCoordinate bounds =
      M.lookupExtent (mName $ ElementScaffoldName path) >>= \case
        Nothing -> pure ()
        Just currentExtent -> do
          leftExtent <- M.lookupExtent (mName $ ElementScaffoldName (parentPath <> [divider]))
          rightExtent <- M.lookupExtent (mName $ ElementScaffoldName (parentPath <> [divider + 1]))
          case (leftExtent, rightExtent) of
            (Just leftSibling, Just rightSibling) -> do
              let (start, leftEnd) = bounds leftSibling
                  (rightStart, end) = bounds rightSibling
                  activeCells = leftEnd - start + end - rightStart
                  ratio = resizeRatio activeCells (start, end) $ axisCoordinate (localToScreen currentExtent location)
              stConfig . csConfigs . cvLayout %= resizeLayoutDivider parentPath divider ratio
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
      (before, leftWeight : rightWeight : after) ->
        before <> [newLeft, newRight] <> after
       where
        total = leftWeight + rightWeight
        newLeft = total * ratio
        newRight = total - newLeft
      _ -> currentWeights
   where
    currentWeights = layoutWeights weights (length children)

  modifyChild index f children =
    case splitAt index children of
      (before, child : after) -> before <> (f child : after)
      _ -> children
