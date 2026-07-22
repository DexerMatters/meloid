-- | Top-level layer declarations for the Brick interface.
module Widgets.Layer (
  LayerName (..),
  activeLayerNames,
  activeFocusLayerNames,
  activeOccluderNames,
) where

import Brick.Widgets.Center qualified as C
import Data.Maybe (isJust, mapMaybe)
import Lens.Micro ((^.))
import Types
import Widgets.Dialogs (dialogFocusChildren, drawDialog)
import Widgets.Lists (drawMenuLayer, menuFocusChildren)
import Widgets.Views (drawView, viewFocusChildren)

data LayerName
  = ViewLayer ViewName
  | DialogLayer
  | MenuLayer

instance Drawable St LayerName where
  draw (ViewLayer view) st = drawView view st
  draw DialogLayer st = C.centerLayer $ drawDialog st
  draw MenuLayer st = drawMenuLayer st
  willReportExtent DialogLayer = True
  willReportExtent MenuLayer = True
  willReportExtent _ = False
  layerSurface DialogLayer = Just (mName DialogLayer)
  layerSurface layer@MenuLayer = Just (mName layer)
  layerSurface _ = Nothing
  focusChildren (ViewLayer view) st = viewFocusChildren view st
  focusChildren DialogLayer st = dialogFocusChildren st
  focusChildren MenuLayer st = menuFocusChildren st
  variant (ViewLayer view) = viewIndex view
  variant DialogLayer = 100
  variant MenuLayer = 200

-- | Top-level layers in Brick's topmost-first order.
activeLayerNames :: St -> [MName St]
activeLayerNames = activeFocusLayerNames

-- | Layers that can own keyboard focus.
activeFocusLayerNames :: St -> [MName St]
activeFocusLayerNames st =
  menuLayer
    <> dialogLayer
    <> maybe [] (pure . mName . ViewLayer) (st ^. stCurrentView)
 where
  dialogLayer
    | isJust (st ^. stDialog) = [mName DialogLayer]
    | otherwise = []
  menuLayer
    | null (st ^. stMenu . msWidgets) = []
    | otherwise = [mName MenuLayer]

-- | Extent-reporting widgets that cover lower layers.
activeOccluderNames :: St -> [MName St]
activeOccluderNames =
  mapMaybe (named layerSurface) . activeLayerNames

viewIndex :: ViewName -> Int
viewIndex MainView = 0
viewIndex DebugView = 1
