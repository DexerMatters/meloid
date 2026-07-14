-- | Top-level layer declarations for the Brick interface.
module Widgets.Layer (
  LayerName (..),
  activeLayerNames,
  activeOccluderNames,
) where

import Brick.Widgets.Center qualified as C
import Data.Maybe (mapMaybe)
import Lens.Micro ((^.))
import Types
import Widgets.Lists (drawMenuLayer)
import Widgets.Views (drawDialogView, drawView)

data LayerName
  = ViewLayer ViewName
  | DialogLayer ViewName
  | MenuLayer

instance Drawable St LayerName where
  draw (ViewLayer view) st = drawView view st
  draw (DialogLayer view) st = C.centerLayer $ drawDialogView view st
  draw MenuLayer st = drawMenuLayer st
  willReportExtent (DialogLayer _) = True
  willReportExtent MenuLayer = True
  willReportExtent _ = False
  layerSurface layer@(DialogLayer _) = Just (mName layer)
  layerSurface layer@MenuLayer = Just (mName layer)
  layerSurface _ = Nothing
  variant (ViewLayer view) = viewIndex view
  variant (DialogLayer view) = 100 + viewIndex view
  variant MenuLayer = 200

-- | Top-level layers in Brick's topmost-first order.
activeLayerNames :: St -> [MName St]
activeLayerNames st =
  menuLayer
    <> maybe [] (pure . mName . DialogLayer) (st ^. stDialogView)
    <> maybe [] (pure . mName . ViewLayer) (st ^. stCurrentView)
 where
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
viewIndex WelcomeDialog = 2
viewIndex SimpleDialog = 3
