{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- | Identity and widget dispatch primitives.
This module keeps the naming layer state-free so it can stay
cycle-free and be reused by the rest of the type tree.
-}
module Types.Identity (
  MName (..),
  ParentRef (..),
  Drawable (..),
  NameKey,
  ViewName (..),
  placeholderName,
  mName,
  drawMName,
  drawNamed,
  castMName,
  nameAncestry,
  named,
) where

import Brick qualified as B
import Brick.Types (EventM, Location, Widget)
import Data.Maybe (isJust)
import Data.Proxy (Proxy (Proxy))
import Data.Typeable (Typeable, cast)
import Type.Reflection (SomeTypeRep, someTypeRep)

data MName st where
  MName :: (Typeable a, Drawable st a) => a -> MName st

{- | A parent relationship for a named widget.
Widget ancestry can point to either another widget or a view root.
-}
data ParentRef st
  = ParentName (MName st)
  | ParentView ViewName

{- | A widget that can be drawn and optionally handle mouse
events. Instances stay close to the concrete widget types
so behavior remains local and declarative.
-}
class (Typeable a) => Drawable st a | a -> st where
  draw :: a -> st -> Widget (MName st)
  willReportExtent :: a -> Bool
  willReportExtent _ = False
  layerSurface :: a -> Maybe (MName st)
  layerSurface _ = Nothing
  onMouseLeftDown :: a -> Maybe (Location -> EventM (MName st) st ())
  onMouseLeftDown _ = Nothing
  onMouseLeftUp :: a -> Maybe (Location -> EventM (MName st) st ())
  onMouseLeftUp _ = Nothing
  onMouseDoubleClick :: a -> Maybe (Location -> EventM (MName st) st ())
  onMouseDoubleClick _ = Nothing
  onMouseRightUp :: a -> Maybe (Location -> EventM (MName st) st ())
  onMouseRightUp _ = Nothing
  onMouseScrollUp :: a -> Maybe (EventM (MName st) st ())
  onMouseScrollUp _ = Nothing
  onMouseScrollDown :: a -> Maybe (EventM (MName st) st ())
  onMouseScrollDown _ = Nothing
  onMouseScrollUp' :: a -> Maybe (EventM (MName st) st ())
  onMouseScrollUp' _ = Nothing
  onMouseScrollDown' :: a -> Maybe (EventM (MName st) st ())
  onMouseScrollDown' _ = Nothing
  parent :: a -> Maybe (ParentRef st)
  parent _ = Nothing
  variant :: a -> Int
  variant _ = 0

data PlaceholderName st = PlaceholderName

instance (Typeable st) => Drawable st (PlaceholderName st) where
  draw _ _ = B.emptyWidget

placeholderName :: forall st. (Typeable st) => MName st
placeholderName = mName (PlaceholderName @st)

{- | A stable comparison key for widget identity.
The key uses the concrete widget type, its variant, and
its parent chain.
-}
data NameKey = NameKey SomeTypeRep Int (Maybe NameKey)
  deriving (Eq, Ord, Show)

-- | The top-level view names used as ancestry roots.
data ViewName
  = MainView
  | DebugView
  | WelcomeDialog
  | SimpleDialog
  deriving (Show, Eq, Ord)

{- | Build the comparison key for a widget name.
The parent chain is part of the key so repeated entries remain
distinct.
-}
nameKey :: forall st. MName st -> NameKey
nameKey (MName (a :: a)) =
  NameKey
    (someTypeRep (Proxy @a))
    (variant a)
    (parentKey <$> parent a)
 where
  parentKey :: ParentRef st -> NameKey
  parentKey (ParentName n) = nameKey n
  parentKey (ParentView v) = viewKey v

viewKey :: ViewName -> NameKey
viewKey v =
  NameKey
    (someTypeRep (Proxy @ViewName))
    (viewVariant v)
    Nothing

viewVariant :: ViewName -> Int
viewVariant MainView = 0
viewVariant DebugView = 1
viewVariant WelcomeDialog = 2
viewVariant SimpleDialog = 3

instance Eq (MName st) where
  a == b = nameKey a == nameKey b

instance Ord (MName st) where
  compare a b = compare (nameKey a) (nameKey b)

instance Show (MName st) where
  showsPrec d = showsPrec d . nameKey

mName :: (Typeable a, Drawable st a) => a -> MName st
mName = MName

castMName :: forall a st. (Typeable a) => MName st -> Maybe a
castMName (MName a) = cast a

eval :: MName st -> st -> Widget (MName st)
eval name@(MName a) st =
  (if hasMouseHandler a then B.clickable name else id)
    ((if willReportExtent a then B.reportExtent name else id) (draw a st))

hasMouseHandler :: (Drawable st a) => a -> Bool
hasMouseHandler a =
  isJust (onMouseLeftDown a)
    || isJust (onMouseLeftUp a)
    || isJust (onMouseDoubleClick a)
    || isJust (onMouseRightUp a)
    || isJust (onMouseScrollUp a)
    || isJust (onMouseScrollDown a)
    || isJust (onMouseScrollUp' a)
    || isJust (onMouseScrollDown' a)

-- | Draw an already existential widget name.
drawMName :: st -> MName st -> Widget (MName st)
drawMName = flip eval

-- | Turn a concrete widget value into a named Brick widget.
drawNamed :: (Typeable a, Drawable st a) => st -> a -> Widget (MName st)
drawNamed st = drawMName st . mName

parentRef :: MName st -> Maybe (ParentRef st)
parentRef (MName a) = parent a

{- | Return the ancestry chain for a widget name.
This is used for dispatch and lookup along parent paths.
-}
nameAncestry :: MName st -> [MName st]
nameAncestry name =
  name
    : case parentRef name of
      Just (ParentName p) -> nameAncestry p
      _ -> []

-- | Helper to dispatch on an existential `MName`.
named :: forall st r. (forall n. (Drawable st n) => n -> r) -> MName st -> r
named f = \case MName n -> f n
