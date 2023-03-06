{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :  XMonad.Layout.Inspect
-- Description :  Inspect layout state.
-- Copyright   :  (c) 2020 Tomáš Janoušek <tomi@nomi.cz>
-- License     :  BSD3
--
-- Maintainer  :  Tomáš Janoušek <tomi@nomi.cz>
-- Stability   :  experimental
-- Portability :  unknown
--
-- Inspect a workspace's layout state. Useful for accessing state contained in
-- layout modifiers.

module XMonad.Layout.Inspect (
    -- * Usage
    -- $usage
    inspectCurrent,
    inspectTag,
    inspectWorkspace,

    -- * The 'InspectLayout' class
    InspectResult,
    InspectLayout(..),
    ) where

import Data.List (find)
import Type.Reflection

import XMonad
import qualified XMonad.StackSet as W

-- $usage
-- This module provides a way for layout authors to make workspace layout state
-- accessible to end-users.
--
-- Best explained by example, suppose you've written a layout modifier:
--
-- > newtype Foo a = Foo String deriving (Read, Show)
-- > instance LayoutModifier Foo a
-- > foo :: LayoutClass l a => String -> l a -> ModifiedLayout Foo l a
-- > foo = ModifiedLayout . Foo
--
-- To allow users to inspect the string contained in a given workspace's layout,
-- first import this module:
--
-- > import XMonad.Layout.Inspect
--
-- Then define instances for 'InspectResult' and 'InspectLayout', and implement
-- getFoo in terms of 'inspectWorkspace':
--
-- > data GetFoo = GetFoo
-- > type instance InspectResult GetFoo = Alt Maybe String
-- >
-- > instance InspectLayout GetFoo Foo a where
-- >   inspectLayout GetFoo (Foo s) = pure s
-- >
-- > getFoo :: (LayoutClass l Window, InspectLayout GetFoo l Window)
-- >        => l Window -> WindowSpace -> Maybe String
-- > getFoo l = getAlt . inspectWorkspace l GetFoo
--
-- An end-user can then call getFoo by passing it the appropriate layout and a
-- given workspace:
--
-- > myLayoutHook = ...
-- >
-- > main = xmonad $ def { layoutHook = myLayoutHook, ...
-- >
-- > xFoo :: WindowSpace -> X ()
-- > xFoo wsp = do
-- >   case getFoo myLayoutHook wsp of
-- >     Nothing -> pure ()
-- >     Just s -> xmessage s

-- | Inspect the layout of the currently focused workspace.
inspectCurrent :: (LayoutClass l Window, InspectLayout i l Window)
               => l Window -> i -> X (InspectResult i)
inspectCurrent l i = gets (inspectWorkspace l i . w)
    where w = W.workspace . W.current . windowset

-- | Inspect the layout of the workspace with a specified tag.
inspectTag :: (LayoutClass l Window, InspectLayout i l Window)
           => l Window -> i -> WorkspaceId
           -> X (Maybe (InspectResult i))
inspectTag l i t = gets (fmap (inspectWorkspace l i) . mw)
    where mw = find ((t==) . W.tag) . W.workspaces . windowset

-- | Inspect the layout of a specified workspace.
inspectWorkspace :: (LayoutClass l Window, InspectLayout i l Window)
                 => l Window -> i -> WindowSpace -> InspectResult i
inspectWorkspace l i = inspectLayout i . asLayout l . W.layout

asLayout :: (LayoutClass l a, Typeable a) => l a -> Layout a -> l a
asLayout l (Layout l') = cast' l' `asTypeOf` l

cast' :: forall a b. (Typeable a, Typeable b) => a -> b
cast' x | Just HRefl <- ta `eqTypeRep` tb = x
        | otherwise = error $ "X.L.Inspect.cast': " ++ show ta ++ " /= " ++ show tb
  where
    ta = typeRep :: TypeRep a
    tb = typeRep :: TypeRep b

type family InspectResult i

-- | A typeclass defining 'inspectLayout' over monoidal @InspectResult@ types.
class Monoid (InspectResult i) => InspectLayout i l a where
    inspectLayout :: i -> l a -> InspectResult i

-- | An overlappable instance provides sensible default behavior, returning
-- 'mempty'. Specific @InspectResult@ instances may override this by defining
-- their own instance of 'InspectLayout'.
instance {-# OVERLAPPABLE #-} Monoid (InspectResult i) => InspectLayout i l a where
    inspectLayout _ _ = mempty

instance (InspectLayout i l1 a, InspectLayout i l2 a) => InspectLayout i (Choose l1 l2) a where
    inspectLayout i (Choose CL l1 _) = inspectLayout i l1
    inspectLayout i (Choose CR _ l2) = inspectLayout i l2
