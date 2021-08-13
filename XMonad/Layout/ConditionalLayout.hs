{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Layout.ConditionalLayout
-- Copyright    : (c) Ivan Malison <IvanMalison@gmail.com>
-- License      : BSD
--
-- Maintainer   : none
-- Stability    : unstable
-- Portability  : portable
--
-- This module provides a LayoutModifier combinator that modifies an existing
-- ModifiedLayout so that its modifications are only applied when a particular
-- condition is met.
-----------------------------------------------------------------------------

module XMonad.Layout.ConditionalLayout where

import XMonad hiding (hide)
import XMonad.Prelude
import XMonad.Layout.LayoutModifier
import qualified XMonad.StackSet as W

-- | A 'ModifierCondition' is a condition run in 'X' that takes a 'WorkspaceId'
-- as a parameter. The reason that this must exist as a type class and a simple
-- function will not suffice is that 'ModifierCondition's are used as parameters
-- to 'ConditionalLayoutModifier', which must implement 'Read' and 'Show' in
-- order to also implement 'LayoutModifier'. By defining a new type for
-- condition, we sidestep the issue that functions can not implement these
-- typeclasses.
class (Read c, Show c) => ModifierCondition c where
  shouldApply :: c -> WorkspaceId -> X Bool
-- TODO: WindowSpace instead?

-- | 'ConditionalLayoutModifier' takes a condition implemented as a
-- 'ModifierCondition' together with a 'LayoutModifier' and builds a new
-- 'LayoutModifier' that is exactly like the provided 'LayoutModifier', except
-- that it is only applied when the provided condition evalutes to True.
data ConditionalLayoutModifier m c a = (Read (m a), Show (m a), ModifierCondition c) =>
  ConditionalLayoutModifier c (m a)

deriving instance (Read (m a), Show (m a), ModifierCondition c) =>
         Show (ConditionalLayoutModifier m c a)
deriving instance (Read (m a), Show (m a), ModifierCondition c) =>
         Read (ConditionalLayoutModifier m c a)

data NoOpModifier a = NoOpModifier deriving (Read, Show)

instance LayoutModifier NoOpModifier a

instance (ModifierCondition c, LayoutModifier m Window) =>
  LayoutModifier (ConditionalLayoutModifier m c) Window where

  modifyLayout (ConditionalLayoutModifier condition originalModifier) w r = do
    applyModifier <- shouldApply condition $ W.tag w
    if applyModifier
      then modifyLayout originalModifier w r
      else modifyLayout NoOpModifier w r

  modifyLayoutWithUpdate (ConditionalLayoutModifier condition originalModifier) w r = do
    applyModifier <- shouldApply condition $ W.tag w
    if applyModifier
      then do
        (res, updatedModifier) <- modifyLayoutWithUpdate originalModifier w r
        let updatedModifiedModifier =
              ConditionalLayoutModifier condition <$> updatedModifier
        return (res, updatedModifiedModifier)
      else (, Nothing) . fst <$> modifyLayoutWithUpdate NoOpModifier w r

  -- This function is not allowed to have any effect on layout, so we always
  -- pass the message along to the original modifier to ensure that it is
  -- allowed to update its internal state appropriately. This is particularly
  -- important for messages like 'Hide' or 'ReleaseResources'.
  handleMessOrMaybeModifyIt
    (ConditionalLayoutModifier condition originalModifier) mess = do
      result <- handleMessOrMaybeModifyIt originalModifier mess
      return $ case result of
                 Nothing -> Nothing
                 Just (Left updated) ->
                   Just $ Left $
                        ConditionalLayoutModifier condition updated
                 Just (Right message) -> Just $ Right message

  redoLayoutWithWorkspace (ConditionalLayoutModifier condition originalModifier)
                       w r ms wrs = do
    applyModifier <- shouldApply condition $ W.tag w
    if applyModifier
      then do
        (res, updatedModifier) <- redoLayout originalModifier r ms wrs
        let updatedModifiedModifier =
              ConditionalLayoutModifier condition <$> updatedModifier
        return (res, updatedModifiedModifier)
      else (, Nothing) . fst <$> redoLayout NoOpModifier r ms wrs

  modifyDescription (ConditionalLayoutModifier _ originalModifier) l =
    modifyDescription originalModifier l

-- | 'ModifiedLayout' extended with a condition and its last evaluation result
-- (for methods that can't evaluate it).
data CondModifiedLayout c m l a = CondModifiedLayout Bool c (ModifiedLayout m l a) deriving (Read, Show)

instance (ModifierCondition c, LayoutModifier m a, LayoutClass l a, Typeable c, Typeable m)
    => LayoutClass (CondModifiedLayout c m l) a
  where
    runLayout (W.Workspace i cml@(CondModifiedLayout a c _) ms) r = do
        a' <- shouldApply c i
        cml' <- if a == a' then pure Nothing else Just . switch <$> hide cml
        fmap (<|> cml') <$> run (fromMaybe cml cml')
      where
        hide x = fmap (fromMaybe x) $ handleMessage x (SomeMessage Hide)
        switch (CondModifiedLayout a' c' ml') = CondModifiedLayout (not a') c' ml'

        run (CondModifiedLayout True c' ml) =
            fmap (fmap (CondModifiedLayout True c')) <$> runLayout (W.Workspace i ml ms) r
        run (CondModifiedLayout False c' (ModifiedLayout m l)) =
            fmap (fmap (CondModifiedLayout False c' . ModifiedLayout m)) <$> runLayout (W.Workspace i l ms) r

    handleMessage (CondModifiedLayout a c ml@(ModifiedLayout lm l)) m
        | Just ReleaseResources <- fromMessage m = fmap (CondModifiedLayout a c) <$> handleMessage ml m
        | a                                      = fmap (CondModifiedLayout a c) <$> handleMessage ml m
        | otherwise          = fmap (CondModifiedLayout a c . ModifiedLayout lm) <$> handleMessage l m

    description (CondModifiedLayout a _ ml@(ModifiedLayout _ l)) = if a then description ml else description l

conditional :: (ModifierCondition c) => c -> (l a -> ModifiedLayout m l a) -> (l a -> CondModifiedLayout c m l a)
conditional c ml = CondModifiedLayout True c . ml
