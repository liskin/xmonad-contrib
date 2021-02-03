-- |
-- Module      :  XMonad.Util.ClickableWorkspaces.Integrations
-- Copyright   :  (c) 2021 Tomáš Janoušek <tomi@nomi.cz>
-- License     :  BSD3
--
-- Maintainer  :  Tomáš Janoušek <tomi@nomi.cz>
-- Stability   :  experimental
-- Portability :  unknown
--
-- Integrations of "XMonad.Util.ClickableWorkspaces" with
-- "XMonad.Layout.IndependentScreens" and "XMonad.Actions.WorkspaceNames".
--
module XMonad.Util.ClickableWorkspaces.Integrations (
    clickableWorkspaceNamesPP,
    clickableMarshallPP,
    ) where

import XMonad
import XMonad.Actions.WorkspaceNames
import XMonad.Hooks.DynamicLog
import XMonad.Layout.IndependentScreens
import XMonad.Util.ClickableWorkspaces

clickableWorkspaceNamesPP :: PP -> X PP
clickableWorkspaceNamesPP pp = do
    rename <- getWorkspaceNames
    clickableRenamedPP rename pp

clickableMarshallPP :: ScreenId -> PP -> X PP
clickableMarshallPP s pp =
    clickableRenamedPP (snd . unmarshall) pp{ ppSort = marshallSort s <$> ppSort pp }
