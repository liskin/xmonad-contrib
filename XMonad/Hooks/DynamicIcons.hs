{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.DynamicIcons
-- Copyright   :  (c) Will Pierlot <willp@outlook.com.au>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Will Pierlot <willp@outlook.com.au>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Dynamically change workspace text based on the contents of the workspace
-----------------------------------------------------------------------------

module XMonad.Hooks.DynamicIcons (
    -- * Usage
    -- $usage

    -- * Creating Dynamic Icons
    dynamicLogIconsWithPP, dynamicLogIconsConvert,

    -- * Data Types
    appIcon, IconSet,
    IconConfig(..), Icon(..),
    iconsFmtAppend, iconsFmtReplace, wrapUnwords,

    ) where
import XMonad

import qualified XMonad.StackSet as S
import qualified Data.Map as M

import XMonad.Hooks.DynamicLog
import Data.Maybe (catMaybes)

-- $usage
--  Dynamically changes a 'Workspace's 'WorkspaceId' based on the 'Window's inside the Workspace.
-- 'IconSet's describe which icons are shown depending on which windows fit a 'Query'.
--
-- To create an 'IconSet' make a 'Query' that returns a ['Icon'].
--
-- 'appIcon' can be used to simplify this process
-- For example,
--
-- > icons :: IconSet
-- > icons = composeAll
-- >           [ className =? "discord" --> appIcon "\xfb6e"
-- >           , className =? "Discord" --> appIcon "\xf268"
-- >           , className =? "Firefox" --> appIcon "\63288"
-- >           , className =? "Spotify" <||> className =? "spotify" --> appIcon "阮"
-- >           ]
--
-- then you can add the hook to your config
--
-- > xmonad $ def
-- >      { logHook = dynamicLogIconsWithPP icons xmobarPP <> myManageHook
-- >      }
--
--  Here is an example of this
--
--  <<https://imgur.com/download/eauPNPz/Dynamic%20Icons%20in%20XMonad>>
--  
--  NOTE: You can use any string you want here. The example shown here, uses NerdFont Icons to represent open applications


-- | Custom datatype for custom icons based on the state of the 'Workspace'
-- For example,
--
-- > Icon "<bold>discord</bold>" "<bold>discord</bold>" "discord" ""
--
-- Then you can add it to your IconSet.
--
-- > icons :: IconSet
-- > icons = mconcat
-- >    [ className =? "discord" --> pure [Icon "<bold>discord</bold>" "<bold>discord</bold>" "discord" ""]
-- >    ]
data Icon = Icon
    { iconCurrent         :: !String -- ^ If the 'Workspace' is the current workspace
    , iconVisible         :: !String -- ^ If the 'Workspace' is visible (Xinerama only)
    , iconHidden          :: !String -- ^ If the 'Workspace' isnt visible but still has windows
    , iconHiddenNoWindows :: !String -- ^ If the 'Workspace' isnt visible and has no windows
    }

-- | The set of Icons to use
type IconSet = Query [Icon]

baseIconSet :: String -> Icon
baseIconSet x =
    Icon { iconCurrent = x
         , iconVisible = x
         , iconHidden = x
         , iconHiddenNoWindows = x
         }

-- | Create an 'IconSet' from a 'String'
appIcon :: String -> IconSet
appIcon x = pure [baseIconSet x]

-- | Adjusts the 'PP' and then runs 'dynamicLogWithPP'
dynamicLogIconsWithPP :: IconSet -- ^ The 'IconSet' to use
                      -> PP -- ^ The 'PP' to alter
                      -> X () -- ^ The resulting 'X' action
dynamicLogIconsWithPP iconset pp = dynamicLogWithPP =<< dynamicLogIconsConvert (def{ iconConfigIcons = iconset, iconConfigPP = pp })

-- | Datatype for expanded 'Icon' configurations
data IconConfig = IconConfig
    { iconConfigIcons :: IconSet -- ^ The 'IconSet' to use
    , iconConfigFmt :: WorkspaceId -> [String] -> String
      -- ^ How to format the result, see 'iconsFmtReplace', 'iconsFmtAppend'.
    , iconConfigPP    :: PP -- ^ The 'PP' to alter
    }

instance Default IconConfig where
    def = IconConfig
        { iconConfigIcons = mempty
        , iconConfigFmt = iconsFmtReplace (wrapUnwords "{" "}")
        , iconConfigPP = def
        }

-- | 'iconConfigFmt' that replaces the workspace name with icons, if any.
--
-- First parameter specifies how to concatenate multiple icons. Useful values
-- include: 'concat', 'unwords', 'wrapUnwords'.
--
-- >>> iconsFmtReplace concat "1" []
-- "1"
--
-- >>> iconsFmtReplace concat "1" ["A", "B"]
-- "AB"
--
-- >>> iconsFmtReplace (wrapUnwords "{" "}") "1" ["A", "B"]
-- "{A B}"
iconsFmtReplace :: ([String] -> String) -> WorkspaceId -> [String] -> String
iconsFmtReplace cat ws is | null is   = ws
                          | otherwise = cat is

-- | 'iconConfigFmt' that appends icons to the workspace name.
--
-- First parameter specifies how to concatenate multiple icons. Useful values
-- include: 'concat', 'unwords', 'wrapUnwords'.
--
-- >>> iconsFmtAppend concat "1" []
-- "1"
--
-- >>> iconsFmtAppend concat "1" ["A", "B"]
-- "1:AB"
iconsFmtAppend :: ([String] -> String) -> WorkspaceId -> [String] -> String
iconsFmtAppend cat ws is | null is   = ws
                         | otherwise = ws ++ ':' : cat is

-- | Join words with spaces, and wrap the result in delimiters unless there
-- was exactly one element.
--
-- >>> wrapUnwords "{" "}" ["A", "B"]
-- "{A B}"
--
-- >>> wrapUnwords "{" "}" ["A"]
-- "A"
--
-- >>> wrapUnwords "{" "}" []
-- ""
wrapUnwords :: String -> String -> [String] -> String
wrapUnwords _ _ [x] = x
wrapUnwords l r xs  = wrap l r (unwords xs)

-- | This is the same as 'dynamicLogIconsWithPP' but it takes a 'IconConfig'.
-- This allows you to manually customise the 'Icon's the stacking function and also your `PP`
dynamicLogIconsConvert :: IconConfig -> X PP
dynamicLogIconsConvert IconConfig{..} = do
    ws <- gets (S.workspaces . windowset)
    icons <- M.fromList . catMaybes <$> mapM (getIcons iconConfigIcons) ws
    pure $ iconConfigPP
        { ppCurrent = ppSection ppCurrent iconCurrent icons
        , ppVisible = ppSection ppVisible iconVisible icons
        , ppHidden = ppSection ppHidden iconHidden icons
        , ppHiddenNoWindows =  ppSection ppHiddenNoWindows iconHiddenNoWindows icons
        }
  where
    ppSection ppField icField icons wks =
        ppField iconConfigPP $ iconConfigFmt wks $ map icField $ M.findWithDefault [] wks icons

getIcons :: IconSet -> WindowSpace -> X (Maybe (WorkspaceId, [Icon]))
getIcons is w = do
    validIcons <- sequence $ foldMap (runQuery is) . S.integrate <$> S.stack w
    pure $ (S.tag w,) <$> (validIcons >>= \x -> if null x then Nothing else Just x)
