{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Actions.SpawnOn
-- Copyright    : (c) Spencer Janssen
-- License      : BSD
--
-- Maintainer   : Spencer Janssen <spencerjanssen@gmail.com>
-- Stability    : unstable
-- Portability  : unportable
--
-- Provides a way to modify a window spawned by a command(e.g shift it to the workspace
-- it was launched on) by using the _NET_WM_PID property that most windows set on creation.
-- Hence this module won't work on applications that don't set this property.
--
-----------------------------------------------------------------------------

module SpawnOn (
    -- * Usage
    -- $usage
    Spawner,
    manageSpawn,
    spawnHere,
    spawnOn,
    spawnAndDo,
    shellPromptHere,
    shellPromptOn
) where

import Data.Char (isDigit)
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (listToMaybe)
import System.FilePath ((</>))
import System.Posix.Types (ProcessID)

import XMonad
import qualified XMonad.StackSet as W

import XMonad.Hooks.ManageHelpers
import XMonad.Prompt
import XMonad.Prompt.Shell
import qualified XMonad.Util.ExtensibleState as XS

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- >    import XMonad.Actions.SpawnOn
--
-- >    main = do
-- >      xmonad defaultConfig {
-- >         ...
-- >         manageHook = manageSpawn <+> manageHook defaultConfig
-- >         ...
-- >      }
--
-- To ensure that application appears on a workspace it was launched at, add keybindings like:
--
-- >  , ((mod1Mask,xK_o), spawnHere "urxvt")
-- >  , ((mod1Mask,xK_s), shellPromptHere defaultXPConfig)
--
-- The module can also be used to apply other manage hooks to the window of
-- the spawned application(e.g. float or resize it).
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".

newtype Spawner = Spawner {pidsRef :: [(ProcessID, ManageHook)]} deriving Typeable

instance ExtensionClass Spawner where
    initialValue = Spawner []

maxPids :: Int
maxPids = 5

-- | Get the current Spawner or create one if it doesn't exist.
modifySpawner :: ([(ProcessID, ManageHook)] -> [(ProcessID, ManageHook)]) -> X ()
modifySpawner f = XS.modify (Spawner . f . pidsRef)

-- | Provides a manage hook to react on process and its child spawned with
-- 'spawnOn', 'spawnHere' etc.
manageSpawn :: ManageHook
manageSpawn = do
    Spawner pids <- liftX XS.get
    mp <- pid
    case flip lookup pids =<< mp of
        Nothing -> let f p = do
                         -- Also apply the manage hook if its parent was recorded
                         -- Adapted from http://www.haskell.org/pipermail/xmonad/2009-September/008474.html
                         ppid <- io $ getPPid p
                         maybe idHook id (flip lookup pids =<< ppid)
                   in
                    maybe idHook f mp
        Just mh  -> do
            whenJust mp $ \p ->
                liftX . modifySpawner $ filter ((/= p) . fst)
            mh

mkPrompt :: (String -> X ()) -> XPConfig -> X ()
mkPrompt cb c = do
    cmds <- io $ getCommands
    mkXPrompt Shell c (getShellCompl cmds) cb

-- | Replacement for Shell prompt ("XMonad.Prompt.Shell") which launches
-- application on current workspace.
shellPromptHere :: XPConfig -> X ()
shellPromptHere = mkPrompt spawnHere

-- | Replacement for Shell prompt ("XMonad.Prompt.Shell") which launches
-- application on given workspace.
shellPromptOn :: WorkspaceId -> XPConfig -> X ()
shellPromptOn ws = mkPrompt (spawnOn ws)

-- | Replacement for 'spawn' which launches
-- application on current workspace.
spawnHere :: String -> X ()
spawnHere cmd = withWindowSet $ \ws -> spawnOn (W.currentTag ws) cmd

-- | Replacement for 'spawn' which launches
-- application on given workspace.
spawnOn :: WorkspaceId -> String -> X ()
spawnOn ws cmd = spawnAndDo (doShift ws) cmd

-- | Spawn an application and apply the manage hook when it opens.
spawnAndDo :: ManageHook -> String -> X ()
spawnAndDo mh cmd = do
    p <- spawnPID cmd
    modifySpawner $ take maxPids . ((p,mh) :)

ppidHeader :: String
ppidHeader = "PPid:\t"

isPPidLine :: String -> Bool
isPPidLine xs = (ppidHeader `isPrefixOf` xs) && all isDigit (drop (length ppidHeader) xs)

snipPPid :: String -> [String]
snipPPid = map (drop (length ppidHeader)) . filter isPPidLine . lines

readPPidLine :: String -> Maybe ProcessID
readPPidLine = listToMaybe . map (fromInteger . fst) . (>>= reads) . snipPPid

getPPid :: ProcessID -> IO (Maybe ProcessID)
getPPid p = fmap readPPidLine $ readFile ("/proc" </> show p </> "status") `catch` const (return "")

-- | Get the list of ancestors, youngest first, of a process.
ancestorChain :: (Functor m, MonadIO m) => ProcessID -> m [ProcessID]
ancestorChain p = io (getPPid p) >>= fmap (p:) . maybe (return []) ancestorChain
