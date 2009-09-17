import XMonad
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig
import XMonad.Hooks.ManageDocks
-- {{ Prompt    
-- http://braincrater.wordpress.com/2008/11/29/pimp-your-xmonad-3-prompt/
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise
--import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.Prompt.XMonad
import XMonad.Prompt.AppLauncher
-- Show man page. Somehow you need to press the Up arrow to get it to show.
import XMonad.Prompt.Man
-- Prompt }}
import XMonad.Actions.SpawnOn
import XMonad.Util.Run
-- http://haskell.org/haskellwiki/Xmonad/Config_archive/John_Goerzen%27s_Configuration#Configuring_xmonad_to_use_xmobar
import XMonad.Hooks.DynamicLog
import XMonad.Actions.SinkAll
import Data.List
-- Emacs-style expression evaluation, requires the xmonad-eval package    
--import XMonad.Actions.Eval
import XMonad.Prompt.Input
import XMonad.Actions.CycleWS
import XMonad.Layout.PerWorkspace
-- Respect size hint, doesn't work?
--import XMonad.Layout.LayoutHints
-- Automatically places floating windows at desired positions.
import XMonad.Hooks.Place
-- Full screen programs: http://code.google.com/p/xmonad/issues/detail?id=228
import XMonad.Hooks.ManageHelpers
    
myXPConfig = defaultXPConfig {
--               font = "xft:Droid Sans:pixelsize=20"
               font = "xft:WenQuanYi Zen Hei:pixelsize=16"
             , position = Top
             --, height = 30
             }

-- If q contains x
contain q x = fmap (isInfixOf x) q

myManageHook = composeOne $
    [ title `contain` c -?> doFloat | c <- myFloats ] ++
    [ transience
    , isDialog -?> doFloat
    , isFullscreen -?> doFullFloat
    , className =? "Dialog" -?> doFloat
    , appName =? "emacs" -?> doShift "2:emacs"
    , appName =? "fqterm.bin" -?> doShift "9:web"
    , className =? "Toplevel" -?> doShift "3:csurf"
    , className =? "Firefox" -?> doShift "9:web"
    ]
    where myFloats = ["Option", "option", "Preference", "preference", "about", "About", "Find"]

main = do
  sp <- mkSpawner
  -- cabal install xmobar -fwith_xft
  xmproc <- spawnPipe "xmobar /home/wh5a/.xmonad/xmobarc"
  xmonad $
         defaultConfig {
         modMask = mod4Mask
       , terminal = "urxvtc"
       , workspaces = ["1:term","2:emacs","3:csurf","4","5","6","7","8","9:web"]
       , manageHook = placeHook (inBounds $ underMouse (0.5,0.5)) <+> manageSpawn sp <+> manageDocks <+> manageHook defaultConfig <+> myManageHook
       , layoutHook = {- layoutHints $ -} avoidStruts $ smartBorders $ onWorkspace "9:web" Full $ layoutHook defaultConfig
       , logHook = dynamicLogWithPP $ xmobarPP {
                     ppOutput = hPutStrLn xmproc
                     -- http://en.wikipedia.org/wiki/X11_color_names
                   , ppTitle = xmobarColor "yellow" "" -- . shorten 52
                   , ppLayout = xmobarColor "burlywood" ""
                   , ppSep = " "
                   }
       }
       `additionalKeysP`
       [ -- dmenu replacement
         ("M-p", shellPromptHere sp myXPConfig)
       --, ("M-r", runOrRaisePrompt myXPConfig)
       , ("M-g", windowPromptGoto myXPConfig)
       , ("M-x", xmonadPrompt myXPConfig)
       -- xmonad-eval; not sure what to make use of it
--       , ("M-S-;", inputPrompt myXPConfig "Eval" >>= flip whenJust (evalExpression defaultEvalConfig))
       , ("M-e", launchApp myXPConfig "emacsclient -c -a emacs")
       , ("M-n", launchApp myXPConfig "dolphin")
--       , ("M-S-k", kill)   -- By default, M-S-k/ M-S-j move windows
       , ("M-C-c", kill)
       , ("M-d", sinkAll)
         -- Cycle forward and backward through non-empty workspaces
       , ("M-<R>", moveTo Next NonEmptyWS)
       , ("M-<L>", moveTo Prev NonEmptyWS)
       , ("M-S-<R>", shiftToNext >> nextWS)
       , ("M-S-<L>", shiftToPrev >> prevWS)
       , ("M-<F1>", manPrompt myXPConfig)
       ]
