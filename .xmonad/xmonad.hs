import XMonad
import Fixfocus
import Maximize
import qualified XMonad.StackSet as W  
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
--import XMonad.Prompt.Input
import XMonad.Actions.CycleWS
import XMonad.Layout.PerWorkspace
-- Respect size hint, doesn't work?
--import XMonad.Layout.LayoutHints
-- Automatically places floating windows at desired positions.
import XMonad.Hooks.Place
-- Full screen programs: http://code.google.com/p/xmonad/issues/detail?id=228
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Tabbed
import Data.Ratio
-- import XMonad.Layout.Minimize
import XMonad.Layout.MouseResizableTile
import qualified XMonad.Actions.FlexibleResize as Flex
import XMonad.Actions.WindowMenu
-- http://doitian.com/2009/09/clickable-dzen2-panel-for-xmonad/
import XMonad.Hooks.EwmhDesktops
import Control.Monad
import XMonad.Actions.GridSelect
-- import XMonad.Actions.Warp
-- import XMonad.Actions.UpdatePointer

myTheme = defaultTheme {
   fontName = "xft:WenQuanYi Zen Hei:pixelsize=17"
 , decoHeight = 25
  }
    
myXPConfig = defaultXPConfig {
--               font = "xft:Droid Sans:pixelsize=20"
               font = "xft:WenQuanYi Zen Hei:pixelsize=16"
             , position = Top
             --, height = 30
             }
             
-- Arch packages (e.g. wget, pacman) often have trouble with Chinese locales.
myTerm = "LANG=en_US.utf8 urxvtc"             

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
    where myFloats = ["Option", "option", "Preference", "preference", "about", "About", "Find", "选项", "书签管理器"]

myModMask = mod4Mask

-- Avoids killing xmonad accidentally when in tabbed layout.
-- Requires patching xmonad-contrib-darcs http://gist.github.com/264183
-- Also avoids killing chrome, borrowed from XMonad.Actions.WindowMenu
myKillWindow w =
  let gsc = defaultGSConfig {- gs_cellheight = 35
                             , gs_cellwidth = 70
                            -}
      notProtected = ["Developer Tools - ", "Chromium选项", "书签管理器", "About Chromium", "Task Manager - Chromium", "chrome://devtools/devtools.html"]
  in do
  t <- runQuery title w
  c <- runQuery className w
  let prompt = c == "Chrome" && all (not . ((flip isPrefixOf) t)) notProtected
  unless (t == "XMonad") $
    if prompt then runSelectedAction gsc
                   [ ("Cancel (k)", return ())
                   , ("Kill (j)" , killWindow w)
                   ]
    else killWindow w

main = do
  -- http://haskell.org/haskellwiki/Xmonad/Notable_changes_since_0.9
  -- sp <- mkSpawner
  -- cabal install xmobar -fwith_xft
  -- xmobar-darcs now supports reading from XState instead of from stdin, thus getting rid of the pipe.
  -- See X.C.Sjanssen for the usage
  xmproc <- spawnPipe "xmobar /home/wh5a/.xmonad/xmobarc"
  xmonad $ ewmh $
         defaultConfig {
         modMask = myModMask
       , terminal = myTerm
--       , borderWidth = 1
       , workspaces = ["1:term","2:emacs","3:csurf","4","5","6","7","8","9:web"]
       , manageHook = placeHook (inBounds $ underMouse (0.5,0.5)) <+> manageSpawn <+> manageDocks <+> manageHook defaultConfig <+> myManageHook
--       , layoutHook = {- layoutHints $ -} maximize $ avoidStruts $ smartBorders $ onWorkspace "9:web" (tabbed shrinkText myTheme ||| Full ||| Tall 1 (3%100) (1%2)) $ (mouseResizableTile ||| layoutHook defaultConfig)
       , layoutHook = {- layoutHints $ minimize -} fixFocus $ maximize $ avoidStruts $ smartBorders $ onWorkspace "9:web" (tabbed shrinkText myTheme ||| Full ||| mouseResizableTile) $ (mouseResizableTile ||| mouseResizableTileMirrored ||| Full)
       , logHook = dynamicLogWithPP $ xmobarPP {
                     ppOutput = hPutStrLn xmproc
                     -- http://en.wikipedia.org/wiki/X11_color_names
                   , ppTitle = xmobarColor "yellow" "" -- . shorten 52
                   , ppLayout = xmobarColor "burlywood" ""
                   , ppSep = " "
                   }
       }
       `additionalMouseBindings`
       [ -- Resize a floating window from whichever corner or edge the mouse is closest to
         ((myModMask, button3), \w -> focus w >> Flex.mouseResizeEdgeWindow (3%5) w)
       , ((myModMask, button2), myKillWindow)
--         ((0, button3), \w -> focus w >> Flex.mouseResizeEdgeWindow (3%5) w)         
       ]
       `additionalKeysP`
       [ -- dmenu replacement
         ("M-p", shellPromptHere myXPConfig)
       , ("M-S-<KP_Enter>", spawn myTerm)
       , ("M-<Return>", launchApp myXPConfig "urxvtc -cd")
       --, ("M-r", runOrRaisePrompt myXPConfig)
       , ("M-g", windowPromptGoto myXPConfig)
       , ("M-x", xmonadPrompt myXPConfig)
       -- xmonad-eval; not sure what to make use of it
--       , ("M-S-;", inputPrompt myXPConfig "Eval" >>= flip whenJust (evalExpression defaultEvalConfig))
       , ("M-e", launchApp myXPConfig "emacsclient -c -a emacs")
       , ("M-n", launchApp myXPConfig "dolphin")
--       , ("M-S-k", kill)   -- By default, M-S-k/ M-S-j move windows
       , ("M-C-c", withFocused myKillWindow)
       , ("M-d", sinkAll)
         -- Cycle forward and backward through non-empty workspaces
       , ("M-<R>", moveTo Next NonEmptyWS)
       , ("M-<L>", moveTo Prev NonEmptyWS)
       , ("M-S-<R>", shiftToNext >> nextWS)
       , ("M-S-<L>", shiftToPrev >> prevWS)
       , ("M-<F1>", manPrompt myXPConfig)
       , ("M-<U>", windows W.focusDown)
       , ("M-<D>", windows W.focusUp)
         -- Maximize a window
       , ("M-m", withFocused (sendMessage . maximizeRestore))
         -- A cool menu
       , ("M-o", windowMenu)
       --   -- unclutter the mouse
       -- , ("M-u", banish LowerRight)
       --   -- Doesn't work on Chrome?
       -- , ("M-S-u", updatePointer $ Relative 0.5 0.5)
       ]
