import XMonad
import Fixfocus
-- Patched to show no border for maximized windows
import Maximize
-- Patched to handle spawned processes' children for shell scripts/wrappers
import SpawnOn
-- Patched to ignore resize increment hints, see http://code.google.com/p/xmonad/issues/detail?id=375
-- Some text-based programs have such hints which can be found in the WM_NORMAL_HINTS property via xprop.
import FloatKeys
import Data.Monoid
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
import XMonad.Util.Run
-- http://haskell.org/haskellwiki/Xmonad/Config_archive/John_Goerzen%27s_Configuration#Configuring_xmonad_to_use_xmobar
import XMonad.Hooks.DynamicLog
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
-- To-test:  java -jar /opt/java/demo/jfc/Stylepad/Stylepad.jar
import XMonad.Hooks.SetWMName
-- Send keys to windows
import XMonad.Util.Paste
import XMonad.Actions.WithAll
-- Modify the layouts' names. I use it to shorten them.
import XMonad.Layout.Renamed

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
--    , className =? "Firefox" -?> doShift "9:web"
    ]
    where myFloats = ["Option", "option", "Preference", "preference", "about", "About", "Find", "选项", "书签管理器", "Emacs TEXTAREA"]

myModMask = mod4Mask

-- Avoids killing xmonad accidentally when in tabbed layout.
-- Requires patching xmonad-contrib-darcs XMonad/Layout/Decoration.hs
-- Also avoids killing chrome, borrowed from XMonad.Actions.WindowMenu
myKillWindow w =
  let gsc = defaultGSConfig {- gs_cellheight = 35
                             , gs_cellwidth = 70
                            -}
      protectedProgs = ["Chromium", "Chrome"]
      notProtected = ["Developer Tools - ", "Chromium选项", "谷歌浏览器选项", "书签管理器", "About Chromium", "关于谷歌浏览器", "Task Manager - Chromium", "任务管理器 - 谷歌浏览器", "chrome://devtools/devtools.html", "保存文件"]
  in do
  t <- runQuery title w
  c <- runQuery className w
  let prompt = any (== c) protectedProgs && all (not . ((flip isPrefixOf) t)) notProtected
  unless (t == "XMonad") $
    if prompt then runSelectedAction gsc
                   [ ("Cancel (k)", return ())
                   , ("Kill (j)" , killWindow w)
                   ]
    else killWindow w

-- Why doesn't this work reliably on Firefox?
shiftInsert w =
  let translatedProgs = ["Chromium", "Chrome", "Firefox", "Namoroka"] in do
    c <- runQuery className w
    let toTranslate = any (== c) translatedProgs
    -- Unfortunately pasteSelection only supports ASCII
    -- If we simply use xdotool to send a middle click, it doens't always work depending on the position of the mouse pointer
    if toTranslate then spawn ("CLIP=$(xsel -o -b); xsel -o | xsel -i -b; " ++
                               "xdotool key --clearmodifiers --window " ++ show w ++  " ctrl+v; echo -n $CLIP | xsel -i -b")
     else sendKey shiftMask xK_Insert

main = do
  -- http://haskell.org/haskellwiki/Xmonad/Notable_changes_since_0.9
  -- sp <- mkSpawner
  -- cabal install xmobar -fwith_xft
  -- xmobar-darcs now supports reading from XState instead of from stdin, thus getting rid of the pipe.
  -- See X.C.Sjanssen for the usage. You need to change xmobarc to read from XMonadLog instead of Stdin too.
  -- Unfortunately, this support is experimental and crashes on some long titles.
  xmproc <- spawnPipe "xmobar /home/wh5a/.xmonad/xmobarc"
  let mTile = renamed [Replace "Tile"] mouseResizableTile
      mMirror = renamed [Replace "Mirror"] mouseResizableTileMirrored
      tab = renamed [CutWordsRight 1] $ tabbed shrinkText myTheme
      maxi x = (renamed [CutWordsLeft 1]) $ maximize x
      conf = ewmh defaultConfig {
         modMask = myModMask
       , terminal = myTerm
--       , borderWidth = 1
       , workspaces = ["1:term","2:emacs","3:csurf","4","5","6","7","8","9:web"]
       , manageHook = placeHook (inBounds $ underMouse (0.5,0.5)) <+> manageSpawn <+> manageDocks <+> manageHook defaultConfig <+> myManageHook
       , layoutHook = {- layoutHints $ minimize -} fixFocus $ maxi $ avoidStruts $ smartBorders $ onWorkspace "9:web" (tab ||| mTile ||| Full) $ (mTile ||| mMirror ||| Full)
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
         -- M-t tiles window
       , ("M-S-t", withFocused float)
         -- Cycle forward and backward through non-empty workspaces
       , ("M-<Tab>", moveTo Next NonEmptyWS)
       , ("M-S-<Tab>", moveTo Prev NonEmptyWS)
       , ("M-S-<R>", shiftToNext >> nextWS)
       , ("M-S-<L>", shiftToPrev >> prevWS)
       , ("M-<F1>", manPrompt myXPConfig)
         -- Maximize a window
       , ("M-m", withFocused (sendMessage . maximizeRestore))
         -- A cool menu
       , ("M-o", windowMenu)
         -- We choose to bind keys natively in XMonad, instead of using xbindkeys
         -- Paste from clipboard, mainly used for urxvt, http://bbs.archlinux.org/viewtopic.php?id=80226
         -- We can send a middle click because the mouse pointer's position doesn't matter for CLI programs
       , ("C-<Insert>", spawn "xsel -x; xsel -o -b | xsel -i; xdotool click 2; xsel -x")
         -- Paste from X primary, mainly used for chrome
       , ("S-<Insert>", withFocused shiftInsert)
       --   -- unclutter the mouse
       -- , ("M-u", banish LowerRight)
       --   -- Doesn't work on Chrome?
       -- , ("M-S-u", updatePointer $ Relative 0.5 0.5)
       , ("M-<L>", withFocused (keysMoveWindow (-10, 0)))
       , ("M-<R>", withFocused (keysMoveWindow (10, 0)))
       , ("M-<U>", withFocused (keysMoveWindow (0, -10)))
       , ("M-<D>", withFocused (keysMoveWindow (0, 10)))
       , ("M-<KP_Left>", withFocused (keysResizeWindow (-10, 0) (1%2, 1%2)))
       , ("M-<KP_Right>", withFocused (keysResizeWindow (10, 0) (1%2, 1%2)))
       , ("M-<KP_Up>", withFocused (keysResizeWindow (0, 10) (1%2, 1%2)))
       , ("M-<KP_Down>", withFocused (keysResizeWindow (0, -10) (1%2, 1%2)))
       , ("M1-<Tab>", windows W.focusDown)
       , ("M1-S-<Tab>", windows W.focusUp)
       ]
  xmonad conf {
    startupHook = startupHook conf >> setWMName "LG3D"
    -- Fix some fullscreen events, e.g. Firefox <video> play
  , handleEventHook = handleEventHook conf `mappend` fullscreenEventHook
    }
