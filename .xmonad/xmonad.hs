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
    
myXPConfig = defaultXPConfig {
--               font = "xft:Droid Sans Mono:pixelsize=14"
               font = "xft:WenQuanYi Zen Hei Mono:pixelsize=20"
               --font = "xft:SimSun:pixelsize=16"
             , position = Top
             --, height = 30
             }

-- If q contains x
contain q x = fmap (isInfixOf x) q

myManageHook = composeAll . concat $
    [ [ title `contain` c --> doFloat | c <- myFloats ]
--    , [ className =? "Firefox-bin" --> doShift "web" ]
    ]
    where myFloats = ["Option", "option", "Preference", "preference"]

main = do
  sp <- mkSpawner
  -- cabal install xmobar -fwith_xft
  xmproc <- spawnPipe "xmobar /home/wh5a/.xmonad/xmobarc"
  xmonad $
         defaultConfig {
         modMask = mod4Mask
       , terminal = "urxvtc"
       , manageHook = manageSpawn sp <+> manageDocks <+> manageHook defaultConfig <+> myManageHook
       , layoutHook = avoidStruts $ smartBorders $ layoutHook defaultConfig
       , logHook = dynamicLogWithPP $ xmobarPP {
                     ppOutput = hPutStrLn xmproc
                   , ppTitle = xmobarColor "yellow" "" -- . shorten 52
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
       , ("M-S-k", kill)
       , ("M-d", sinkAll)
       ]
