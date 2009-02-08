import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

main = do
  xmproc <- spawnPipe "/home/dave/.cabal/bin/xmobar /home/dave/.xmonad/xmobar"
  xmonad $ defaultConfig {
    manageHook = manageDocks <+> manageHook defaultConfig,
    layoutHook = avoidStruts  $  layoutHook defaultConfig,
    terminal = "/usr/bin/xterm",
    normalBorderColor = "#cccccc",
    focusedBorderColor = "#cd8b00",
    logHook = dynamicLogWithPP $ xmobarPP {
      ppOutput = hPutStrLn xmproc,
      ppTitle = xmobarColor "green" "" . shorten 50
    }
  } `additionalKeys`
    [((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock"),
      ((controlMask, xK_Print), spawn "scrot -s"),
      ((0, xK_Print), spawn "scrot")
    ]
