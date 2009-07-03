import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Layout.ThreeColumns
import System.IO

myLayouts = ThreeCol 1 (3/100) (1/2)

main = do
  xmproc <- spawnPipe "/home/dave/.cabal/bin/xmobar"
  xmonad $ defaultConfig {
    manageHook = manageDocks <+> manageHook defaultConfig,
    -- layoutHook = avoidStruts  $  layoutHook defaultConfig,
    layoutHook = avoidStruts  $  myLayouts,
    terminal = "/usr/bin/rxvt-unicode",
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
