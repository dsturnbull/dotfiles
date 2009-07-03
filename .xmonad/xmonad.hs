import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Layout.ThreeColumns
import System.IO
import XMonad
main = xmonad defaults

defaults = defaultConfig {
    terminal = "/opt/local/bin/urxvt"
  , borderWidth = 1
  , normalBorderColor = "#cccccc"
  , focusedBorderColor = "#cd8b00"
  , modMask = mod4Mask
}
