import XMonad

main = xmonad $ defaultConfig {
  terminal = "/usr/bin/gnome-terminal --hide-menubar",
  normalBorderColor = "#cccccc",
  modMask = mod4Mask
}
