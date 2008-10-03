import XMonad

main = xmonad $ defaultConfig {
    borderWidth        = 2,
    terminal           = "/usr/bin/gnome-terminal --hide-menubar",
    normalBorderColor  = "#cccccc",
    defaultGaps = [(18,0,0,0)],
    focusedBorderColor = "#cd8b00",
    modMask = mod4Mask
}
