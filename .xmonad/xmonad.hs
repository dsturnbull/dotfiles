import System.IO
import System.Posix.Env
import XMonad
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.LayoutHints
import XMonad.Layout.Maximize
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Prompt
import XMonad.Prompt.Ssh
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run(spawnPipe)
import qualified Data.Map as M
import qualified XMonad.Actions.Search as S
import qualified XMonad.Actions.Submap as SM
import qualified XMonad.Prompt         as P

main = do
    putEnv "BROWSER=firefox-x11-standalone"
    xmproc <- spawnPipe "xmobar $HOME/.xmonad/xmobar"
    xmonad $ defaultConfig
        { terminal           = "/opt/local/bin/urxvt"
        , borderWidth        = 1
        , normalBorderColor  = "#cccccc"
        , focusedBorderColor = "#cd8b00"
        , modMask            = mod4Mask
        , manageHook         = manageDocks <+> manageHook defaultConfig
        , layoutHook         = smartBorders . layoutHints . avoidStruts $ myLayouts
        , logHook            = dynamicLogWithPP $ xmobarPP
            { ppOutput = hPutStrLn xmproc
            , ppTitle  = xmobarColor "green" "" . shorten 50
            }
        } `additionalKeys` myKeys

myKeys =
    [ ((mod4Mask, xK_f), viewEmptyWorkspace)
    , ((modShift, xK_f), tagToEmptyWorkspace)
    , ((mod4Mask, xK_a), sendMessage MirrorExpand)
    , ((mod4Mask, xK_z), sendMessage MirrorShrink)
    , ((mod4Mask, xK_s), search)
    , ((modShift, xK_s), sshPrompt defaultXPConfig)
    ]
    where modShift    = mod4Mask .|. shiftMask
          search      = SM.submap $ searchMap $ S.promptSearch P.defaultXPConfig
          searchMap m = M.fromList $
              [ ((0, xK_g), m S.google)
              , ((0, xK_h), m S.hoogle)
              , ((0, xK_w), m S.wikipedia)
              , ((0, xK_a), m S.amazon)
              , ((0, xK_i), m S.imdb)
              , ((0, xK_m), m S.maps)
              , ((0, xK_y), m S.youtube)
              ]

myLayouts = tiled' ||| Mirror tiled' ||| Full
  where
    nmaster  = 1     -- The default number of windows in the master pane
    ratio    = 1/2   -- Default proportion of screen occupied by master pane
    delta    = 3/100 -- Percent of screen to increment by when resizing panes
    tiled'   = maximize $ ResizableTall nmaster delta ratio []

-- vim:filetype=haskell

