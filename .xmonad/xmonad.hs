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
    xmonad $ myConfig xmproc `additionalKeys` myKeys

myConfig xmproc = defaultConfig
    { terminal           = "/opt/local/bin/urxvt"
    , borderWidth        = 1
    , normalBorderColor  = "#cccccc"
    , focusedBorderColor = "#cd8b00"
    , modMask            = mod4Mask
    , focusFollowsMouse  = sloppyFocus
    , manageHook         = manageDocks <+> manageHook defaultConfig
    , layoutHook         = smartBorders . layoutHints . avoidStruts $ myLayouts
    , logHook            = dynamicLogWithPP $ xmobarPP
        { ppOutput = hPutStrLn xmproc
        , ppTitle  = xmobarColor "green" "" . shorten 50
        }
    }
myKeys =
    [ ((modMask, xK_f), viewEmptyWorkspace)
    , ((modShft, xK_f), tagToEmptyWorkspace)
    , ((modMask, xK_a), sendMessage MirrorExpand)
    , ((modMask, xK_z), sendMessage MirrorShrink)
    , ((modMask, xK_s), search)
    , ((modShft, xK_s), sshPrompt defaultXPConfig)
    ]
    where modMask     = mod4Mask
          modShft     = modMask .|. shiftMask
          modCtrl     = modMask .|. controlMask
          modShCr     = modMask .|. shiftMask .|. controlMask
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
    where nmaster = 1     -- The default number of windows in the master pane
          ratio   = 1/2   -- Default proportion of screen occupied by master pane
          delta   = 3/100 -- Percent of screen to increment by when resizing panes
          tiled'  = maximize $ ResizableTall nmaster delta ratio []

-- dodgy trackpad!
sloppyFocus = False

-- vim:filetype=haskell

