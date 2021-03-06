import System.Posix.Env
import XMonad
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.LayoutHints
import XMonad.Layout.Maximize
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.WindowNavigation
import XMonad.Prompt
import XMonad.Prompt.Ssh
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run(spawnPipe)
import qualified Data.Map as M
import qualified System.IO.UTF8        as U
import qualified XMonad.Actions.Search as S
import qualified XMonad.Actions.Submap as SM
import qualified XMonad.Prompt         as P

main = do
    -- putEnv "BROWSER=firefox-x11-standalone"
    -- xmproc <- spawnPipe "xmobar $HOME/.xmonad/xmobar"
    xmonad $ myConfig `additionalKeys` myKeys

myConfig = defaultConfig
    { terminal           = "/opt/local/bin/urxvt"
    , borderWidth        = 1
    , normalBorderColor  = "#cccccc"
    , focusedBorderColor = "#cd8b00"
    , modMask            = mod1Mask
    , focusFollowsMouse  = sloppyFocus
    , manageHook         = manageDocks <+> manageHook defaultConfig
    , layoutHook         = smartBorders . layoutHints . avoidStruts $ myLayouts
    }

myKeys =
    [ ((modMask, xK_f), viewEmptyWorkspace)
    , ((modShft, xK_f), tagToEmptyWorkspace)
    , ((modMod2, xK_k), sendMessage MirrorExpand)
    , ((modMod2, xK_l), sendMessage Expand)
    , ((modMod2, xK_h), sendMessage Shrink)
    , ((modMod2, xK_j), sendMessage MirrorShrink)
    , ((modMask, xK_s), search)
    , ((modMask, xK_t), itunesMap)
    , ((modShft, xK_s), sshPrompt defaultXPConfig)
    , ((modMask, xK_h), sendMessage $ Go L)
    , ((modMask, xK_j), sendMessage $ Go D)
    , ((modMask, xK_k), sendMessage $ Go U)
    , ((modMask, xK_l), sendMessage $ Go R)
    , ((modShft, xK_h), sendMessage $ Swap L)
    , ((modShft, xK_j), sendMessage $ Swap D)
    , ((modShft, xK_k), sendMessage $ Swap U)
    , ((modShft, xK_l), sendMessage $ Swap R)
    ]
    where modMask     = mod1Mask
          modShft     = modMask .|. shiftMask
          modCtrl     = modMask .|. controlMask
          modShCr     = modMask .|. shiftMask .|. controlMask
          modMod2     = mod2Mask
          modM1Cr     = modMask .|. mod2Mask .|. controlMask
          search      = SM.submap $ searchMap $ S.promptSearch P.defaultXPConfig
          nilMask     = 0
          jstShft     = shiftMask
          searchMap m = M.fromList $
              [ ((nilMask, xK_g), m S.google)
              , ((nilMask, xK_h), m S.hoogle)
              , ((nilMask, xK_w), m S.wikipedia)
              , ((nilMask, xK_a), m S.amazon)
              , ((nilMask, xK_i), m S.imdb)
              , ((nilMask, xK_m), m S.maps)
              , ((nilMask, xK_y), m S.youtube)
              ]
          itunesMap = SM.submap . M.fromList $
              [ ((nilMask, xK_n),      spawn "itunes next")
              , ((nilMask, xK_p),      spawn "itunes prev")
              , ((nilMask, xK_space),  spawn "itunes pause")
              , ((jstShft, xK_space),  spawn "itunes play")
              , ((nilMask, xK_m),      spawn "itunes mute")
              , ((nilMask, xK_u),      spawn "itunes unmute")
              , ((nilMask, xK_period), spawn "itunes vol up")
              , ((nilMask, xK_comma),  spawn "itunes vol down")
              ]

myLayouts = windowNavigation $ tiled' ||| Mirror tiled' ||| Full
    where nmaster = 1     -- The default number of windows in the master pane
          ratio   = 1/2   -- Default proportion of screen occupied by master pane
          delta   = 3/100 -- Percent of screen to increment by when resizing panes
          tiled'  = maximize $ ResizableTall nmaster delta ratio []

-- dodgy trackpad!
sloppyFocus = False

-- vim:filetype=haskell

