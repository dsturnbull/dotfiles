import System.IO
import System.Posix.Env
import XMonad
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders
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
        { terminal = "/opt/local/bin/urxvt"
        , borderWidth = 1
        , normalBorderColor = "#cccccc"
        , focusedBorderColor = "#cd8b00"
        , modMask = mod4Mask
        , manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = smartBorders $ layoutHints $ avoidStruts $ layoutHook defaultConfig
        , logHook = dynamicLogWithPP $ xmobarPP
            { ppOutput = hPutStrLn xmproc
            , ppTitle = xmobarColor "green" "" . shorten 50
            }
        } `additionalKeys`
            [ ((mod4Mask,                xK_f), viewEmptyWorkspace)
            , ((mod4Mask .|. shiftMask,  xK_f), tagToEmptyWorkspace)
            , ((mod4Mask .|. shiftMask,  xK_s), sshPrompt defaultXPConfig)
            , ((mod4Mask,                xK_s), SM.submap $ searchEngineMap $ S.promptSearch P.defaultXPConfig)
            ]

searchEngineMap method = M.fromList $
    [ ((0, xK_g), method S.google)
    , ((0, xK_h), method S.hoogle)
    , ((0, xK_w), method S.wikipedia)
    , ((0, xK_a), method S.amazon)
    , ((0, xK_i), method S.imdb)
    , ((0, xK_m), method S.maps)
    , ((0, xK_y), method S.youtube)
    ]

-- vim:filetype=haskell

