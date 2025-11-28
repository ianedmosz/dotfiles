import Data.Map qualified as M
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.InsertPosition
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.Spiral
import XMonad.Layout.Renamed
import XMonad.StackSet qualified as W
import XMonad.Util.EZConfig (additionalKeysP, removeKeysP)
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce
import System.Exit (exitWith, ExitCode(ExitSuccess))
import XMonad.Layout.Gaps
import XMonad.Layout.ToggleLayouts
import XMonad.Actions.UpdatePointer
myBorderWith :: Dimension
myBorderWith = 2

mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

myManageHook :: ManageHook
myManageHook =
  insertPosition Below Newer <+> manageHook def

myWorkspaces :: [WorkspaceId]
myWorkspaces = ["一", "二", "三", "四", "五", "六", "七", "八", "九"]

myTerminal :: String
myTerminal = "alacritty"

myStartupHook :: X ()
myStartupHook = do
    spawnOnce "xrdb -merge ~/.Xresources"
    spawnOnce "nitrogen --restore"
    spawnOnce "picom &"
    spawnOnce "slstatus &"
    spawnOnce "dunst &"
    spawnOnce "eval $(/usr/bin/gnome-keyring-daemon --start)"
    spawnOnce "export SSH_AUTH_SOCK"
    spawnOnce "nm-applet &"
    spawnOnce "~/.local/bin/monitor.sh"
    spawn "xset s off"     -- screensaver OFF
    spawn "xset -dpms"     -- desactivar energía
    spawn "xset s noblank" -- evitar pantalla negra


myLayout =
    avoidStruts
    $ smartBorders
    $ smartSpacingWithEdge 8
    $ toggleLayouts (noBorders Full)
        ( renamed [Replace "Tall"]   tiled
       ||| renamed [Replace "Wide"]  (Mirror tiled)
       ||| renamed [Replace "Full"]  Full
       ||| renamed [Replace "Spiral"] (spiral (6/7))
        )
  where
    tiled = Tall 1 (3/100) (1/2)

myXmobarPP :: PP
myXmobarPP = def
    { ppCurrent         = xmobarColor "#f7768e" ""
    , ppVisible         = xmobarColor "#f7768e" ""
    , ppHidden          = xmobarColor "#a9b1d6" ""
    , ppHiddenNoWindows = const ""   -- no mostrar vacíos
    , ppTitle           = const ""   -- sin títulos
    , ppLayout          = const ""   -- sin layout name
    }

myStatusBar :: StatusBarConfig
myStatusBar = statusBarProp "~/.config/xmobar/launch.sh" (pure myXmobarPP)

myKeys :: [(String, X ())]
myKeys =
    [ ("M-<Return>", spawn "alacritty")
    , ("M-d", spawn "dmenu_run")
    , ("M-S-c", io (exitWith ExitSuccess))
    , ("M-S-q", kill)
    , ("M-S-r", spawn "xmonad --recompile" >> spawn "xmonad --restart")
    , ("M-S-f", sendMessage ToggleLayout >> sendMessage ToggleStruts)
    , ("M-c", sendMessage $ JumpToLayout "Spiral")
    , ("M-j", windows W.focusDown)
    , ("M-k", windows W.focusUp)
    , ("M-<Tab>", windows W.focusDown)
    , ("M-p", spawn "~/.local/bin/wallmenu.sh")
    , ("M-S-p", spawn "~/.local/bin/power-menu.sh")

    , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
    , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
    , ("<XF86AudioMute>",        spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
    , ("<XF86MonBrightnessUp>",   spawn "brightnessctl set +5%")
    , ("<XF86MonBrightnessDown>", spawn "brightnessctl set 5%-")
    ]

myWSKeys :: [(String, X ())]
myWSKeys =
    [ ("M-1",   windows $ W.view  "一")
    , ("M-2",   windows $ W.view  "二")
    , ("M-3",   windows $ W.view  "三")
    , ("M-4",   windows $ W.view  "四")
    , ("M-5",   windows $ W.view  "五")
    , ("M-6",   windows $ W.view  "六")
    , ("M-7",   windows $ W.view  "七")
    , ("M-8",   windows $ W.view  "八")
    , ("M-9",   windows $ W.view  "九")

    , ("M-S-1", windows $ W.shift "一")
    , ("M-S-2", windows $ W.shift "二")
    , ("M-S-3", windows $ W.shift "三")
    , ("M-S-4", windows $ W.shift "四")
    , ("M-S-5", windows $ W.shift "五")
    , ("M-S-6", windows $ W.shift "六")
    , ("M-S-7", windows $ W.shift "七")
    , ("M-S-8", windows $ W.shift "八")
    , ("M-S-9", windows $ W.shift "九")
    ]

wsKeysToRemove :: [String]
wsKeysToRemove =
    [ "M-" ++ show n     | n <- [1..9] ] ++
    [ "M-S-" ++ show n   | n <- [1..9] ]

myConfig =
  ( def
      { terminal           = myTerminal
      , borderWidth        = myBorderWith
      , normalBorderColor  = "#44b6a"
      , focusedBorderColor = "#ad8ee6"
      , layoutHook         = myLayout
      , workspaces         = myWorkspaces
      , manageHook         = myManageHook
      , startupHook        = myStartupHook
      , logHook = updatePointer (0.5, 0.5) (0, 0)
      }
    `removeKeysP` wsKeysToRemove
  ) `additionalKeysP` (myKeys ++ myWSKeys)

main :: IO ()
main = xmonad . ewmhFullscreen . ewmh . withEasySB myStatusBar defToggleStrutsKey $ myConfig
