import XMonad
import XMonad.Actions.Plane
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ICCCMFocus

import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.ResizableTile
import XMonad.Layout.Circle
import XMonad.Layout.PerWorkspace (onWorkspace)


import XMonad.Util.Run
import XMonad.Util.EZConfig

import System.IO
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map as M



{-Xmobar Configuration Vaiables
 - -}
myTitleColor        = "#eeeeee"
myTitleLength       = 80
myCurrentWSColor    = "#e6744c"
myVisibleWSColor    = "#c185a7"
myUrgentWSColor     = "#cc0000"
myCurrentWSLeft     = "["
myCurrentWSRight    = "]"
myVisibleWSLeft     = "("
myVisibleWSRight    = ")"
myUrgentWSLeft      = "{"
myUrgentWSRight     = "}"
--simple variables 
myTerminal      = "urxvt"
myBorderWidth   = 1
--myWorkspaces = ["1:web", "2:term", "3:dev", "4:docs", "5:read", "6:aread", "7:media" , "8:dc", "9:admin"]
--Hooks
--myManageHook =  composeAll[
--                    
--                    ]


myKeys =  [
        ((mod4Mask , xK_z), spawn "xscreensaver-command -lock"),
        ((mod4Mask , xK_p), spawn "scrot \"%Y-%m-%d-%s_$wx$h.png\" -e \"mv $f ~/Pictures/Scrots/\""),
        ((0, xK_Print), spawn "scrot"),
        ((0, 0x1008FF13), spawn "amixer -q set Master 10%+"),
        ((0, 0x1008FF11), spawn "amixer -q set Master 10%-"),
        ((0, 0x1008FF12), spawn "amixer -q set Master toggle")
        ]


main = do
    xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
    xmonad $ defaultConfig
        {
        --Hooks and Layouts
        manageHook = manageDocks <+> manageHook defaultConfig,
        layoutHook = avoidStruts  $  layoutHook defaultConfig,

        --Xmobar
        logHook = takeTopFocus <+> dynamicLogWithPP xmobarPP{
            ppOutput    = hPutStrLn xmproc,
            ppTitle     = xmobarColor myTitleColor "" . shorten myTitleLength,
            ppCurrent   = xmobarColor myCurrentWSColor "" . wrap myCurrentWSLeft myCurrentWSRight,
            ppVisible   = xmobarColor myVisibleWSColor "" . wrap myVisibleWSLeft myVisibleWSRight,
            ppUrgent    = xmobarColor myUrgentWSColor "" . wrap myUrgentWSLeft myUrgentWSRight
        },

        --Simple Variables
        terminal = myTerminal,
        borderWidth = myBorderWidth
        --Bindings
        }`additionalKeys` myKeys
