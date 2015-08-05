import XMonad-- {{{

import XMonad.Actions.GridSelect

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.EwmhDesktops

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
import XMonad.Util.NamedWindows
import System.IO
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Data.List-- }}}


{-Xmobar Configuration Vaiables
 - -}
myTitleColor        = "#eeeeee"-- {{{
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
myWorkspaces = ["web", "dev", "doc", "acd", "cal", "com", "med" , "dow", "mus"]
myNormalBorderColor = "#7c7c7c"
myFocusedBorderColor = "#ffb6b0"-- }}}
--Hooks
myManageHook    = composeAll . concat $ ---{{{
                    [[className =? "Firefox"      --> doShift "web"],
                    [className =? "libprs500"    --> doShift "cal"],
                    [className =? "Linuxdcpp"    --> doShift "dow"],
                    [className =? "MPlayer"    --> doShift "med"],
                    [className =? "Audacious"    --> doShift "mus"],
                    [className =? "Rhythmbox"    --> doShift "mus"],
                    [className =? "MComix"    --> doShift "com"],
                    [className =? "Deluge"    --> doShift "dow"],
                    [title     =? t --> doFloat | t<-myTitleFloats],
                    [className  =? c --> doFloat | c<-myClassFloats]]
                    where
                    myTitleFloats   = ["Transferring", "Dialog", "Mailcheck"]
                    myClassFloats   = ["Pinentry", "Yad", "Audacious", "XVroot", "XTerm"]-- }}}

spawnSelected' :: [(String, String)] -> X()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
            where conf = defaultGSConfig
myKeyBindings =  [-- {{{
        ((mod4Mask , xK_z), spawn "xtrlock"),
        ((mod4Mask , xK_p), spawn "scrot \"%Y-%m-%d-%s_$wx$h.png\" -e \"mv $f ~/Pictures/Scrots/\""),
        ((0, xK_Print), spawn "scrot"),
        ((mod4Mask, xK_r), spawn "rox"),
        ((mod4Mask, xK_f), spawn "firefox"),
        ((mod4Mask, xK_d), spawn "linuxdcpp"),
        ((mod4Mask, xK_t), spawn "urxvt -e \"/usr/bin/tmux\""),
        ((0, 0x1008FF13), spawn "amixer -q set Master 10%+"),
        ((0, 0x1008FF11), spawn "amixer -q set Master 10%-"),
        ((0, 0x1008FF12), spawn "amixer -q set Master toggle"),
        ((0, 0x1008FF17), spawn "audtool playlist-advance"),
        ((0, 0x1008FF16), spawn "audtool playlist-reverse"),
        ((0, 0x1008FF14), spawn "audtool playback-playpause"),
        ((mod4Mask, xK_s), spawnSelected' [
                                                ("Gnome-Terminal", "gnome-terminal"),
                                                ("VLC", "vlc"),
                                                ("Firefox", "firefox"),
                                                ("DC++", "linuxdcpp"),
                                                ("Calibre", "calibre"),
                                                ("ROX", "rox"),
                                                ("MComix", "mcomix"),
                                                ("Screen Lock", "xscreensaver-command -lock"),
                                                ("Nautilus", "nautilus --no-desktop"),
                                                ("CMUS", "urxvt -e \"/usr/bin/cmus\""),
                                                ("Audacious", "audacious"),
                                                ("Deluge", "deluge"),
                                                ("HTOP", "urxvt -e \"/usr/bin/htop\""),
                                                ("Tome", "torify tome4"),
                                                ("NAO", "urxvt -e \"/usr/bin/ssh nethack@alt.org\""),
                                                ("Matlab", "matlab"),
                                                ("RhythmBox", "rhythmbox"),
                                                ("TexMaker", "texmaker"),
                                                ("VirtualBox","virtualbox"),
                                                ("Clementine", "clementine")
                                                ]),
        ((mod4Mask, xK_g), goToSelected defaultGSConfig)
        ]-- }}}
myKeys = myKeyBindings-- {{{
        ++
        [((m .|.  mod4Mask, k), windows $ f i)
        | (i, k) <- zip (myWorkspaces) [xK_1 .. xK_9]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]-- }}}


data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where-- {{{
    urgencyHook LibNotifyUrgencyHook w = do
            name     <- getName w
            Just idx <- fmap (W.findTag w) $ gets windowset
            safeSpawn "notify-send" [show name, "workspace " ++ idx]-- }}}

main = do-- {{{
    xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
    xmonad $ withUrgencyHook LibNotifyUrgencyHook $ ewmh defaultConfig
        {
        --Hooks and Layouts
        manageHook = manageDocks <+> myManageHook,
        layoutHook = avoidStruts  $  layoutHook defaultConfig,
        startupHook = setWMName "LG3D",
        --Xmobar
        logHook = dynamicLogWithPP xmobarPP{
            ppOutput    = hPutStrLn xmproc,
            ppTitle     = xmobarColor myTitleColor "" . shorten myTitleLength,
            ppCurrent   = xmobarColor myCurrentWSColor "" . wrap myCurrentWSLeft myCurrentWSRight,
            ppVisible   = xmobarColor myVisibleWSColor "" . wrap myVisibleWSLeft myVisibleWSRight,
            ppUrgent    = xmobarColor myUrgentWSColor "" . wrap myUrgentWSLeft myUrgentWSRight
        } >> setWMName "LG3D",

        --Simple Variables
        terminal = myTerminal,
        borderWidth = myBorderWidth,
        workspaces  = myWorkspaces,
        --colors
        normalBorderColor   = myNormalBorderColor,
        focusedBorderColor  = myFocusedBorderColor
        --Bindings
        }`additionalKeys` myKeys-- }}}
