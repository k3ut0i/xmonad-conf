import XMonad

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
import XMonad.Layout.Gaps
import XMonad.Layout.Accordion

import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Util.NamedWindows
import System.IO
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Data.List


myTitleColor, myCurrentWSColor, myVisibleWSColor, myUrgentWSColor :: String
myCurrentWSLeft, myCurrentWSRight, myVisibleWSLeft, myVisibleWSRight, myUrgentWSLeft, myUrgentWSRight :: String
myTitleLength :: Int

myTitleColor        = "#ffffff"
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

myTerminal      = "urxvt"
myBorderWidth   = 1
myWorkspaces = ["web", "dev", "doc", "acd", "cal", "com", "med" , "dow", "mus"]
myNormalBorderColor = "#7c7c7c"
myFocusedBorderColor = "#ffb6b0"


myManageHook    = composeAll . concat $
                    [[className =? "Firefox"      --> doShift "web"],
                     [className =? "firefox" --> doShift "web"],
                    [className =? "libprs500"    --> doShift "cal"],
                    [className =? "FBReader"    --> doShift "cal"],
                    [className =? "Linuxdcpp"    --> doShift "dow"],
                    [className =? "MPlayer"    --> doShift "med"],
                    [className =? "Audacious"    --> doShift "mus"],
                    [className =? "Rhythmbox"    --> doShift "mus"],
                    [className =? "MComix"    --> doShift "com"],
                    [className =? "Deluge"    --> doShift "dow"],
                    [className =? "Emacs" --> doShift "dev"],
                    [title     =? t --> doFloat | t<-myTitleFloats],
                    [className  =? c --> doFloat | c<-myClassFloats]]
                    where
                    myTitleFloats   = ["Transferring", "Dialog", "Mailcheck"]
                    myClassFloats   = ["Pinentry", "Yad", "Audacious", "XVroot", "XTerm", "Conky", "Tilda"]


spawnSelected' :: [(String, String)] -> X()
spawnSelected' lst = gridselect def lst >>= flip whenJust spawn

myKeyBindings :: [((KeyMask,KeySym), X())]
myKeyBindings =  [
        ((mod4Mask , xK_z), spawn "xtrlock"),
        ((0, xK_Print), spawn "scrot"),
        ((0, 0x1008FF13), spawn "amixer -q set Master 10%+"),
        ((0, 0x1008FF11), spawn "amixer -q set Master 10%-"),
        ((0, 0x1008FF12), spawn "amixer -q set Master toggle"),
        ((0, 0x1008FF02), spawn "xbacklight -inc 10"),
        ((0, 0x1008FF03), spawn "xbacklight -dec 10"),
        ((0, 0x1008FF17), spawn "mpc next"),
        ((0, 0x1008FF16), spawn "mpc prev "),
        ((0, 0x1008FF14), spawn "mpc toggle"),
        ((mod4Mask, xK_d), sendMessage ToggleStruts),
        ((mod4Mask, xK_s), spawnSelected' [
                                                ("Gnome-Terminal", "gnome-terminal"),
                                                ("VLC", "vlc"),
                                                ("Firefox", "firefox"),
                                                ("DC++", "linuxdcpp"),
                                                ("Calibre", "calibre"),
                                                ("ROX", "rox"),
                                                ("MComix", "mcomix"),
                                                ("XScreenLock", "xscreensaver-command -lock"),
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
                                                ("Clementine", "clementine"),
                                                ("EmacsX", "emacs"),
                                                ("EmacsClient", "emacsclient -c")
                                                ]),
        ((mod4Mask, xK_g), goToSelected def)
        ]

myKeys :: [((KeyMask, KeySym), X())]                 
myKeys = myKeyBindings
        ++
        [((m .|.  mod4Mask, k), windows $ f i)
        | (i, k) <- zip myWorkspaces [xK_1 .. xK_9]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
            name     <- getName w
            Just idx <- W.findTag w <$> gets windowset
            safeSpawn "notify-send" [show name, "workspace " ++ idx]

myLayoutHook = noBorders $ myFull ||| myTabs ||| myTall
               where
                 myTabs = tabbed shrinkText def
                 myTall = Tall 1 (3/100) (1/2)
                 myFull = Full
main :: IO()
main = do
    xmproc <- spawnPipe "~/.local/bin/xmobar ~/.xmonad/xmobarrc"
--    spawn "~/.local/bin/xmobar ~/.xmonad/xmobarrcBottom"
 
    xmonad $ withUrgencyHook LibNotifyUrgencyHook $ ewmh def
        {

        manageHook = manageDocks <+> myManageHook,
        layoutHook = avoidStruts  $  myLayoutHook,
        startupHook = setWMName "LG3D",
        handleEventHook = mconcat [ docksEventHook
                                  , handleEventHook def ] ,
        logHook = dynamicLogWithPP xmobarPP{
            ppOutput    = hPutStrLn xmproc,
            ppTitle     = xmobarColor myTitleColor "" . shorten myTitleLength,
            ppCurrent   = xmobarColor myCurrentWSColor "" . wrap myCurrentWSLeft myCurrentWSRight,
            ppVisible   = xmobarColor myVisibleWSColor "" . wrap myVisibleWSLeft myVisibleWSRight,
            ppUrgent    = xmobarColor myUrgentWSColor "" . wrap myUrgentWSLeft myUrgentWSRight
        } >> setWMName "LG3D",


        terminal = myTerminal,
        borderWidth = myBorderWidth,
        workspaces  = myWorkspaces,
        modMask = mod4Mask,

        normalBorderColor   = myNormalBorderColor,
        focusedBorderColor  = myFocusedBorderColor

        }`additionalKeys` myKeys
