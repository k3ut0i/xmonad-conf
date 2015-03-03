import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
main = xmonad $ myConfig `additionalKeys` myKeys
       

myConfig = defaultConfig
        {manageHook = manageDocks <+> manageHook defaultConfig,
        layoutHook = avoidStruts  $  layoutHook defaultConfig,
        terminal = "urxvt",
        borderWidth = 3
        }
myKeys =  [
        ((mod4Mask , xK_z), spawn "xscreensaver-command -lock"),
        ((mod4Mask , xK_p), spawn "scrot \"%Y-%m-%d-%s_$wx$h.png\" -e \"mv $f ~/Pictures/Scrots/\""),
        ((0, xK_Print), spawn "scrot")
        ]
