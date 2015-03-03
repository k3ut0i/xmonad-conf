import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import qualified Data.Map as M
main = xmonad myConfig

myConfig = defaultConfig
        {
        --Hooks ans layouts
        manageHook = manageDocks <+> manageHook defaultConfig,
        layoutHook = avoidStruts  $  layoutHook defaultConfig,
        --Simple Variables
        terminal = "urxvt",
        borderWidth = 4,
        --Bindings
        keys = myKeys
        }

myKeys conf@(XConfig{XMonad.modMask = mod1Mask})= M.fromList [
        ((mod4Mask , xK_z), spawn "xscreensaver-command -lock"),
        ((mod4Mask , xK_p), spawn "scrot \"%Y-%m-%d-%s_$wx$h.png\" -e \"mv $f ~/Pictures/Scrots/\""),
        ((0, xK_Print), spawn "scrot")
        ]


