import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
main = xmonad defaultConfig
        {
    terminal = "urxvt",
    borderWidth = 3
        }
