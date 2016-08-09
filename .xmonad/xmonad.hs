import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run
import XMonad.Layout.Spacing

import System.Directory
import System.IO
import Text.Printf
import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = do
    home <- getHomeDirectory
    xmproc <- spawnPipe "xmobar"
    xmonad $ desktopConfig
        { terminal           = "st dvtm -M -h 1000 -d 0"
        , focusedBorderColor = "#555"
        , normalBorderColor  = "black"
        , borderWidth        = 2
        , handleEventHook    = mconcat [docksEventHook, handleEventHook def]
        , keys               = myKeys home
        , logHook            = dynamicLogWithPP (barConfig xmproc)
        , layoutHook         = smartSpacing 8 $ avoidStruts $ layoutHook def
        , manageHook         = manageDocks <+> manageHook def
        }

barConfig :: Handle -> PP
barConfig h = xmobarPP
    { ppCurrent         = xmobarColor white   black  . wrap " " " "
    , ppHiddenNoWindows = xmobarColor light   dark   . wrap " " " "
    , ppHidden          = xmobarColor light   dark   . wrap "◦" " "
    , ppUrgent          = xmobarColor black   red
    , ppLayout          = const ""
    , ppWsSep           = ""
    , ppSep             = " ░ "
    , ppOutput          = hPutStrLn h
    }
  where
    red   = "red"
    white = "white"
    black = "black"
    light = "#888888"
    dark  = "#333333"

toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig { XMonad.modMask = modMask } = (modMask, xK_b)

myKeys :: FilePath -> XConfig Layout -> Map (KeyMask, KeySym) (X ())
myKeys home conf@XConfig { XMonad.modMask = modMask } =
    Map.union ks (XMonad.keys def conf)
  where
    ks = Map.fromList
       [ ((modMask, xK_F12),     safeSpawn "systemctl" ["suspend"])
       , ((modMask, xK_p),       dmenu)
       , ((noModMask, xK_Print), spawn $ printf "scrot -u -e 'mv $f %s/screenshots'" home)
       ]
    dmenu :: X ()
    dmenu = safeSpawn
        "dmenu_run" [ "-fn", "'xft:DejaVu Sans Mono:size=12:antialias=true:hinting=true:hintstyle=hintslight'"
                    , "-i"
                    , "-nb", "black"
                    , "-nf", "#bbb"
                    , "-sb", "#444"
                    , "-sf", "white"
                    ]
