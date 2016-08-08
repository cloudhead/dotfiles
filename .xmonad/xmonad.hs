import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

main :: IO ()
main = xmonad =<< statusBar "xmobar" barConfig toggleStrutsKey desktopConfig
     { terminal           = "st dvtm -M"
     , focusedBorderColor = "#666666"
     , normalBorderColor  = "black"
     , borderWidth        = 2
     , handleEventHook    = mconcat [docksEventHook, handleEventHook def]
     }

barConfig :: PP
barConfig = xmobarPP
    { ppCurrent         = xmobarColor white   black  . wrap " " " "
    , ppHiddenNoWindows = xmobarColor light   dark   . wrap " " " "
    , ppHidden          = xmobarColor light   dark   . wrap "◦" " "
    , ppUrgent          = xmobarColor black   red
    , ppWsSep           = ""
    , ppSep             = " ░ "
    }
  where
    red   = "red"
    white = "white"
    black = "black"
    light = "#888888"
    dark  = "#333333"

toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig { XMonad.modMask = modMask } = (modMask, xK_b)
