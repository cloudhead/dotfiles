import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run
import XMonad.Layout.Spacing
import XMonad.StackSet
import XMonad.ManageHook
import XMonad.Layout.NoBorders
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.CycleWS
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.FloatNext

import System.Directory
import System.IO
import Text.Printf
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad

termName :: FilePath
termName = "st"

main :: IO ()
main = do
    home <- getHomeDirectory
    xmproc <- spawnPipe "xmobar"
    xmonad $ withUrgencyHook NoUrgencyHook $ ewmh $ desktopConfig
        { terminal           = termName
        , focusedBorderColor = "#555"
        , normalBorderColor  = "#1F1F1F"
        , borderWidth        = 1
        , handleEventHook    = mconcat [docksEventHook, handleEventHook def]
        , keys               = myKeys home
        , logHook            = dynamicLogWithPP (barConfig xmproc)
        , layoutHook         = avoidStruts $ smartBorders $ layoutHook def
        , manageHook         = floatNextHook <+> manageDocks <+> manageHook def
        , startupHook        = startup
        }

startup :: X ()
startup = do
    -- Open a terminal if none are open.
    withWindowSet $ \wins -> do
        result <- filterM (runQuery (title =? termName)) (allWindows wins)
        when (null result) $
            safeSpawnProg termName

barConfig :: Handle -> PP
barConfig h = xmobarPP
    { ppCurrent         = xmobarColor white   black  . wrap " " " "
    , ppHiddenNoWindows = xmobarColor grey    black  . wrap " " " "
    , ppHidden          = xmobarColor light   dark   . wrap " " " "
    , ppUrgent          = xmobarColor black   red    . wrap " " " "
    , ppTitle           = xmobarColor red     black
    , ppLayout          = const ""
    , ppWsSep           = ""
    , ppSep             = "  "
    , ppOutput          = hPutStrLn h
    }
  where
    red   = "#AA0000"
    white = "white"
    black = "black"
    light = "#888888"
    dark  = "#333333"
    grey  = "#555555"

toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig { XMonad.modMask = modMask } = (modMask, xK_b)

myKeys :: FilePath -> XConfig Layout -> Map (KeyMask, KeySym) (X ())
myKeys home conf@XConfig { XMonad.modMask = modMask } =
    Map.union ks (XMonad.keys def conf)
  where
    ks = Map.fromList
       [ ((modMask, xK_F12),                    safeSpawn "sleep" [])
       , ((modMask .|. shiftMask, xK_F12),      safeSpawn "systemctl" ["hibernate"])
       , ((modMask, xK_F5),                     safeSpawn "refresh-display" [])
       , ((modMask, xK_p),                      dmenu)
       , ((modMask, xK_Tab),                    toggleWS)
       , ((modMask .|. controlMask, xK_Return), toggleFloatNext >> (spawn $ XMonad.terminal conf))
       , ((noModMask, xK_Print),                spawn $ printf "scrot -u -e 'mv $f %s/screenshots'" home)
       ]
    dmenu :: X ()
    dmenu = safeSpawn
        "dmenu_run" [ "-fn", "Inconsolata-14,Monospace-12"
                    , "-i"
                    , "-y", "-2"
                    , "-h",  "40"
                    , "-nb", "black"
                    , "-nf", "#bbb"
                    , "-sb", "#444"
                    , "-sf", "white"
                    ]
