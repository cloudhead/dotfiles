import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run
import XMonad.Layout.Spacing
import XMonad.StackSet hiding (workspaces)
import XMonad.ManageHook
import XMonad.Layout.NoBorders
import XMonad.Layout.ThreeColumns
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.CycleWS
import XMonad.Util.EZConfig (additionalKeys, additionalKeysP)
import XMonad.Prompt
import XMonad.Prompt.XMonad

import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.FloatNext

import Graphics.X11.ExtraTypes.XF86

import System.Directory
import System.IO
import System.Exit
import Text.Printf
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad

data EnterPrompt = EnterPrompt String

instance XPrompt EnterPrompt where
    showXPrompt (EnterPrompt n) = " " ++ n ++ " "

confirmPrompt :: XPConfig -> String -> X () -> X ()
confirmPrompt config app func = mkXPrompt (EnterPrompt app) config (mkComplFunFromList []) $ const func

termName :: FilePath
termName = "urxvt"

main :: IO ()
main = do
    home <- getHomeDirectory
    xmproc <- spawnPipe "xmobar"
    xmonad $ withUrgencyHook NoUrgencyHook $ ewmh $ desktopConfig
        { terminal           = termName
        , workspaces         = ["α", "β", "γ", "δ", "φ", "σ", "η", "θ", "λ", "ω"]
        , focusedBorderColor = "medium aquamarine"
        , normalBorderColor  = "#1F1F1F"
        , borderWidth        = 1
        , handleEventHook    = mconcat [docksEventHook, handleEventHook def]
        , keys               = myKeys home
        , logHook            = dynamicLogWithPP (barConfig xmproc)
        , layoutHook         = avoidStruts $ smartBorders $ myLayout
        , manageHook         = floatNextHook <+> manageDocks <+> manageHook def
        , startupHook        = startup <+> docksStartupHook
        } `additionalKeysP` myXF86Keys
  where
    myLayout = layoutHook def
    myXF86Keys =
       [ ("<XF86MonBrightnessUp>",              safeSpawn "xbacklight" ["-inc", "10"])
       , ("<XF86MonBrightnessDown>",            safeSpawn "xbacklight" ["-dec", "10"])
       ]

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
    , ppHiddenNoWindows = xmobarColor dred    black  . wrap " " " "
    , ppHidden          = xmobarColor grey    black  . wrap " " " "
    , ppUrgent          = xmobarColor black   red    . wrap " " " "
    , ppTitle           = xmobarColor red     black
    , ppLayout          = const ""
    , ppWsSep           = ""
    , ppSep             = "  "
    , ppOutput          = hPutStrLn h
    }
  where
    red   = "#AA0000"
    dred  = "#550000"
    white = "white"
    black = "black"
    light = "#888888"
    dark  = "#333333"
    grey  = "#555555"

toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig { XMonad.modMask = modMask } = (modMask, xK_b)

myXPConfig :: XPConfig
myXPConfig = def
  { position          = CenteredAt 0.5 0.09
  , alwaysHighlight   = True
  , height            = 60
  , promptBorderWidth = 1
  , font              = "xft:Inconsolata:size=14"
  , borderColor       = "#555555"
  , bgColor           = "#111111"
  }

myKeys :: FilePath -> XConfig Layout -> Map (KeyMask, KeySym) (X ())
myKeys home conf@XConfig { XMonad.modMask = modMask } =
    Map.union ks (XMonad.keys def conf)
  where
    ks = Map.fromList
       [ ((modMask, xK_F12),                    safeSpawn "sleep" [])
       , ((modMask .|. shiftMask, xK_F12),      safeSpawn "systemctl" ["hibernate"])
       , ((modMask, xK_F11),                    safeSpawn "slock" [])
       , ((modMask, xK_F10),                    safeSpawn "toggle-displays" [])
       , ((modMask, xK_F5),                     safeSpawn "refresh-display" [])
       , ((modMask, xK_p),                      rofi)
       , ((modMask, xK_Tab),                    toggleWS)
       , ((modMask .|. controlMask, xK_Return), toggleFloatNext >> (spawn $ XMonad.terminal conf))
       , ((modMask, xK_Print),                  spawn $ printf "scrot -u -e 'mv $f %s/screenshots'" home)
       , ((modMask .|. shiftMask, xK_Print),    safeSpawn "screenshot-region" [])
       , ((modMask, xK_0),                      windows $ greedyView "ω")
       , ((modMask .|. shiftMask, xK_0),        windows $ shift "ω")
       , ((modMask, xK_Left),                   prevWS)
       , ((modMask, xK_Right),                  nextWS)
       , ((modMask .|. shiftMask, xK_q),        confirmPrompt myXPConfig "Exit XMonad?" $ io (exitWith ExitSuccess))
       ]
    dmenu :: X ()
    dmenu = safeSpawn
        "dmenu_run" [ "-fn", "Inconsolata-14,Monospace-12"
                    , "-i"
                    , "-y", "-1"
                    , "-h",  "24"
                    , "-nb", "black"
                    , "-nf", "#bbb"
                    , "-sb", "#444"
                    , "-sf", "white"
                    ]

rofi :: X ()
rofi = safeSpawn
    "rofi" [ "-show", "run"
           , "-color-active", "#fdf6e3,#268bd2,#eee8d5,#268bd2,#fdf6e3"
           , "-color-normal", "#111,#bbb,#111,#bbb,#000"
           , "-color-urgent", "#fdf6e3,#dc322f,#eee8d5,#dc322f,#fdf6e3"
           , "-color-window", "#111,#666"
           , "-font", "Inconsolata 16"
           , "-hide-scrollbar"
           , "-levenshtein-sort"
           , "-lines", "7"
           , "-matching", "fuzzy"
           , "-padding", "5"
           , "-separator-style", "solid"
           , "-width", "30"
           ]
