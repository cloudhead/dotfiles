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
import System.FilePath
import Text.Printf
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad

data InputPrompt = InputPrompt String

instance XPrompt InputPrompt where
    showXPrompt (InputPrompt s) = " " ++ s

inputPrompt :: XPConfig -> String -> X (Maybe String)
inputPrompt c p = mkXPromptWithReturn (InputPrompt p) c (const (pure [])) return

data EnterPrompt = EnterPrompt String

instance XPrompt EnterPrompt where
    showXPrompt (EnterPrompt n) = " " ++ n ++ " "

screenshotPrompt :: String -> Bool -> X ()
screenshotPrompt home select = do
    str <- inputPrompt cfg "~/screenshots/"
    case str of
        Just s  -> spawn $ printf "sleep 0.3 && scrot %s '%s' -e 'mv $f ~/screenshots'" mode s
        Nothing -> pure ()
  where
    mode = if select then "--select" else "--focused"
    cfg = myXPConfig { position = CenteredAt 0.5 0.3
                     , defaultText = "" }

editPrompt :: String -> X ()
editPrompt home = do
    str <- inputPrompt cfg "$ edit ~/"
    case str of
        Just s  -> openInEditor s
        Nothing -> pure ()
  where
    cfg = myXPConfig { position = CenteredAt 0.5 0.3
                     , defaultText = "" }

openInEditor :: String -> X ()
openInEditor path =
    safeSpawn termName ["nvim", path]

termName :: FilePath
termName = "kitty"

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
        , layoutHook         = smartBorders $ avoidStruts $ myLayout
        , manageHook         = floatNextHook <+> manageDocks <+> manageHook def
        , startupHook        = docksStartupHook
        } `additionalKeysP` myXF86Keys
  where
    myLayout = tiled ||| Mirror tiled ||| Full
    tiled    = Tall nmaster delta ratio
    nmaster  = 1
    ratio    = 1/2
    delta    = 4/100

    myXF86Keys =
       [ ("<XF86MonBrightnessUp>",      safeSpawn "light"  ["-A", "5"])
       , ("<XF86MonBrightnessDown>",    safeSpawn "light"  ["-U", "5"])
       , ("<XF86AudioRaiseVolume>",     safeSpawn "amixer" ["-q", "sset", "Master", "2+"])
       , ("<XF86AudioLowerVolume>",     safeSpawn "amixer" ["-q", "sset", "Master", "2-"])
       , ("<XF86AudioMute>",            safeSpawn "amixer" ["-q", "sset", "Master", "toggle"])
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
    , ppHiddenNoWindows = xmobarColor dgrey   black  . wrap " " " "
    , ppHidden          = xmobarColor grey    black  . wrap " " " "
    , ppUrgent          = xmobarColor black   red    . wrap " " " "
    , ppTitle           = xmobarColor red     black
    , ppLayout          = const ""
    , ppWsSep           = "  "
    , ppSep             = "  "
    , ppOutput          = hPutStrLn h
    }
  where
    red   = "maroon"
    dred  = "maroon"
    white = "white"
    black = "black"
    light = "#888888"
    dark  = "#333333"
    grey  = "#777777"
    dgrey = "#333333"

toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig { XMonad.modMask = modMask } = (modMask, xK_b)

myXPConfig :: XPConfig
myXPConfig = def
  { position          = CenteredAt 0.5 0.1
  , alwaysHighlight   = True
  , height            = 60
  , promptBorderWidth = 1
  , font              = "xft:monospace:size=13"
  , borderColor       = "aquamarine"
  , bgColor           = "black"
  }

myKeys :: FilePath -> XConfig Layout -> Map (KeyMask, KeySym) (X ())
myKeys home conf@XConfig { XMonad.modMask = modMask } =
    Map.union ks (XMonad.keys def conf)
  where
    ks = Map.fromList
       [ ((modMask, xK_F12),                    safeSpawn "systemctl" ["suspend"])
       , ((modMask .|. shiftMask, xK_F12),      safeSpawn "systemctl" ["hibernate"])
       , ((modMask, xK_F11),                    safeSpawnProg "slock")
       , ((modMask, xK_F10),                    safeSpawnProg "toggle-displays")
       , ((modMask, xK_F5),                     safeSpawnProg "refresh-display")
       , ((modMask .|. controlMask, xK_r),      safeSpawn "killall" ["kitty", "-USR1"])
       , ((modMask, xK_s),                      safeSpawn "autorandr" ["--cycle"])
       , ((modMask, xK_p),                      runExecutable)
       , ((modMask .|. shiftMask, xK_p),        editPrompt home)
       , ((modMask, xK_o),                      switchWindow)
       , ((modMask, xK_Tab),                    toggleWS)
       , ((modMask .|. controlMask, xK_Return), toggleFloatNext >> (spawn $ XMonad.terminal conf))
       , ((modMask, xK_Print),                  screenshotPrompt home False)
       , ((modMask .|. shiftMask, xK_Print),    screenshotPrompt home True)
       , ((modMask, xK_0),                      windows $ greedyView "ω")
       , ((modMask .|. shiftMask, xK_0),        windows $ shift "ω")
       , ((modMask, xK_Left),                   prevWS)
       , ((modMask, xK_Right),                  nextWS)
       ]

rofi :: String -> [String] -> X ()
rofi mode opts = safeSpawn "rofi" (opts ++ ["-show", mode])

runExecutable :: X ()
runExecutable = rofi "run" []

switchWindow :: X ()
switchWindow = rofi "window" ["-width", "50", "-lines", "14"]
