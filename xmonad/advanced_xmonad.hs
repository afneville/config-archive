 -- Base
import XMonad
import System.Directory
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

    -- Actions
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import qualified XMonad.Actions.TreeSelect as TS
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)
import qualified XMonad.Actions.Search as S

    -- Data
import Data.Char (isSpace, toUpper)
import Data.Maybe (fromJust)
import Data.Monoid
import Data.Maybe (isJust)
import Data.Tree
import qualified Data.Map as M

    -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory

    -- Layouts
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns

    -- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
-- import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed as R
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.BoringWindows
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

   -- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce

myFont :: String
myFont = "xft:Source Code Pro:regular:size=12:antialias=true:hinting=true"

myEmojiFont :: String
myEmojiFont = "xft:JoyPixels:regular:size=9:antialias=true:hinting=true"

myModMask :: KeyMask
myModMask = mod4Mask       -- Sets modkey to super/windows key

myTerminal :: String
myTerminal = "alacritty"   -- Sets default terminal

myBrowser :: String
myBrowser = "firefox"               -- Sets qutebrowser as browser for tree select

myEditor :: String
--myEditor = "emacsclient -c"  -- Sets emacs as editor for tree select
myEditor = "emacs"  -- Sets emacs as editor for tree select
-- myEditor = myTerminal ++ " -e vim "    -- Sets vim as editor for tree select

myBorderWidth :: Dimension
myBorderWidth = 1          -- Sets border width for windows

myNormColor :: String
myNormColor   = "#282c34"  -- Border color of normal windows

myFocusColor :: String
myFocusColor  = "#46d9ff"  -- Border color of focused windows

altMask :: KeyMask
altMask = mod1Mask         -- Setting this for use in xprompts

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myStartupHook :: X ()
myStartupHook = do

      spawnOnce "/usr/bin/emacs --daemon &" -- emacs daemon for the emacsclient
      --spawnOnce "nitrogen --restore" -- emacs daemon for the emacsclient
      spawnOnce "hsetroot -solid \"#333333\""
      spawnOnce "xsetroot -cursor_name left_ptr &"
      setWMName "LG3D"

  --Makes setting the spacingRaw simpler to write. The spacingRaw module adds a configurable amount of space around windows.
-- mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
-- mySpacing i = spacingRaw False (Border 0 5 0 5) True (Border 5 0 5 0) True
-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
-- mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
-- mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True
myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
    { swn_font              = "xft:Ubuntu:bold:size=40"
    , swn_fade              = 0.5
    , swn_bgcolor           = "#1c1f24"
    , swn_color             = "#ffffff"
    }
-- Defining a bunch of layouts, many that I don't use.
-- limitWindows n sets maximum number of windows displayed for layout.
-- mySpacing n sets the gap size around the windows.
-- tall = renamed [R.Replace "tall"]
--             -- $ windowNavigation
--             -- $ addTabs shrinkText myTabTheme
--             -- $ subLayout [] (withBorders Simplest)
--             -- $ limitWindows 12
--             -- $ mySpacing 8
--             $ ResizableTall 1 (3/100) (1/2) []

-- monocle  = renamed [Replace "monocle"]
--            $ windowNavigation
--            $ addTabs shrinkText myTabTheme
--            -- $ subLayout [] (smartBorders Simplest)
--            -- $ mySpacing 8
--            $ limitWindows 20 Full

-- setting colors for tabs layout and tabs sublayout.
-- myTabTheme = def { fontName            = myFont
--                  , activeColor         = "#46d9ff"
--                  , inactiveColor       = "#313846"
--                  , activeBorderColor   = "#46d9ff"
--                  , inactiveBorderColor = "#282c34"
--                  , activeTextColor     = "#282c34"
--                  , inactiveTextColor   = "#d0d0d0"
--                  }

-- Theme for showWName which prints current workspace when you change workspaces.


-- The layout hook
-- myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats
-- myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
-- myLayoutHook = avoidStruts $ myDefaultLayout
--                 where
--                   myDefaultLayout = tall
--                                  ||| Full
                                 -- ||| noBorders monocle
                                 -- ||| floats
                                 -- ||| noBorders tabs
                                 -- ||| grid
                                 -- ||| spirals
                                 -- ||| threeCol
                                 -- ||| threeRow

--myLayoutHook = avoidStruts (windowNavigation $ subTabbed $ boringWindows $
--                       Tall 1 (3/100) (1/2)
--                       ||| Full)
 -- magnify  = renamed [Replace "magnify"]
            -- $ windowNavigation
            -- $ addTabs shrinkText myTabTheme
            -- $ subLayout [] (smartBorders Simplest)
            -- $ magnifier
            -- $ limitWindows 12
            -- $ mySpacing 8
            -- $ ResizableTall 1 (3/100) (1/2) []
 -- threeCol = renamed [Replace "threeCol"]
            -- $ windowNavigation
            -- $ addTabs shrinkText myTabTheme
            -- $ subLayout [] (smartBorders Simplest)
            -- $ limitWindows 7
            -- $ ThreeCol 1 (3/100) (1/2)
 -- threeRow = renamed [Replace "threeRow"]
            -- $ windowNavigation
            -- $ addTabs shrinkText myTabTheme
            -- $ subLayout [] (smartBorders Simplest)
            -- $ limitWindows 7
            -- -- Mirror takes a layout and rotates it by 90 degrees.
            -- -- So we are applying Miror to the ThreeCol layout.
            -- $ Mirror
            -- $ ThreeCol 1 (3/100) (1/2)
 -- tabs     = renamed [Replace "tabs"]
            -- -- I cannot add spacing to this layout because it will
            -- -- add spacing between window and tabs which looks bad.
            -- $ tabbed shrinkText myTabTheme
 -- grid     = renamed [Replace "grid"]
            -- $ windowNavigation
            -- $ addTabs shrinkText myTabTheme
            -- $ subLayout [] (smartBorders Simplest)
            -- $ limitWindows 12
            -- $ mySpacing 0
            -- $ mkToggle (single MIRROR)
            -- $ Grid (16/10)
 -- floats   = renamed [Replace "floats"]
            -- $ windowNavigation
            -- $ addTabs shrinkText myTabTheme
            -- $ subLayout [] (smartBorders Simplest)
            -- $ limitWindows 20 simplestFloat
 -- 
 -- spirals  = renamed [Replace "spirals"]
            -- $ windowNavigation
            -- $ addTabs shrinkText myTabTheme
            -- $ subLayout [] (smartBorders Simplest)
            -- $ mySpacing' 8
            -- $ spiral (6/7)
myLayoutHook = avoidStruts (tiled ||| Full)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100


myWorkspaces = ["gen", "www", "ps1", "dev", "sys", "doc"]
-- myWorkspaces = ["1", "2", "3", "4", "5", "6", "7"]
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..] -- (,) == \x y -> (x,y)

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
     -- using 'doShift ( myWorkspaces !! 7)' sends program to workspace 8!
     -- I'm doing it this way because otherwise I would have to write out the full
     -- name of my workspaces, and the names would very long if using clickable workspaces.
     [
     ]

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
    where fadeAmount = 1.0

myKeys :: [(String, X ())]
myKeys =
    -- Xmonad
        [ ("M-C-r", spawn "xmonad --recompile") -- Recompiles xmonad
        , ("M-q", spawn "xmonad --restart")   -- Restarts xmonad
        , ("M-S-q", io exitSuccess)             -- Quits xmonad

    -- Useful programs to have a keybinding for launch
        , ("M-<Return>", spawn (myTerminal))
        , ("M-e", spawn (myEditor))
        , ("M-p", spawn ("rofi -show run"))

    -- Kill windows
        , ("M-c", kill1)     -- Kill the currently focused client
        , ("M-S-c", killAll)   -- Kill all windows on current workspace

    -- Workspaces
        --, ("M-.", nextScreen)  -- Switch focus to next monitor
        --, ("M-,", prevScreen)  -- Switch focus to prev monitor
        --, ("M-S-<KP_Add>", shiftTo Next nonNSP >> moveTo Next nonNSP)       -- Shifts focused window to next ws
        --, ("M-S-<KP_Subtract>", shiftTo Prev nonNSP >> moveTo Prev nonNSP)  -- Shifts focused window to prev ws

    -- Floating windows
        --, ("M-f", sendMessage (T.Toggle "floats")) -- Toggles my 'floats' layout
        , ("M-t", withFocused $ windows . W.sink)  -- Push floating window back to tile
        , ("M-S-t", sinkAll)                       -- Push ALL floating windows to tile

    -- Increase/decrease spacing (gaps)
        , ("M-d", decWindowSpacing 4)           -- Decrease window spacing
        , ("M-i", incWindowSpacing 4)           -- Increase window spacing
        , ("M-S-d", decScreenSpacing 4)         -- Decrease screen spacing
        , ("M-S-i", incScreenSpacing 4)         -- Increase screen spacing

    -- Windows navigation
        , ("M-m", windows W.focusMaster)  -- Move focus to the master window
        , ("M-j", windows W.focusDown)    -- Move focus to the next window
        , ("M-<Tab>", windows W.focusDown)    -- Move focus to the next window
        , ("M-k", windows W.focusUp)      -- Move focus to the prev window
        , ("M-S-m", windows W.swapMaster) -- Swap the focused window and the master window
        , ("M-S-j", windows W.swapDown)   -- Swap focused window with next window
        , ("M-S-k", windows W.swapUp)     -- Swap focused window with prev window
        , ("M-<Backspace>", promote)      -- Moves focused window to master, others maintain order
        , ("M-S-<Tab>", rotSlavesDown)    -- Rotate all windows except master and keep focus in place
        , ("M-C-<Tab>", rotAllDown)       -- Rotate all the windows in the current stack

    -- Layouts
        , ("M-<Space>", sendMessage NextLayout)           -- Switch to next layout
        --, ("M-C-M1-<Up>", sendMessage Arrange)
        --, ("M-C-M1-<Down>", sendMessage DeArrange)
        --, ("M-f", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full
        , ("M-b", sendMessage ToggleStruts)     -- Toggles struts
        --, ("M-S-n", sendMessage $ MT.Toggle NOBORDERS)  -- Toggles noborder

    -- Increase/decrease windows in the master pane or the stack
        , ("M-S-<Up>", sendMessage (IncMasterN 1))      -- Increase number of clients in master pane
        , ("M-S-<Down>", sendMessage (IncMasterN (-1))) -- Decrease number of clients in master pane
        , ("M-C-<Up>", increaseLimit)                   -- Increase number of windows
        , ("M-C-<Down>", decreaseLimit)                 -- Decrease number of windows

    -- Window resizing
        , ("M-h", sendMessage Shrink)                   -- Shrink horiz window width
        , ("M-l", sendMessage Expand)                   -- Expand horiz window width
        , ("M-M1-j", sendMessage MirrorShrink)          -- Shrink vert window width
        , ("M-M1-k", sendMessage MirrorExpand)          -- Exoand vert window width

    -- Sublayouts
    -- This is used to push windows to tabbed sublayouts, or pull them out of it.
        , ("M-C-h", sendMessage $ pullGroup L)
        , ("M-C-l", sendMessage $ pullGroup R)
        , ("M-C-k", sendMessage $ pullGroup U)
        , ("M-C-j", sendMessage $ pullGroup D)
        , ("M-C-m", withFocused (sendMessage . MergeAll))
        , ("M-C-u", withFocused (sendMessage . UnMerge))
        , ("M-C-/", withFocused (sendMessage . UnMergeAll))
        , ("M-C-.", onGroup W.focusUp')    -- Switch focus to next tab
        , ("M-C-,", onGroup W.focusDown')  -- Switch focus to prev tab

    -- Multimedia Keys
        , ("<XF86AudioMute>",   spawn "amixer set Master toggle")
        , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
        , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
        , ("<XF86Eject>", spawn "screenshot")
        ]

main :: IO ()
main = do
    -- Launching three instances of xmobar on their monitors.
    xmproc <- spawnPipe "xmobar /home/alex/.xmonad/.xmobarrc"
    -- the xmonad, ya know...what the WM is named after!
    xmonad $ ewmh def
        { manageHook = ( isFullscreen --> doFullFloat ) <+> myManageHook <+> manageDocks
        -- Run xmonad commands from command line with "xmonadctl command". Commands include:
        -- shrink, expand, next-layout, default-layout, restart-wm, xterm, kill, refresh, run,
        -- focus-up, focus-down, swap-up, swap-down, swap-master, sink, quit-wm. You can run
        -- "xmonadctl 0" to generate full list of commands written to ~/.xsession-errors.
        -- To compile xmonadctl: ghc -dynamic xmonadctl.hs
        , handleEventHook    = serverModeEventHookCmd
                               <+> serverModeEventHook
                               <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
                               <+> docksEventHook
        , modMask            = myModMask
        , terminal           = myTerminal
        , startupHook        = myStartupHook
        , layoutHook         = spacingRaw False (Border 0 5 0 5) True (Border 5 0 5 0) True $ showWName' myShowWNameTheme $ myLayoutHook
--        , layoutHook         = showWName' myShowWNameTheme $ myLayoutHook
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormColor
        , focusedBorderColor = myFocusColor
        , logHook = workspaceHistoryHook <+> myLogHook <+> dynamicLogWithPP xmobarPP
                        { ppOutput = \x -> hPutStrLn xmproc x
                        , ppCurrent = xmobarColor "#98be65" "" . wrap "" ""           -- Current workspace in xmobar
                        , ppVisible = xmobarColor "#98be65" ""             -- Visible but not current workspace
                        , ppHidden = xmobarColor "#82AAFF" "" . wrap "" "" -- Hidden workspaces in xmobar
                        , ppHiddenNoWindows = xmobarColor "#c792ea" ""    -- Hidden workspaces (no windows)
                        , ppTitle = xmobarColor "#b3afc2" "" . shorten 0               -- Title of active window in xmobar
                        , ppSep =  " | "                    -- Separators in xmobar
                        , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"            -- Urgent workspace
                        , ppExtras  = [windowCount]                                     -- # of windows current workspace
                        , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                        }
        } `additionalKeysP` myKeys
