-- Base
import XMonad
import System.Directory
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

    -- Actions
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS (Direction1D(..), moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Actions.Navigation2D
import qualified XMonad.Actions.Search as S

    -- Data
import Data.Char (isSpace, toUpper)
import Data.Maybe (fromJust)
import Data.Monoid
import Data.Maybe (isJust)
import Data.Tree
import qualified Data.Map as M

    -- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
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
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))
-- import XMonad.Layout.BoringWindows

   -- Utilities
import XMonad.Util.Dmenu
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce

myFont :: String
myFont = "xft:Jetbrains Mono Nerd Font:regular:size=9:antialias=true:hinting=true"

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "tabbed -r 2 st -w ''"

myBrowser :: String
myBrowser = "brave"

myEmacs :: String
myEmacs = "emacsclient -c "

myEditor :: String
myEditor = "emacsclient -c "
-- myEditor = myTerminal ++ " -e nvim " -- if you want to use neovim etc.

myBorderWidth :: Dimension
myBorderWidth = 2

myNormColour :: String
myNormColour   = "#353C4A"

myFocusColour :: String
myFocusColour  = "#81A1C1"

altMask :: KeyMask
altMask = mod1Mask

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myStartupHook :: X ()
myStartupHook = do
          -- spawnOnce "nitrogen --restore &"
          setWMName "LG3D"

-- myColorizer :: Window -> Bool -> X (String, String)
-- myColorizer = colorRangeFromClassName
--                   (0x28,0x2c,0x34) -- lowest inactive bg
--                   (0x28,0x2c,0x34) -- highest inactive bg
--                   (0xc7,0x92,0xea) -- active bg
--                   (0xc0,0xa7,0x9a) -- inactive fg
--                   (0x28,0x2c,0x34) -- active fg
-- 
-- -- gridSelect menu layout
-- mygridConfig :: p -> GSConfig Window
-- mygridConfig colorizer = (buildDefaultGSConfig myColorizer)
--     { gs_cellheight   = 40
--     , gs_cellwidth    = 200
--     , gs_cellpadding  = 6
--     , gs_originFractX = 0.5
--     , gs_originFractY = 0.5
--     , gs_font         = myFont
--     }
-- 
-- spawnSelected' :: [(String, String)] -> X ()
-- spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
--     where conf = def
--                    { gs_cellheight   = 50
--                    , gs_cellwidth    = 150
--                    , gs_cellpadding  = 10
--                    , gs_originFractX = 0.5
--                    , gs_originFractY = 0.5
--                    , gs_font         = myFont
--                    }
-- 
-- myAppGrid = [
--                    ("Terminal", "alacritty")
--                  , ("G-Terminal", "gnome-terminal")
--                  , ("EmacsClient", "emacsclient -c ")
--                  , ("Emacs", "emacs")
--                  , ("Neovim", "alacritty -e nvim")
--                  , ("Htop", "alacritty -e htop")
--                  , ("Brave", "brave")
--                  , ("Firefox", "firefox")
--                  , ("Qutebrowser", "qutebrowser")
--                  , ("Discord", "discord")
--                  , ("Files", "pcmanfm")
--                  , ("Word", "lowriter")
--                  , ("Office", "libreoffice")
--                  , ("CAD", "librecad")
--                  , ("Network", "nm-connection-editor")
--                  , ("Appearance", "lxappearance")
--                  , ("Power", "xfce4-power-manager-settings")
--                  ]

-- myScratchPads :: [NamedScratchpad]
-- myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
--                 ]
--   where
--     spawnTerm  = myTerminal ++ " -t scratchpad"
--     findTerm   = title =? "scratchpad"
--     manageTerm = customFloating $ W.RationalRect l t w h
--                where
--                  h = 0.6
--                  w = 0.6
--                  t = 0.8 -h
--                  l = 0.8 -w

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

tall     = renamed [Replace "tall"]
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 12
           $ mySpacing 0
           $ ResizableTall 1 (3/100) (1/2) []
-- magnify  = renamed [Replace "magnify"]
--            $ windowNavigation
--            $ smartBorders
--            $ addTabs shrinkText myTabTheme
--            $ subLayout [] (smartBorders Simplest)
--            $ magnifier
--            $ limitWindows 12
--            $ mySpacing 5
--            $ ResizableTall 1 (3/100) (1/2) []
monocle  = renamed [Replace "monocle"]
           $ windowNavigation
           -- $ smartBorders
           -- $ addTabs shrinkText myTabTheme
           -- $ subLayout [] (smartBorders Simplest)
           $ limitWindows 20 Full

full     = renamed [Replace "full"]
           $ windowNavigation
           $ mySpacing 0
           $ limitWindows 20 Full
-- floats   = renamed [Replace "floats"] $ windowNavigation
--            $ smartBorders
--            $ addTabs shrinkText myTabTheme
--            $ subLayout [] (smartBorders Simplest)
--            $ limitWindows 20 simplestFloat
-- grid     = renamed [Replace "grid"]
--            $ windowNavigation
--            $ smartBorders
--            $ addTabs shrinkText myTabTheme
--            $ subLayout [] (smartBorders Simplest)
--            $ limitWindows 12
--            $ mySpacing 0
--            $ mkToggle (single MIRROR)
--            $ Grid (16/10)
-- spirals  = renamed [Replace "spirals"]
--            $ windowNavigation
--            $ smartBorders
--            $ addTabs shrinkText myTabTheme
--            $ subLayout [] (smartBorders Simplest)
--            $ mySpacing' 8
--            $ spiral (6/7)
-- threeCol = renamed [Replace "threeCol"]
--            $ windowNavigation
--            $ smartBorders
--            $ addTabs shrinkText myTabTheme
--            $ subLayout [] (smartBorders Simplest)
--            $ limitWindows 7
--            $ ThreeCol 1 (3/100) (1/2)
-- threeRow = renamed [Replace "threeRow"]
--            $ windowNavigation
--            $ smartBorders
--            $ addTabs shrinkText myTabTheme
--            $ subLayout [] (smartBorders Simplest)
--            $ limitWindows 7
--            -- Mirror takes a layout and rotates it by 90 degrees.
--            -- So we are applying Mirror to the ThreeCol layout.
--            $ Mirror
--            $ ThreeCol 1 (3/100) (1/2)
tabs     = renamed [Replace "tabs"]
           $ tabbed shrinkText myTabTheme

-- setting colors for tabs layout and tabs sublayout.
myTabTheme = def { fontName            = myFont
                 , activeColor         = "#81a1c1"
                 , inactiveColor       = "#353C4A"
                 , activeBorderColor   = "#81a1c1"
                 , inactiveBorderColor = "#353C4A"
                 , activeTextColor     = "#282c34"
                 , inactiveTextColor   = "#d0d0d0"
                 }

myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
    { swn_font              = "xft:Jetbrains Mono Nerd Font:bold:size=40"
    , swn_fade              = 1.0
    , swn_bgcolor           = "#1c1f24"
    , swn_color             = "#ffffff"
    }

-- The layout hook
myLayoutHook = avoidStruts $ mouseResize $ windowArrange
             $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
             where
               myDefaultLayout =     withBorder myBorderWidth tall
                                 ||| full
                                 ||| noBorders monocle
                                 ||| tabs

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
-- myWorkspaces = [" 1 ", " 2 ", " 3 ", " 4 ", " 5 ", " 6 ", " 7 ", " 8 ", " 9 "]
-- myWorkspaces = [" dev1 ", " dev2 ", " www ", " chat ", " doc ", " sys "]

myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..]

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
     [ className =? "confirm"         --> doFloat
     , className =? "file_progress"   --> doFloat
     , className =? "dialog"          --> doFloat
     , className =? "download"        --> doFloat
     , className =? "notification"    --> doFloat
     -- Browsers go to workspace 3 (www), except qutebrowser which can be summoned on any workspace.
     -- , className =? "Brave-browser" --> doShift ( myWorkspaces !! 2 )
     -- , className =? "firefox" --> doShift ( myWorkspaces !! 2 )
     -- , className =? "discord" --> doShift ( myWorkspaces !! 3 )
     , className =? "Nitrogen" --> doFloat
     -- , className =? "Nitrogen" --> doShift ( myWorkspaces !! 5 )
     -- , className =? "Lxappearance" --> doShift ( myWorkspaces !! 5 )
     -- , className =? "libreoffice-startcenter" --> doShift ( myWorkspaces !! 4 )
     , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
     ] 
     -- <+> namedScratchpadManageHook myScratchPads

myKeys :: [(String, X ())]
myKeys =
        [

        -- Xmonad actions
          ("M-C-r", spawn "xmonad --recompile")
        , ("M-S-r", spawn "xmonad --restart")
        , ("M-S-q", io exitSuccess)
        , ("M-b", spawn "killall xmobar")

        -- Spawn applications
        , ("M-p", spawn "dmenu_run -i -p \"Launch:\"")

        -- , ("M1-s p", spawn "picom --experimental-backends &")
        -- , ("M1-s b", spawn "xmobar /home/alex/.xmobarrc")

        , ("M-<Return>", spawn (myTerminal))
        , ("M-f", spawn (myTerminal ++ " -e vifm"))
        , ("M-s", spawn (myBrowser))

        -- Kill things
        , ("M-c", kill1)                -- Kill the currently focused client
        , ("M-S-c", killAll)            -- Kill all windows on current workspace
        , ("M1-k x", spawn "xkill" )

        -- movement between workspaces

        -- , ("M-.", nextScreen)  -- next monitor
        -- , ("M-,", prevScreen)  -- prev monitor
        -- , ("M-S-<Down>", shiftTo Next nonNSP >> moveTo Next nonNSP)
        -- , ("M-S-<Right>", shiftTo Next nonNSP >> moveTo Next nonNSP)
        -- , ("M-S-<Up>", shiftTo Prev nonNSP >> moveTo Prev nonNSP)
        -- , ("M-S-<Left>", shiftTo Prev nonNSP >> moveTo Prev nonNSP)
        -- , ("M-<Down>", moveTo Next nonNSP)
        -- , ("M-<Right>", moveTo Next nonNSP)
        -- , ("M-<Up>", moveTo Prev nonNSP)
        -- , ("M-<Left>", moveTo Prev nonNSP)

        -- , ("M-a", spawnSelected' myAppGrid)                          -- app grid
        -- , ("M-<Tab>", goToSelected $ mygridConfig myColorizer)       -- window switcher
        -- , ("M-b", bringSelected $ mygridConfig myColorizer)          -- window switcher

        -- navigation within a workspace

        -- , ("M-h", windows W.focusMaster)
        -- , ("M-m", windows W.focusMaster)
        -- , ("M-j", windows W.focusDown)
        -- , ("M-l", windows W.focusDown)
        -- , ("M-k", windows W.focusUp)
        -- , ("M-S-m", windows W.swapMaster)
        -- , ("M-S-j", windows W.swapDown)
        -- , ("M-S-k", windows W.swapUp)
        -- , ("M-<Backspace>", promote)
        -- , ("M-S-<Tab>", rotSlavesDown)
        -- , ("M-C-<Tab>", rotAllDown)

        -- manipulate window arrangement

        -- , ("M-f", sendMessage (T.Toggle "floats"))           -- floating layout
        , ("M-t", withFocused $ windows . W.sink)               -- return window to the stack
        , ("M-S-t", sinkAll)                                    -- return all windows to stack

        , ("M-<Tab>", sendMessage NextLayout)
        -- , ("M-C-M1-<Up>", sendMessage Arrange)
        -- , ("M-C-M1-<Down>", sendMessage DeArrange)

        , ("M-d", decWindowSpacing 2 >> decScreenSpacing 2)     -- increase gap size
        , ("M-i", incWindowSpacing 2 >> incScreenSpacing 2)     -- decrease gap size
        , ("M-n", sendMessage $ MT.Toggle NOBORDERS)            -- toggle window borders
        , ("M-<Space>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- go fullscreen

        -- , ("M-S-i", sendMessage (IncMasterN 1))         -- increase # of windows in master pane
        -- , ("M-S-d", sendMessage (IncMasterN (-1)))      -- decrease # of windows in master pane
        -- , ("M-C-<Up>", increaseLimit)                -- Increase # of windows on workspace
        -- , ("M-C-<Down>", decreaseLimit)              -- Decrease # of windows on workspace

        -- create and manipulate sub layouts

        , ("M-S-h", sendMessage $ pullGroup L)
        , ("M-S-l", sendMessage $ pullGroup R)
        , ("M-S-k", sendMessage $ pullGroup U)
        , ("M-S-j", sendMessage $ pullGroup D)
        , ("M-S-m", withFocused (sendMessage . MergeAll))
        -- , ("M-C-u", withFocused (sendMessage . UnMerge))
        , ("M-S-u", withFocused (sendMessage . UnMergeAll))
        , ("M-.", onGroup W.focusUp')    -- Switch focus to next tab
        , ("M-,", onGroup W.focusDown')  -- Switch focus to prev tab

        -- Window resizing
        , ("M-M1-h", sendMessage Shrink)                   -- Shrink horiz window width
        , ("M-M1-l", sendMessage Expand)                   -- Expand horiz window width
        , ("M-M1-j", sendMessage MirrorShrink)          -- Shrink vert window width
        , ("M-M1-k", sendMessage MirrorExpand)          -- Exoand vert window width
        -- Scratchpads
        -- , ("M-r", namedScratchpadAction myScratchPads "terminal")

        -- emacs bindings
        , ("M-e", spawn (myEmacs))
        , ("M1-e b", spawn "emacs --daemon")
        , ("M1-e k", spawn "killall emacs")
        , ("M1-e i", spawn (myEmacs ++ ("--eval '(ibuffer)'")))
        , ("M1-e d", spawn (myEmacs ++ ("--eval '(dired nil)'")))
        , ("M1-e s", spawn (myEmacs ++ ("--eval '(eshell)'")))

    -- Multimedia Keys
        , ("<XF86AudioMute>",        spawn "amixer set Master toggle"     )
        , ("<XF86AudioLowerVolume>", spawn "amixer set Master 10%- unmute")
        , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 10%+ unmute")
        , ("<XF86Eject>",            spawn "dm_screenshot.sh")
        ]
    -- The following lines are needed for named scratchpads.
          -- where nonNSP          = WSIs (return (\ws -> W.tag ws /= "NSP"))
                -- nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "NSP"))

blue, lowWhite, purple, red, white, yellow, green :: String -> String
purple   = xmobarColor "#b48ead" ""
green    = xmobarColor "#a3be8c" ""
blue     = xmobarColor "#51afef" ""
white    = xmobarColor "#d8dee9" ""
yellow   = xmobarColor "#ebcb8b" ""
red      = xmobarColor "#bf616a" ""
lowWhite = xmobarColor "#c8c9c1" ""

main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar /home/alex/.xmobarrc"

    xmonad $ ewmh $ navigation2D def
             (xK_k, xK_h, xK_j, xK_l)
             [(mod4Mask,               windowGo  ),
              (mod4Mask .|. controlMask, windowSwap)]
             False 
         $ def
        { manageHook = ( isFullscreen --> doFullFloat ) <+> myManageHook <+> manageDocks
        , handleEventHook    = serverModeEventHookCmd
                               <+> serverModeEventHook
                               <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
                               <+> docksEventHook
                               <+> fullscreenEventHook
        , modMask            = myModMask
        , terminal           = myTerminal
        , startupHook        = myStartupHook
        -- , layoutHook         = showWName' myShowWNameTheme $ myLayoutHook
        , layoutHook         = myLayoutHook
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormColour
        , focusedBorderColor = myFocusColour
        , logHook = dynamicLogWithPP $ namedScratchpadFilterOutWorkspacePP $ xmobarPP
                        { ppOutput          = hPutStrLn xmproc
                        , ppSep             = white " | "
                        , ppTitleSanitize   = xmobarStrip
                        , ppCurrent         = blue . wrap (blue "[") (blue "]")
                        , ppHidden          = green . wrap " " " "
                        , ppHiddenNoWindows = lowWhite . wrap " " " "
                        , ppUrgent          = red . wrap (yellow "!") (yellow "!")
                        , ppOrder           = \[ws, l, _] -> [ws, l]
                        -- , ppExtras          = [formatFocused formatUnfocused]
                        }
        } `additionalKeysP` myKeys
