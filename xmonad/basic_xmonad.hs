import XMonad
import XMonad.Config.Desktop
import Data.Monoid
import XMonad.Core
import System.IO (hPutStrLn)
import System.Exit
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import XMonad.Util.EZConfig
import XMonad.Layout.Spacing

import XMonad.Hooks.SetWMName
import Graphics.X11.ExtraTypes.XF86

import XMonad.Layout.ShowWName
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.WorkspaceHistory
-- import XMonad.Actions.Volume

myTerminal      = "alacritty"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth   = 1

myModMask       = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
-- 
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces = ["gen", "www", "ps1", "dev", "sys", "doc"]
-- myWorkspaces = ["1", "2", "3", "4", "5", "6", "7"]
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..] -- (,) == \x y -> (x,y)
--myWorkspaces    = ["WWW","PS1","VIM","EMC","DOC","{ }",">>>","PCM","SYS"]

myNormalBorderColor  = "#007777"
myFocusedBorderColor = "#487aff"

myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
    { swn_font              = "xft:Ubuntu:bold:size=40"
    , swn_fade              = 0.5
    , swn_bgcolor           = "#1c1f24"
    , swn_color             = "#ffffff"
    }
------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ 
      ((modm,               xK_Return), spawn $ XMonad.terminal conf),
      ((modm,               xK_p     ), spawn "rofi -show run"),
      ((modm,               xK_s     ), spawn "firefox"),
      ((modm,               xK_f     ), spawn "pcmanfm"),
      --((modm,               xK_e     ), spawn "emacsclient --no-wait --create-frame"), this is for use with emacs in a client server manner
      ((modm,               xK_e     ), spawn "emacs"),
      ((modm,               xK_g     ), spawn "screenshot"),
      ((modm .|. shiftMask, xK_g     ), spawn "screenshot save"),
 
     -- close focused window,
      ((modm,               xK_c     ), kill),
 
     -- Manipulate layouts
      ((modm,               xK_space ), sendMessage NextLayout),
      ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf), -- Reset to default
 
     -- Resize viewed windows to the correct size,
      ((modm,               xK_n     ), refresh),
 
     -- Move focus between windows
      ((modm,               xK_Tab   ), windows W.focusDown),
      ((modm,               xK_j     ), windows W.focusDown),
      ((modm,               xK_k     ), windows W.focusUp  ),
      ((modm,               xK_m     ), windows W.focusMaster  ),
 
     -- Swap position of focused windows
      ((modm .|. shiftMask,               xK_j     ), windows W.swapDown  ),
      ((modm .|. shiftMask,               xK_k     ), windows W.swapUp    ),
 
     -- Shrink the master area,
      ((modm,               xK_h     ), sendMessage Shrink),
 
     -- Expand the master area,
      ((modm,               xK_l     ), sendMessage Expand),
 
     -- Push window back into tiling,
      ((modm,               xK_t     ), withFocused $ windows . W.sink),
 
     -- Change the number of windows in the master area
      ((modm              , xK_comma ), sendMessage (IncMasterN 1)),
      ((modm              , xK_period), sendMessage (IncMasterN (-1))),

--      ((modm .|. controlMask, xK_h), sendMessage $ pullGroup L),
--      ((modm .|. controlMask, xK_l), sendMessage $ pullGroup R),
--      ((modm .|. controlMask, xK_k), sendMessage $ pullGroup U),
--      ((modm .|. controlMask, xK_j), sendMessage $ pullGroup D),
--
--      ((modm .|. controlMask, xK_m), withFocused (sendMessage . MergeAll)),
--      ((modm .|. controlMask, xK_u), withFocused (sendMessage . UnMerge)),
--
--      ((modm .|. controlMask, xK_period), onGroup W.focusUp'),
--      ((modm .|. controlMask, xK_comma), onGroup W.focusDown'),
      -- Exit and restart hotkeys.
      ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess)),
      ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart"),
    
     -- Audio Volume

      ((0, xF86XK_AudioMute), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle"),
      ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%"),
      ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%")


    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]



------------------------------------------------------------------------
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
--myLayout = windowNavigation $ subTabbed $
--                      Tall 1 (3/100) (1/2) 

myLayout = avoidStruts (tiled ||| Full)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
--xmobar setup

--myBar = "xmobar"

--myPP = xmobarPP { ppCurrent = xmobarColor "#50fa7b" "" . wrap "[" "]" }

--toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)




------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Nitrogen"       --> doFloat  
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

------------------------------------------------------------------------
myEventHook = mempty

------------------------------------------------------------------------

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
    where fadeAmount = 1.0
------------------------------------------------------------------------
myStartupHook = do

    --spawnOnce "hsetroot -solid \"#333333\""
    spawnOnce "nitrogen --restore"
    spawnOnce "xsetroot -cursor_name left_ptr &"
    spawnOnce "xrandr_script"
    spawnOnce "picom &"
    --spawnOnce "emacs --daemon"
    setWMName "LG3D"
------------------------------------------------------------------------
main = do

    xmproc <- spawnPipe "xmobar /home/alex/.xmonad/.xmobarrc"
    -- xmonad $ docks myConfig
--main = xmonad myConfig
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
    xmonad $ docks def
        {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        -- layoutHook = spacingRaw False (Border 0 10 0 10) True (Border 10 0 10 0) True $ myLayout,
        layoutHook         = spacingRaw False (Border 0 5 0 5) True (Border 5 0 5 0) True $ showWName' myShowWNameTheme $ myLayout,
        --layoutHook = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        --logHook            = myLogHook,
        logHook = workspaceHistoryHook <+> myLogHook <+> dynamicLogWithPP xmobarPP
                        { ppOutput = \x -> hPutStrLn xmproc x
                       -- { ppOutput = hPutStrLn h
                        , ppCurrent = xmobarColor "#98be65" "" . wrap "" ""           -- Current workspace in xmobar
                        , ppVisible = xmobarColor "#98be65" ""             -- Visible but not current workspace
                        , ppHidden = xmobarColor "#82AAFF" "" . wrap "" "" -- Hidden workspaces in xmobar
                        , ppHiddenNoWindows = xmobarColor "#c792ea" ""    -- Hidden workspaces (no windows)
                        , ppTitle = xmobarColor "#b3afc2" "" . shorten 0               -- Title of active window in xmobar
                        , ppSep =  " | "                    -- Separators in xmobar
                        , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"            -- Urgent workspace
                        --, ppExtras  = [windowCount]                                     -- # of windows current workspace
                        , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                        },
        startupHook        = myStartupHook
    }
