-- core
import XMonad
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Util.Run 

--Hooks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

-- layouts
import qualified XMonad.Layout.Magnifier as Mag
import XMonad.Layout.ThreeColumns

-- actions
import XMonad.Actions.Navigation2D

-- variables

myBorderWidth :: Dimension
myBorderWidth = 2

myNormColour :: String
myNormColour   = "#353C4A"

myFocusColour :: String
myFocusColour  = "#81A1C1"

-- myManageHook = composeAll
     -- [ 
     --   className =? "confirm"         --> doFloat
     -- , className =? "file_progress"   --> doFloat
     -- , className =? "dialog"          --> doFloat
     -- , className =? "download"        --> doFloat
     -- , className =? "notification"    --> doFloat
     --   Browsers go to workspace 3 (www), except qutebrowser which can be summoned on any workspace.
     -- , className =? "Brave-browser" --> doShift ( myWorkspaces !! 2 )
     -- , className =? "firefox" --> doShift ( myWorkspaces !! 2 )
     -- , className =? "discord" --> doShift ( myWorkspaces !! 3 )
     -- , className =? "Nitrogen" --> doFloat
     -- , className =? "Nitrogen" --> doShift ( myWorkspaces !! 5 )
     -- , className =? "Lxappearance" --> doShift ( myWorkspaces !! 5 )
     -- , className =? "libreoffice-startcenter" --> doShift ( myWorkspaces !! 4 )
     -- , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
     -- ]

myLayouts = tiled ||| Mirror tiled ||| Full ||| Mag.magnifierOff (threeCol)
    where 
        tiled = Tall nmaster delta ratio
        threeCol = ThreeColMid nmaster delta ratio
        nmaster = 1
        delta = 3/100
        ratio = 1/2

myKeys :: [(String, X ())]
myKeys =
        [
        -- xmonad
          -- ("M-S-q", io exitSuccess)
          ("M-c", kill)     -- Kill the currently focused client
        -- , ("M-S-c", killAll)   -- Kill all windows on current workspace

        -- applications
        , ("M-<Return>", spawn "alacritty")
        , ("M-p", spawn "dmenu_run -i -p \"Launch:\"")

        -- Magnifier
        , ("M-=", sendMessage Mag.MagnifyMore)
        , ("M--", sendMessage Mag.MagnifyLess)
        , ("M-0", sendMessage Mag.Toggle)

--         -- Directional navigation of windows
--         , ("M-l", windowGo R False)
--         , ("M-h", windowGo L False)
--         , ("M-k", windowGo U False)
--         , ("M-j", windowGo D False)
-- 
--         -- Swap adjacent windows
--         , ("M-l", windowSwap R False)
--         , ("M-h", windowSwap L False)
--         , ("M-k", windowSwap U False)
--         , ("M-j", windowSwap D False)
        ]



blue, lowWhite, purple, red, white, yellow, green :: String -> String
purple   = xmobarColor "#b48ead" ""
green    = xmobarColor "#a3be8c" ""
blue     = xmobarColor "#81a1c1" ""
white    = xmobarColor "#d8dee9" ""
yellow   = xmobarColor "#ebcb8b" ""
red      = xmobarColor "#bf616a" ""
lowWhite = xmobarColor "#c8c9c1" ""

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ navigation2D def
             (xK_k, xK_h, xK_j, xK_l)
             [(mod4Mask,               windowGo  ),
              (mod4Mask .|. shiftMask, windowSwap)]
             False 
         $ def
    { manageHook = manageDocks <+> manageHook defaultConfig
    , handleEventHook = handleEventHook defaultConfig <+> docksEventHook
    , layoutHook = avoidStruts $ myLayouts
    , logHook = dynamicLogWithPP xmobarPP
                    -- { ppOutput = hPutStrLn xmproc
                    -- , ppTitle = xmobarColor "green" "" . shorten 50
                    -- }
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
    , terminal = "alacritty"
    , borderWidth        = myBorderWidth
    , normalBorderColor  = myNormColour
    , focusedBorderColor = myFocusColour
    , modMask = mod4Mask

    } `additionalKeysP` myKeys
