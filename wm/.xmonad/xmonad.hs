-- core
import XMonad
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab

--Hooks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog

-- layouts
import qualified XMonad.Layout.Magnifier as Mag
import XMonad.Layout.ThreeColumns

-- variables

myBorderWidth :: Dimension
myBorderWidth = 2

myNormColour :: String
myNormColour   = "#353C4A"

myFocusColour :: String
myFocusColour  = "#81A1C1"

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

        ]

myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = purple " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = blue . wrap (blue "[") (blue "]")
    , ppHidden          = green . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _] -> [ws, l]
    -- , ppExtras          = [formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . purple . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, purple, red, white, yellow, green :: String -> String
    purple   = xmobarColor "#b48ead" ""
    green    = xmobarColor "#a3be8c" ""
    blue     = xmobarColor "#81a1c1" ""
    white    = xmobarColor "#d8dee9" ""
    yellow   = xmobarColor "#ebcb8b" ""
    red      = xmobarColor "#bf616a" ""
    lowWhite = xmobarColor "#c8c9c1" ""

myConfig = def
    { 
        modMask = mod4Mask,  -- Rebind Mod to the Super key
        layoutHook = myLayouts,
        borderWidth        = myBorderWidth,
        normalBorderColor  = myNormColour,
        focusedBorderColor = myFocusColour
    }
    `additionalKeysP` myKeys

main :: IO ()
main = xmonad . ewmh =<< statusBar "xmobar" myXmobarPP toggleStrutsKey myConfig
    where
        toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
        toggleStrutsKey XConfig{ modMask = m } = (m, xK_b)
