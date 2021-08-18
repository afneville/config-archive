-- deprecated code / code snippets from old xmonad configs

myXmobarPP :: PP
myXmobarPP = def
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
        layoutHook = avoidStruts $ myLayouts,
        manageHook = myManageHook <+> manageDocks,

        borderWidth        = myBorderWidth,
        normalBorderColor  = myNormColour,
        focusedBorderColor = myFocusColour,
        logHook = dynamicLogWithPP xmobarPP
    }
    `additionalKeysP` myKeys

 
main :: IO ()
main = xmonad . ewmh =<< statusBar "xmobar" myXmobarPP toggleStrutsKey  myConfig
    where
        toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
        toggleStrutsKey XConfig{ modMask = m } = (m, xK_b)

-- main :: IO ()
-- main = do
-- 
--     xmproc <- spawnPipe "xmobar"
-- 
--     xmonad 
--         -- $ withNavigation2DConfig def
--         -- $ navigation2D def
--         --      (xK_k, xK_h, xK_j, xK_l)
--         --      [(mod4Mask,               windowGo  ),
--         --       (mod4Mask .|. shiftMask, windowSwap)]
--         --      False
--         -- $ ewmh $ myConfig
--         $ myConfig
--  
