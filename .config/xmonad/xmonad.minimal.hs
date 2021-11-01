-- xmonad.minimal.hs

import Data.List ( (\\) )
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Layout.IndependentScreens
import XMonad.Util.Run (spawnPipe)
import qualified Data.Map as M

ansi :: M.Map String String
ansi = M.fromList
  [
    ("cyan",             "#00ffff"),
    ("gray20",           "#333333"),
    ("gray33",           "#555555"),
    ("gray87",           "#dddddd"),
    ("gray99",           "#999999"),
    ("green",            "#00ff00"),
    ("red",              "#ff0000"),
    ("white",            "#ffffff"),
    ("yellow",           "#ffff00")
  ]

myBlue :: String
myBlue   = "#0080FF"

myRed :: String
myRed    = "#FF4A36"

xmobarCommand1 :: String
xmobarCommand1 = "xmobar $HOME/.config/xmonad/xmobar.hs"

-- Define how xmonad-workspace-status is presented in what color.
myXmobarPP :: PP
myXmobarPP = xmobarPP
  {
  --   ppCurrent = xmobarColor myBlue "" . wrap "[" "]"  -- currently focused workspace
  -- , ppTitle   = xmobarColor myRed "" -- title of currently focused program
  -- ...
    ppCurrent         = xmobarColor (ansi M.! "cyan") "" . wrap "[" "]",
    ppHidden          = xmobarColor (ansi M.! "white") "",
    ppHiddenNoWindows = xmobarColor (ansi M.! "gray99") "",
    ppTitle           = xmobarColor (ansi M.! "cyan") "" . shorten 50,
    ppVisible         = wrap "(" ")",
    ppUrgent          = xmobarColor (ansi M.! "red") (ansi M.! "yellow")
  }

-- Function to create the keyboard shortcut to show and hide the bar.
xmobarShowHideKeymap (XConfig {modMask = modKey}) = (modKey, xK_b)

myConfig = def
  { modMask     = mod1Mask
  , terminal    = "urxvt" -- for Mod + Shift + Enter
  -- ...
  }

main :: IO ()
main = do
  xmonad =<< statusBar xmobarCommand1 myXmobarPP xmobarShowHideKeymap myConfig

-- end
