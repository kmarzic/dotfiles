-------------------------------------------------------------------------------
-- xmonad.hs
-- Last update: 2020-03-21 14:18:36 (CET)
-------------------------------------------------------------------------------

import Data.Maybe ( maybeToList )
import Data.List ( (\\) )
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.Minimize(minimizeWindow, withLastMinimized, maximizeWindowAndFocus)
import XMonad.Actions.UpdatePointer
import XMonad.Config
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicHooks
import XMonad.Hooks.DynamicLog
import XMonad.Layout.Column
import XMonad.Layout.Gaps
import XMonad.Layout.Groups.Helpers
import XMonad.Layout.IndependentScreens
import XMonad.Layout.Minimize(minimize)
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Roledex
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ToggleLayouts
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Util.EZConfig
import XMonad.Util.NamedWindows ( getName )
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Scratchpad
import XMonad.Util.SpawnOnce
import Graphics.X11.ExtraTypes.XF86
import System.Environment (getEnv)
import System.IO
import System.IO.Unsafe
import qualified Data.Map as M
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
import qualified XMonad.Actions.FlexibleResize as Flex
import qualified XMonad.Layout.Groups as G
import qualified XMonad.Layout.Groups.Helpers as Group
import qualified XMonad.StackSet as W


-------------------------------------------------------------------------------
-- Config
-------------------------------------------------------------------------------

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

monokai :: M.Map String String
monokai = M.fromList
  [
    ("blue",             "#0088cc"),
    ("cyan",             "#00ffff"),
    ("white",            "#ffffff")
  ]

nord :: M.Map String String
nord = M.fromList
  [
    ("black0",           "#3B4252"), -- 0
    ("red0",             "#BF616A"), -- 1
    ("green0",           "#A3BE8C"), -- 2
    ("yellow0",          "#EBCB8B"), -- 3
    ("blue0",            "#81A1C1"), -- 4
    ("purple0",          "#B48EAD"), -- 5
    ("cyan0",            "#88C0D0"), -- 6
    ("white0",           "#E5E9F0"), -- 7
    ("black1",           "#4C566A"), -- 8
    ("red1",             "#BF616A"), -- 9
    ("green1",           "#A3BE8C"), -- 10
    ("yellow1",          "#EBCB8B"), -- 11
    ("blue1",            "#81A1C1"), -- 12
    ("purple1",          "#B48EAD"), -- 13
    ("cyan1",            "#8FBCBB"), -- 14
    ("white1",           "#ECEFF4")  -- 15
  ]

solarized :: M.Map String String
solarized = M.fromList
  [
    ("solarizedBase03",  "#002b36"),
    ("solarizedBase02",  "#073642"),
    ("solarizedBase01",  "#586e75"),
    ("solarizedBase00",  "#657b83"),
    ("solarizedBase0",   "#839496"),
    ("solarizedBase1",   "#93a1a1"),
    ("solarizedBase2",   "#eee8d5"),
    ("solarizedBase3",   "#fdf6e3"),
    ("solarizedYellow",  "#b58900"),
    ("solarizedOrange",  "#cb4b16"),
    ("solarizedRed",     "#dc322f"),
    ("solarizedMagenta", "#d33682"),
    ("solarizedViolet",  "#6c71c4"),
    ("solarizedBlue",    "#268bd2"),
    ("solarizedCyan",    "#2aa198"),
    ("solarizedGreen",   "#859900")
  ]

help :: String
help = unlines
  [
    "The default modifier key is 'alt'. Keybindings:",
    "",
    "-- Launching and killing programs",
    "mod-Shift-Enter      Launch xterminal",
    "mod-Enter            Launch xterminal",
    "mod-s                Launch scratchpad",
    "mod-p                Launch dmenu",
    "mod-d                Launch dmenu",
    "mod-Menu             Launch dmenu",
    "mod-Shift-c          Close/kill the focused window",
    "mod-F4               Close/kill the focused window",
    "PrintScreen          Root screenshot",
    "mod-PrintScreen      Window screenshot",
    "mod-Space            Rotate through the available layout algorithms",
    "mod-Shift-Space      Reset the layouts on the current Workspace to default",
    "mod-n                Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab              Move focus to the next window",
    "mod-Shift-Tab        Move focus to the previous window",
    "mod-j                Move focus to the next window in Group",
    "mod-k                Move focus to the previous window in Group",
    "mod-m                Move focus to the master window",
    "mod-l                Move focus to the previous Group",
    "mod-h                Move focus to the next Group",
    "",
    "-- Modifying the window order",
    "mod-Shift-j          Swap the focused window with the next window",
    "mod-Shift-k          Swap the focused window with the previous window",
    "mod-Shift-l          Move the focused window to the previous Group",
    "mod-Shift-h          Move the focused window to the next Group",
    "",
    "-- Resizing the master/slave ratio",
    "mod-f                Toggle full screen",
    "mod-a                Shrink resizable area",
    "mod-z                Expand resizable area",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[0/~,1..9] Move client to Workspace N",
    "mod-{w,e,r}          Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}    Move client to screen 1, 2, or 3",
    "mod-,                Prev Screen",
    "mod-.                Next Screen",
    "mod-Shift-,          Shift to Prev Screen",
    "mod-Shift-.          Shift to Next Screen",
    "mod-[0/~,1..9]       Switch to Workspace N",
    "mod-[                Previous Workspace",
    "mod-]                Next Workspace",
    -- "mod-Shift-Left       Previous Workspace",
    -- "mod-Shift-Right      Next Workspace",
    "mod-Shift-[          Previous NonEmpty Workspace",
    "mod-Shift-]          Next NonEmpty Workspace",
    -- "mod-Control-Left     Previous Workspace",
    -- "mod-Control-Right    Next Workspace",
    "",
    "-- Floating layer support",
    "mod-t                Push window back into tiling; unfloat and re-tile it",
    "",
    "-- Quit, or restart",
    "mod-Shift-q          Quit xmonad",
    "mod-Ctrl-x           Quit xmonad",
    "mod-Ctrl-l           Lock screen",
    "mod-Ctrl-s           Monitor off",
    "mod-Ctrl-m           Xrandr reload",
    "mod-q                Restart xmonad",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1          Set the window to floating mode and move by dragging",
    "mod-button2          Raise the window to the top of the stack",
    "mod-button3          Set the window to floating mode and resize by dragging",
    "mod-button4          Switch to previous Workspace",
    "mod-button5          Switch to next Workspace",
    "mod-Shift-button4    Send client to previous Workspace",
    "mod-Shift-button5    Send client to next Workspace"
  ]

fontRegular :: String
fontRegular = "xft:monospace:pixelsize=14:antialias=true:style=regular"

fontBold :: String
fontBold = "xft:monospace:pixelsize=14:antialias=true:style=bold"
-- fontBold = "xft:monospace:pixelsize=13:antialias=true:style=bold"
-- fontBold = "xft:monospace:pixelsize=12:antialias=true:style=bold"
-- fontBold = "xft:Terminus:pixelsize=14:antialias=true:style=bold"
-- fontBold = "xft:Terminus:pixelsize=13:antialias=true:style=bold"
-- fontBold = "xft:Terminus:pixelsize=12:antialias=true:style=bold"

fontTerminalScratchpad :: String
fontTerminalScratchpad = "xft:monospace:pixelsize=14:antialias=true:style=bold,xft:Source\\ Code\\ Pro\\ Medium:pixelsize=18:antialias=true:hinting=true:style:bold"

dmenuCommandAnsi :: String -- theme: ansi
dmenuCommandAnsi = "/usr/bin/dmenu_run -i -nf \"#00ffff\" -nb \"#101010\" -sb \"#00ffff\" -sf \"#101010\" -fn " ++ fontRegular ++ " -p 'Run: '"

dmenuCommandMonokai :: String -- theme: monokai
dmenuCommandMonokai = "/usr/bin/dmenu_run -i -nf \"#00ffff\" -nb \"#101010\" -sb \"#00ffff\" -sf \"#101010\" -fn " ++ fontRegular ++ " -p 'Run: '"

dmenuCommandNord :: String -- theme: nord
dmenuCommandNord = "/usr/bin/dmenu_run -i -nf \"#ffffff\" -nb \"#222222\" -sb \"#009910\" -sf \"#ffffff\" -fn " ++ fontRegular ++ " -p 'Run: '"

dmenuCommandSolarizedDark :: String -- theme: solarized dark
dmenuCommandSolarizedDark = "/usr/bin/dmenu_run -i -nf \"#2aa198\" -nb \"#002b36\" -sb \"#2aa198\" -fn " ++ fontRegular ++ " -p 'Run: '"

dmenuCommandSolarizedLight :: String -- theme: solarized light
dmenuCommandSolarizedLight = "/usr/bin/dmenu_run -i -nf \"#2aa198\" -nb \"#fdf6e3\" -sb \"#2aa198\" -fn " ++ fontRegular ++ " -p 'Run: '"

xmobarCommand1 :: String
xmobarCommand1 = "xmobar $HOME/.xmonad/xmobar.hs"

xmobarCommand2 :: ScreenId -> String
xmobarCommand2 (S s) = unwords ["xmobar", "-x", show s, "$HOME/.xmonad/xmobar.hs"]

i3statusCommand1 :: String
i3statusCommand1 = "i3status -c $HOME/.i3status.conf | xmobar -b -t \"%UnsafeStdinReader%\" -c \"[ Run UnsafeStdinReader ]\""

lemonbarCommand1 :: String
lemonbarCommand1 = "lemonbar -d -b -a 32 -u 2 -g x24 -f " ++ fontRegular ++ " -F \"#2199ee\" -B \"#000000\" -U \"#00ff00\" | bash "

myTerminal :: String
myTerminal = "urxvt"
-- myTerminal = "urxvtc"
-- myTerminal = "termite"
-- myTerminal = "$HOME/bin/st"

myTerminalScratchpad :: String
myTerminalScratchpad = "urxvt -fn " ++ fontTerminalScratchpad
-- myTerminalScratchpad = "kitty &"
-- myTerminalScratchpad = "termite --class=termscratch"
-- myTerminalScratchpad = "tilda -f " ++ fontTerminalScratchpad
-- myTerminalScratchpad = "$HOME/bin/st -c scratchpad -n scratchpad"

myModMask :: KeyMask
myModMask = mod1Mask

myFocusFollowsMouse :: Bool
-- myFocusFollowsMouse = True
myFocusFollowsMouse = False

myBorderWidth :: Dimension
myBorderWidth = 1
-- myBorderWidth = 2

myNormalBorderColorAnsi :: String -- theme: ansi
myNormalBorderColorAnsi = ansi M.! "white"
myFocusedBorderColorAnsi :: String
myFocusedBorderColorAnsi = ansi M.! "cyan"

myNormalBorderColorMonokai :: String -- theme: monokai
myNormalBorderColorMonokai = monokai M.! "white"
myFocusedBorderColorMonokai :: String
myFocusedBorderColorMonokai = monokai M.! "cyan"

myNormalBorderColorNord :: String -- theme: nord
myNormalBorderColorNord = nord M.! "black1"
myFocusedBorderColorNord :: String
myFocusedBorderColorNord = nord M.! "red0"

myNormalBorderColorSolarizedDark :: String -- theme: solarized dark
myNormalBorderColorSolarizedDark = solarized M.! "solarizedBase02"
myFocusedBorderColorSolarizedDark :: String
myFocusedBorderColorSolarizedDark = solarized M.! "solarizedCyan"

myNormalBorderColorSolarizedLight :: String -- theme: solarized light
myNormalBorderColorSolarizedLight = solarized M.! "solarizedBase3"
myFocusedBorderColorSolarizedLight :: String
myFocusedBorderColorSolarizedLight = solarized M.! "solarizedBlue"

xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
  where doubleLts '<' = "<<"
        doubleLts x   = [x]

myWorkspaces1 :: [String]
myWorkspaces1 = ["1","2","3","4","5","6","7","8","9","0"]

myWorkspaces :: [String]
myWorkspaces = clickable . (map xmobarEscape) $ ["1","2","3","4","5","6","7","8","9","0"]
  where
    clickable l = [ "<action=`xdotool key alt+" ++ show (n) ++ "`>" ++ ws ++ "</action>" | (i,ws) <- zip ([1..9] ++ [0]) l, let n = i ] -- 10 workspaces


-------------------------------------------------------------------------------
-- Startup
-------------------------------------------------------------------------------

myStartUpScreen :: X()
myStartUpScreen = do
  nScreens <- countScreens
  if nScreens == 1
  then do
    screenWorkspace 0 >>= flip whenJust (windows . W.view)
    windows $ W.greedyView "1"
  else if nScreens == 2
  then do
    screenWorkspace 0 >>= flip whenJust (windows . W.view)
    windows $ W.greedyView "1"
    screenWorkspace 1 >>= flip whenJust (windows . W.view)
    windows $ W.greedyView "9"
  else if nScreens == 3
  then do
    screenWorkspace 0 >>= flip whenJust (windows . W.view)
    windows $ W.greedyView "1"
    screenWorkspace 1 >>= flip whenJust (windows . W.view)
    windows $ W.greedyView "0"
    screenWorkspace 2 >>= flip whenJust (windows . W.view)
    windows $ W.greedyView "9"
  else
    return ()

myStartUp :: X()
myStartUp = do
  -- spawnOnce "feh --bg-scale ~/wallpapers/green/lines_spots_color_texture_50390_3840x2400.jpg"
  -- spawnOnce "setxkbmap -model pc105 -option 'eurosign:e,lv3:ralt_switch,compose:nocaps' 'hr(us)'"
  -- spawnOnce "dunst -config $HOME/.config/dunst/dunstrc"
  spawn "$HOME/.xmonad/screen_toggle.sh -x"
  spawn "$HOME/.xmonad/trayer.sh"


-------------------------------------------------------------------------------
-- Window Rules
-------------------------------------------------------------------------------

myManageScratchPad :: ManageHook
myManageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
  where
    h = 0.6 -- terminal height, 60%
    w = 0.6 -- terminal width, 60%
    t = 0.3 -- distance from top edge, 30%
    l = 0.1 -- distance from left edge, 10%

myManageHook :: ManageHook
myManageHook = composeAll . concat $
  [
    [className =? "Chromium" --> doShift (myWorkspaces !! 4)],
    [className =? "Chromium-browser" --> doShift (myWorkspaces !! 4)],
    [className =? "Chrome" --> doShift (myWorkspaces !! 4)],
    [className =? "Opera" --> doShift (myWorkspaces !! 4)],
    [className =? "Firefox" --> doShift (myWorkspaces !! 4)],
    [className =? "Firefox-esr" --> doShift (myWorkspaces !! 4)],
    [className =? "Mozilla Firefox" --> doShift (myWorkspaces !! 4)],
    [className =? "New Tab - Mozilla Firefox" --> doShift (myWorkspaces !! 4)],
    [className =? "Vivaldi" --> doShift (myWorkspaces !! 4)],
    [className =? "Vivaldi-stable" --> doShift (myWorkspaces !! 4)],
    [className =? "Pidgin" --> doShift (myWorkspaces !! 6)],
    [className =? "Skype" --> doShift (myWorkspaces !! 6)],
    [className =? "VirtualBox Manager" --> doShift (myWorkspaces !! 7)],
    [className =? "Evolution" --> doShift (myWorkspaces !! 8)],
    [className =? "Mozilla Thunderbird" --> doShift (myWorkspaces !! 8)],
    --
    [role      =? "GtkFileChooserDialog" --> doFullFloat],
    --
    [className =? c --> doFloat  | c <- myClassFloats],
    [title     =? t --> doFloat  | t <- myTitleFloats],
    [resource  =? r --> doFloat  | r <- myResourceFloats],
    [resource  =? i --> doIgnore | i <- myIgnores],
    --
    [isDialog       --> doFloat],
    [isFullscreen   --> (doF W.focusDown <+> doFullFloat)]
  ]
  where
    role          = stringProperty "WM_WINDOW_ROLE"
    netName       = stringProperty "_NET_WM_NAME"
    name          = stringProperty "WM_NAME"
    myClassFloats =
      [
        "Gimp", "MPlayer", "Nvidia-settings", "Sysinfo", "vlc", "Vncviewer",
        "XCalc", "XFontSel", "Xmessage"
      ]
    myTitleFloats =
      [
        "Autofill Options", "Choose a file", "Clear Private Data", "Copying files", "Downloads",
        "File Operation Progress", "File Properties", "File Transfers", "Moving files",
        "Passwords and Exceptions", "Preferences", "Rename File", "Replace", "Save As...", "Search Engines",
        "Deleting", "Exit",
        "Firefox Preferences", "Iceweasel Preferences", "Thunderbird Preferences"
      ]
    myResourceFloats =
      [
        "buddy_list", "ticker", "gimp-toolbox", "gimp-dock", "gimp-image-window",  "xeyes"
      ]
    myIgnores =
      [
        "cairo-compmgr", "desktop", "desktop_window", "kdesktop", "trayer"
      ]


-------------------------------------------------------------------------------
-- Layouts
-------------------------------------------------------------------------------

myTabConfigAnsi :: Theme -- theme: ansi
myTabConfigAnsi = def
  {
    activeColor = ansi M.! "gray33",
    activeTextColor = ansi M.! "white",
    activeBorderColor = ansi M.! "cyan",
    inactiveColor = ansi M.! "gray20",
    inactiveTextColor = ansi M.! "gray87",
    inactiveBorderColor = ansi M.! "white",
    urgentColor = ansi M.! "red",
    urgentTextColor = ansi M.! "white",
    urgentBorderColor = ansi M.! "gray20",
    fontName = fontBold
  }

myTabConfigMonokai :: Theme -- theme: monokai
myTabConfigMonokai = def
  {
    activeColor = "#555555",
    activeTextColor = "#ffffff",
    activeBorderColor = "#00ffff",
    inactiveColor = "#333333",
    inactiveTextColor = "#dddddd",
    inactiveBorderColor = "#ffffff",
    urgentColor = "#ff0000",
    urgentTextColor = "#ffffff",
    urgentBorderColor = "#2f343a",
    fontName = fontBold
  }

myTabConfigNord :: Theme -- theme: nord
myTabConfigNord = def
  {
    activeColor = nord M.! "black1",
    activeTextColor = nord M.! "white1",
    activeBorderColor = nord M.! "red1",
    inactiveColor = nord M.! "black0",
    inactiveTextColor = nord M.! "white0",
    inactiveBorderColor = nord M.! "red0",
    urgentColor = nord M.! "red1",
    urgentTextColor = nord M.! "white1",
    urgentBorderColor = nord M.! "yellow1",
    fontName = fontBold
  }

myTabConfigSolarized :: Theme -- theme: solarized [ light and dark ]
myTabConfigSolarized = def
  {
    activeColor = solarized M.! "solarizedBase00",
    activeTextColor = solarized M.! "solarizedBase2",
    activeBorderColor = solarized M.! "solarizedBase1",
    inactiveColor = solarized M.! "solarizedBase03",
    inactiveTextColor = solarized M.! "solarizedBase1",
    inactiveBorderColor = solarized M.! "solarizedBase1",
    urgentColor = solarized M.! "solarizedRed",
    urgentTextColor = solarized M.! "solarizedBase3",
    urgentBorderColor = solarized M.! "solarizedBase03",
    fontName = fontBold
  }

myTabConfigSolarizedDark :: Theme -- theme: solarized dark
myTabConfigSolarizedDark = def
  {
    activeColor = solarized M.! "solarizedBase00",
    activeTextColor = solarized M.! "solarizedBase2",
    activeBorderColor = solarized M.! "solarizedBase1",
    inactiveColor = solarized M.! "solarizedBase03",
    inactiveTextColor = solarized M.! "solarizedBase1",
    inactiveBorderColor = solarized M.! "solarizedBase1",
    urgentColor = solarized M.! "solarizedRed",
    urgentTextColor = solarized M.! "solarizedBase3",
    urgentBorderColor = solarized M.! "solarizedBase03",
    fontName = fontBold
  }

myTabConfigSolarizedLight :: Theme -- theme: solarized light
myTabConfigSolarizedLight = def
  {
    activeColor = solarized M.! "solarizedGreen",
    activeTextColor = solarized M.! "solarizedBase3",
    activeBorderColor = solarized M.! "solarizedBase03",
    inactiveColor = solarized M.! "solarizedBase1",
    inactiveTextColor = solarized M.! "solarizedBase3",
    inactiveBorderColor = solarized M.! "solarizedBase03",
    urgentColor = solarized M.! "solarizedRed",
    urgentTextColor = solarized M.! "solarizedBase3",
    urgentBorderColor = solarized M.! "solarizedBase03",
    fontName = fontBold
  }

myLayoutHook tabConfig =
  gaps0
  -- gaps1
  -- $ smartSpacing 1
  -- $ spacing 1
  $ spacingRaw True (Border 0 0 0 0) True (Border 1 1 1 1) True
  -- $ smartBorders
  $ avoidStruts
  -- $ toggleLayouts (noBorders $ full')
  -- $ toggleLayouts (noBorders $ tab2')
  -- $ (flip G.group) (Full ||| Mirror (Column 1.41) ||| Mirror (Column 1))
  $ (flip G.group) (Full)
  -- $ myLayouts
  -- $ tab2' ||| full' ||| tiled' ||| mirror' ||| threecol' ||| resizetab' ||| roledex'
  -- $ tab2' ||| full' ||| tiled' ||| mirror' ||| roledex'
  $ full' ||| tab2' ||| tiled' ||| mirror' ||| roledex'
  where
    -- myLayouts  = tab2' ||| tiled' ||| mirror' ||| threecol' ||| full' ||| resizetab' ||| roledex'
    -- myLayouts  = tab2' ||| full' ||| tiled' ||| mirror' ||| roledex'
    -- myLayouts  = full' ||| tab2' ||| tiled' ||| mirror' ||| roledex'
    --
    tab2'      = tabbedAlways shrinkText tabConfig
    -- tab2'      = spacingRaw True (Border 0 1 1 1) True (Border 1 1 1 1) True $ tabbedAlways shrinkText tabConfig
    --
    tiled'     = Tall nmaster0 delta0 ratio0
    -- tiled'     = spacingRaw True (Border 0 1 1 1) True (Border 1 1 1 1) True $ Tall nmaster0 delta0 ratio0
    --
    mirror'    = Mirror tiled'
    --
    threecol'  = ThreeColMid nmaster0 delta0 ratio0
    --
    -- full'      = gaps0 $ Full
    -- full'      = gaps1 $ Full
    full'      = Full
    --
    resizetab' = ResizableTall 1 (3/100) (1/2) []
    --
    roledex'   = Roledex
    --
    -- The default number of windows in the master pane
    nmaster0   = 1
    --
    -- Default proportion of screen occupied by master pane
    ratio0     = 1/2
    --
    -- Percent of screen to increment by when resizing panes
    delta0     = 2/100
    --
    -- Gaps
    gaps0      = gaps [(U,0), (D,0), (L,0), (R,0)]
    gaps1      = gaps [(U,2), (D,2), (L,2), (R,2)]


-------------------------------------------------------------------------------
-- Status bars and logging
-------------------------------------------------------------------------------

myIcon :: String -> String
myIcon name = abc
  where
    myHome = unsafePerformIO $ getEnv "HOME"
    abc = "<icon=" ++ myHome ++ "/" ++ name ++ "/>"

myPPLayout :: String -> String
myPPLayout layout = case layout of
      -- (1) ascii layout
      -- "Tabbed Simplest by Full" -> "[_]"
      -- "Full by Full"            -> "[ ]"
      -- "Tall by Full"            -> "[|]"
      -- "Mirror Tall by Full"     -> "[-]"
      -- "Roledex by Full"         -> "[@]"
      -- _                         -> layout
      --
      -- (2) icon layout
      "Spacing Tabbed Simplest by Full" -> myIcon ".xmonad/icons/layout_tabbed.xbm"
      "Tabbed Simplest by Full"         -> myIcon ".xmonad/icons/layout_tabbed.xbm"
      "Spacing Full by Full"            -> myIcon ".xmonad/icons/layout_full.xbm"
      "Full by Full"                    -> myIcon ".xmonad/icons/layout_full.xbm"
      "Spacing Tall by Full"            -> myIcon ".xmonad/icons/layout_tall.xbm"
      "Tall by Full"                    -> myIcon ".xmonad/icons/layout_tall.xbm"
      "Spacing Mirror Tall by Full"     -> myIcon ".xmonad/icons/layout_mirror.xbm"
      "Mirror Tall by Full"             -> myIcon ".xmonad/icons/layout_mirror.xbm"
      "Spacing Roledex by Full"         -> myIcon ".xmonad/icons/layout_roledex.xbm"
      "Roledex by Full"                 -> myIcon ".xmonad/icons/layout_roledex.xbm"
      _                                 -> layout

logTitles :: X (Maybe String) -- this is a Logger
logTitles =
   withWindowSet $ fmap (Just . unwords) -- fuse window names
   . traverse (fmap show . getName) -- show window names
   . (\ws -> W.index ws \\ maybeToList (W.peek ws)) -- all windows except the focused (may be slow)

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myLogHookAnsiPP :: PP -- theme: ansi
myLogHookAnsiPP = def
  {
    ppCurrent         = xmobarColor (ansi M.! "cyan") "" . wrap "[" "]",
    ppHidden          = xmobarColor (ansi M.! "white") "",
    ppHiddenNoWindows = xmobarColor (ansi M.! "gray99") "",
    ppTitle           = xmobarColor (ansi M.! "cyan") "" . shorten 50,
    ppVisible         = wrap "(" ")",
    ppUrgent          = xmobarColor (ansi M.! "red") (ansi M.! "yellow"),
    ppLayout          = xmobarColor (ansi M.! "green") "" . (\layout -> myPPLayout (layout)),
    ppSep             = " ", -- separator between each object
    ppWsSep           = " ", -- separator between workspaces
    -- (1)
    -- ppExtras          = [ logTitles ],
    -- ppOrder           = \(ws:l:t:ts:_) -> ws : l : t : [xmobarColor "gray" "" ts]
    -- (2)
    ppExtras          = [ windowCount ],
    ppOrder           = \(ws:l:t:ts:ex) -> [ws,l,"[",xmobarColor (ansi M.! "red") "" ts,"]",t] ++ ex ++ []
  }

myLogHookMonokaiPP :: PP -- theme: monokai
myLogHookMonokaiPP = def
  {
    ppCurrent         = xmobarColor "cyan" "" . wrap "[" "]",
    ppHidden          = xmobarColor "#ffffff" "",
    ppHiddenNoWindows = xmobarColor "#999999" "",
    ppTitle           = xmobarColor "cyan" "" . shorten 50,
    ppVisible         = wrap "(" ")",
    ppUrgent          = xmobarColor "red" "yellow",
    ppLayout          = xmobarColor "green" "" . (\layout -> myPPLayout (layout)),
    ppSep             = " ", -- separator between each object
    ppWsSep           = " ", -- separator between workspaces
    -- (1)
    -- ppExtras          = [ logTitles ],
    -- ppOrder           = \(ws:l:t:ts:_) -> ws : l : t : [xmobarColor "gray" "" ts]
    -- (2)
    ppExtras          = [ windowCount ],
    ppOrder           = \(ws:l:t:ts:ex) -> [ws,l,"[",xmobarColor "red" "" ts,"]",t] ++ ex ++ []
  }

myLogHookNordPP :: PP -- theme: nord
myLogHookNordPP = def
  {
    ppCurrent         = xmobarColor (nord M.! "cyan0") "" . wrap "[" "]",
    ppHidden          = xmobarColor (nord M.! "yellow1") "",
    ppHiddenNoWindows = xmobarColor (nord M.! "white0") "",
    ppTitle           = xmobarColor (nord M.! "cyan0") "" . shorten 50,
    ppVisible         = wrap "(" ")",
    ppUrgent          = xmobarColor (nord M.! "red0") (nord M.! "yellow0"),
    ppLayout          = xmobarColor (nord M.! "cyan0") "" . (\layout -> myPPLayout (layout)),
    ppSep             = " ", -- separator between each object
    ppWsSep           = " ", -- separator between workspaces
    -- (1)
    -- ppExtras          = [ logTitles ],
    -- ppOrder           = \(ws:l:t:ts:_) -> ws : l : t : [xmobarColor "gray" "" ts]
    -- (2)
    ppExtras          = [ windowCount ],
    ppOrder           = \(ws:l:t:ts:ex) -> [ws,l,"[",xmobarColor (nord M.! "red0") "" ts,"]",t] ++ ex ++ []
  }

myLogHookSolarizedDarkPP :: PP -- theme: solarized dark
myLogHookSolarizedDarkPP = def
  {
    ppCurrent         = xmobarColor (solarized M.! "solarizedRed") "" . wrap "[" "]",
    ppHidden          = xmobarColor (solarized M.! "solarizedBase3") "",
    ppHiddenNoWindows = xmobarColor (solarized M.! "solarizedBase1") "",
    ppTitle           = xmobarColor (solarized M.! "solarizedCyan") "" . shorten 50,
    ppVisible         = wrap "(" ")",
    ppUrgent          = xmobarColor (solarized M.! "solarizedRed") (solarized M.! "solarizedYellow"),
    ppLayout          = xmobarColor (solarized M.! "solarizedCyan") "" . (\layout -> myPPLayout (layout)),
    ppSep             = " ", -- separator between each object
    ppWsSep           = " ", -- separator between workspaces
    -- (1)
    -- ppExtras          = [ logTitles ],
    -- ppOrder           = \(ws:l:t:ts:_) -> ws : l : t : [xmobarColor "gray" "" ts]
    -- (2)
    ppExtras          = [ windowCount ],
    ppOrder           = \(ws:l:t:ts:ex) -> [ws,l,"[",xmobarColor (solarized M.! "solarizedRed") "" ts,"]",t] ++ ex ++ []
  }

myLogHookSolarizedLightPP :: PP -- theme: solarized light
myLogHookSolarizedLightPP = def
  {
    ppCurrent         = xmobarColor (solarized M.! "solarizedBase3") (solarized M.! "solarizedBlue") . wrap "[" "]",
    ppHidden          = xmobarColor (solarized M.! "solarizedBase03") "",
    ppHiddenNoWindows = xmobarColor (solarized M.! "solarizedBase1") "",
    ppTitle           = xmobarColor (solarized M.! "solarizedBlue") "" . shorten 50,
    ppVisible         = wrap "(" ")",
    ppUrgent          = xmobarColor (solarized M.! "solarizedRed") (solarized M.! "solarizedYellow"),
    ppLayout          = xmobarColor (solarized M.! "solarizedBlue") "" . (\layout -> myPPLayout (layout)),
    ppSep             = " ", -- separator between each object
    ppWsSep           = " ", -- separator between workspaces
    -- (1)
    -- ppExtras          = [ logTitles ],
    -- ppOrder           = \(ws:l:t:ts:_) -> ws : l : t : [xmobarColor "gray" "" ts]
    -- (2)
    ppExtras          = [ windowCount ],
    ppOrder           = \(ws:l:t:ts:ex) -> [ws,l,"[",xmobarColor (solarized M.! "solarizedRed") "" ts,"]",t] ++ ex ++ []
  }

-- ansi

myLogHookAnsi1 :: Handle -> X()
myLogHookAnsi1 h = dynamicLogWithPP myLogHookAnsiPP
  {
    ppOutput = hPutStrLn h
  }

myLogHookAnsi2a :: Handle -> ScreenId -> PP
myLogHookAnsi2a h s = myLogHookAnsiPP
  {
    ppOutput = hPutStrLn h
  }

myLogHookAnsi2 :: [Handle] -> ScreenId -> X ()
myLogHookAnsi2 hs ns = mapM_ dynamicLogWithPP $ zipWith myLogHookAnsi2a hs [0..ns-1]

-- monokai

myLogHookMonokai1 :: Handle -> X()
myLogHookMonokai1 h = dynamicLogWithPP myLogHookMonokaiPP
  {
    ppOutput = hPutStrLn h
  }

myLogHookMonokai2a :: Handle -> ScreenId -> PP
myLogHookMonokai2a h s = myLogHookMonokaiPP
  {
    ppOutput = hPutStrLn h
  }

myLogHookMonokai2 :: [Handle] -> ScreenId -> X ()
myLogHookMonokai2 hs ns = mapM_ dynamicLogWithPP $ zipWith myLogHookMonokai2a hs [0..ns-1]

-- nord

myLogHookNord1 :: Handle -> X ()
myLogHookNord1 h = dynamicLogWithPP myLogHookNordPP
  {
    ppOutput = hPutStrLn h
  }

myLogHookNord2a :: Handle -> ScreenId -> PP
myLogHookNord2a h s = myLogHookNordPP
  {
    ppOutput = hPutStrLn h
  }

myLogHookNord2 :: [Handle] -> ScreenId -> X ()
myLogHookNord2 hs ns = mapM_ dynamicLogWithPP $ zipWith myLogHookNord2a hs [0..ns-1]

-- solarized dark

myLogHookSolarizedDark1 :: Handle -> X ()
myLogHookSolarizedDark1 h = dynamicLogWithPP myLogHookSolarizedDarkPP
  {
    ppOutput = hPutStrLn h
  }

myLogHookSolarizedDark2a :: Handle -> ScreenId -> PP
myLogHookSolarizedDark2a h s = myLogHookSolarizedDarkPP
  {
    ppOutput = hPutStrLn h
  }

myLogHookSolarizedDark2 :: [Handle] -> ScreenId -> X ()
myLogHookSolarizedDark2 hs ns = mapM_ dynamicLogWithPP $ zipWith myLogHookSolarizedDark2a hs [0..ns-1]

-- solarized light

myLogHookSolarizedLight1 :: Handle -> X ()
myLogHookSolarizedLight1 h = dynamicLogWithPP myLogHookSolarizedLightPP
  {
    ppOutput = hPutStrLn h
  }

myLogHookSolarizedLight2a :: Handle -> ScreenId -> PP
myLogHookSolarizedLight2a h s = myLogHookSolarizedLightPP
  {
    ppOutput = hPutStrLn h
  }

myLogHookSolarizedLight2 :: [Handle] -> ScreenId -> X ()
myLogHookSolarizedLight2 hs ns = mapM_ dynamicLogWithPP $ zipWith myLogHookSolarizedLight2a hs [0..ns-1]


-------------------------------------------------------------------------------
-- Bindings
-------------------------------------------------------------------------------

myKeysDmenuCommandAnsi =
  [
    ((mod1Mask,                  xK_d      ), spawn dmenuCommandAnsi), -- theme: ansi
    ((mod1Mask,                  xK_p      ), spawn dmenuCommandAnsi), -- theme: ansi
    ((0,                         xK_Menu   ), spawn dmenuCommandAnsi)  -- theme: ansi
  ]

myKeysDmenuCommandMonokai =
  [
    ((mod1Mask,                  xK_d      ), spawn dmenuCommandMonokai), -- theme: monokai
    ((mod1Mask,                  xK_p      ), spawn dmenuCommandMonokai), -- theme: monokai
    ((0,                         xK_Menu   ), spawn dmenuCommandMonokai)  -- theme: monokai
  ]

myKeysDmenuCommandNord =
  [
    ((mod1Mask,                  xK_d      ), spawn dmenuCommandNord), -- theme: nord
    ((mod1Mask,                  xK_p      ), spawn dmenuCommandNord), -- theme: nord
    ((0,                         xK_Menu   ), spawn dmenuCommandNord)  -- theme: nord
  ]

myKeysDmenuCommandSolarizedDark =
  [
    ((mod1Mask,                  xK_d      ), spawn dmenuCommandSolarizedDark), -- theme: solarized dark
    ((mod1Mask,                  xK_p      ), spawn dmenuCommandSolarizedDark), -- theme: solarized dark
    ((0,                         xK_Menu   ), spawn dmenuCommandSolarizedDark)  -- theme: solarized dark
  ]

myKeysDmenuCommandSolarizedLight =
  [
    ((mod1Mask,                  xK_d      ), spawn dmenuCommandSolarizedLight), -- theme: solarized light
    ((mod1Mask,                  xK_p      ), spawn dmenuCommandSolarizedLight), -- theme: solarized light
    ((0,                         xK_Menu   ), spawn dmenuCommandSolarizedLight)  -- theme: solarized light
  ]

myKeys =
  [
    ((mod1Mask,                  xK_Return ), spawn myTerminal),
    ((mod1Mask,                  xK_s      ), scratchPad),
    ((mod1Mask,                  xK_F4     ), kill),
    -- ((mod1Mask,                  xK_m      ), myStartUpScreen),
    ((0,                         xK_Print  ), spawn "scrot ~/screenshot_$(date +%Y%m%d.%H%M%S).jpg"),
    ((mod1Mask,                  xK_Print  ), spawn "$HOME/bin/screenshot.sh"),
    ((mod1Mask,                  xK_q      ), spawn "$HOME/.xmonad/recompile.sh"),
    ((mod1Mask .|. shiftMask,    xK_q      ), spawn "$HOME/.xmonad/exit.sh message"),
    ((mod1Mask .|. shiftMask,    xK_slash  ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -")),
    --
    ((mod1Mask,                  xK_f      ), sendMessage (Toggle "Full")),
    ((mod1Mask,                  xK_a      ), sendMessage MirrorShrink), -- shrink resizable area
    ((mod1Mask,                  xK_z      ), sendMessage MirrorExpand), -- expand resizable area
    --
    -- ((mod1Mask,                  xK_j      ), windows W.focusUp), -- switch to previous workspace
    -- ((mod1Mask,                  xK_k      ), windows W.focusDown), -- switch to next workspace
    ((mod1Mask,                  xK_j      ), Group.focusUp), -- switch to previous workspace
    ((mod1Mask,                  xK_k      ), Group.focusDown), -- switch to next workspace
    -- ((mod1Mask .|. shiftMask,    xK_j      ), windows W.swapUp),  -- swap the focused window with the previous window
    -- ((mod1Mask .|. shiftMask,    xK_k      ), windows W.swapDown), -- swap the focused window with the next window
    ((mod1Mask .|. shiftMask,    xK_j      ), Group.swapUp >> refresh),  -- swap the focused window with the previous window
    ((mod1Mask .|. shiftMask,    xK_k      ), Group.swapDown >> refresh), -- swap the focused window with the next window
    -- ((mod1Mask,                  xK_Down   ), nextScreen), -- cycling through screens
    -- ((mod1Mask,                  xK_Up     ), prevScreen), -- cycling through screens
    ((mod1Mask,                  xK_comma  ), prevScreen), -- previous screen
    ((mod1Mask,                  xK_period ), nextScreen), -- next screen
    ((mod1Mask .|. shiftMask,    xK_comma  ), shiftPrevScreen >> prevScreen), -- shift to previous screen
    ((mod1Mask .|. shiftMask,    xK_period ), shiftNextScreen >> nextScreen), -- shift to next screen
    -- ((mod1Mask .|. shiftMask,    xK_Down   ), swapNextScreen), -- cycling through screens
    -- ((mod1Mask .|. shiftMask,    xK_Up     ), swapPrevScreen), -- cycling through screens
    ((mod1Mask,                  xK_h      ), Group.focusGroupUp), -- move the focus to the previous group
    ((mod1Mask,                  xK_l      ), Group.focusGroupDown), -- move the focus to the next group
    ((mod1Mask .|. shiftMask,    xK_h      ), Group.moveToGroupUp False), -- move the focused window to the previous group
    ((mod1Mask .|. shiftMask,    xK_l      ), Group.moveToGroupDown False), -- move the focused window to the next group
    ((mod1Mask,                  xK_bracketleft), prevWS), -- previous workspace
    ((mod1Mask,                  xK_bracketright), nextWS), -- next workspace
    ((mod1Mask .|. shiftMask,    xK_Left), prevWS), -- previous workspace
    ((mod1Mask .|. shiftMask,    xK_Right), nextWS), -- next workspace
    ((mod1Mask .|. shiftMask,    xK_bracketleft),   DO.moveTo Prev HiddenNonEmptyWS), -- previous non empty workspace
    ((mod1Mask .|. shiftMask,    xK_bracketright),  DO.moveTo Next HiddenNonEmptyWS), -- previous non empty workspace
    ((mod1Mask .|. controlMask,  xK_Left),   DO.moveTo Prev HiddenNonEmptyWS), -- previous non empty workspace
    ((mod1Mask .|. controlMask,  xK_Right),  DO.moveTo Next HiddenNonEmptyWS), -- previous non empty workspace
    --
    ((mod1Mask,                  xK_m      ), withFocused minimizeWindow <+> windows W.focusDown),
    ((mod1Mask .|. shiftMask,    xK_m      ), withLastMinimized maximizeWindowAndFocus),
    --
    ((0, xF86XK_AudioLowerVolume           ), spawn "amixer -q set Master,0 5%- unmute"),
    ((0, xF86XK_AudioRaiseVolume           ), spawn "amixer -q set Master,0 5%+ unmute"),
    ((0, xF86XK_AudioMute                  ), spawn "amixer -q set Master,0 toggle"),
    ((0, xF86XK_MonBrightnessUp            ), spawn "$HOME/.xmonad/brigtness.sh inc 10"),
    ((0, xF86XK_MonBrightnessDown          ), spawn "$HOME/.xmonad/brigtness.sh dec 10"),
    ((0, xF86XK_ModeLock                   ), spawn "$HOME/.xmonad/exit.sh lock"),
    ((0, xF86XK_Mail                       ), spawn "evolution"),
    ((0, xF86XK_WWW                        ), spawn "$HOME/bin/vivaldi.sh noproxy"),
    ((0, xF86XK_Terminal                   ), spawn myTerminal),
    --
    ((mod1Mask .|. controlMask,  xK_c      ), spawn "$HOME/bin/vivaldi.sh noproxy"),
    ((mod1Mask .|. controlMask,  xK_f      ), spawn "firefox"),
    ((mod1Mask .|. controlMask,  xK_o      ), spawn "$HOME/bin/opera.sh proxy"),
    ((mod1Mask .|. controlMask,  xK_g      ), spawn "gajim"),
    ((mod1Mask .|. controlMask,  xK_m      ), spawn "evolution"),
    ((mod1Mask .|. controlMask,  xK_p      ), spawn "pidgin"),
    ((mod1Mask .|. controlMask,  xK_s      ), spawn "$HOME/bin/skype"),
    ((mod1Mask .|. controlMask,  xK_t      ), spawn "thunderbird"),
    ((mod1Mask .|. controlMask,  xK_v      ), spawn "VirtualBox"),
    --
    ((shiftMask .|. controlMask, xK_l      ), spawn "$HOME/.xmonad/exit.sh lock"),
    ((shiftMask .|. controlMask, xK_s      ), spawn "$HOME/.xmonad/exit.sh monitor_off"),
    ((shiftMask .|. controlMask, xK_m      ), spawn "$HOME/.xmonad/screen_toggle.sh -x"),
    ((shiftMask .|. controlMask, xK_x      ), spawn "$HOME/.xmonad/exit.sh message")
  ]
  ++
  -- (1) Replacing greedyView with view
  -- mod-[1..9] %! Switch to workspace N
  -- mod-shift-[1..9] %! Move client to workspace N
  [ ((m .|. mod1Mask, k), windows $ f i) -- Replace 'mod1Mask' with your mod key of choice.
    | (i, k) <- zip myWorkspaces ([xK_1 .. xK_9] ++ [ xK_0 ])
    -- , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)] -- default (greedyView)
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)] -- view
  ]
  ++
  -- (2) Reorder screens
  [ ((m .|. mod1Mask, key), screenWorkspace sc >>= flip whenJust (windows . f)) -- Replace 'mod1Mask' with your mod key of choice.
    -- | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..] -- default map
    | (key, sc) <- zip [xK_w, xK_e, xK_r] [0,2,1] -- was [0..] *** change to match your screen order ***
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
  ]
  where
    scratchPad = scratchpadSpawnActionTerminal myTerminalScratchpad

myMouse =
  [
    ((mod1Mask, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)), -- Set the window to floating mode and move by dragging
    ((mod1Mask, button2), (\w -> focus w >> windows W.shiftMaster)),                      -- Raise the window to the top of the stack
    ((mod1Mask, button3), (\w -> focus w >> Flex.mouseResizeWindow w)),                   -- Set the window to floating mode and resize by dragging
    ((mod1Mask, button4), (\_ -> prevWS)),                                                -- Switch to previous workspace
    ((mod1Mask, button5), (\_ -> nextWS)),                                                -- Switch to next workspace
    ((mod1Mask .|. shiftMask, button4), (\_ -> shiftToPrev)),                             -- Send client to previous workspace
    ((mod1Mask .|. shiftMask, button5), (\_ -> shiftToNext))                              -- Send client to next workspace
  ]


-------------------------------------------------------------------------------
-- Configurations
-------------------------------------------------------------------------------

myConfigDefault = def
    {
      terminal             = myTerminal,
      modMask              = myModMask,
      focusFollowsMouse    = myFocusFollowsMouse,
      borderWidth          = myBorderWidth,
      workspaces           = myWorkspaces,
      startupHook          = myStartUp >> myStartUpScreen,
      manageHook           = myManageHook <+> manageDocks <+> dynamicMasterHook <+> myManageScratchPad,
      handleEventHook      = handleEventHook def <+> docksEventHook
    } `additionalKeys` myKeys
      `additionalMouseBindings` myMouse

myConfigAnsi xmobar nScreens = myConfigDefault -- theme: ansi
    {
      normalBorderColor    = myNormalBorderColorAnsi,
      focusedBorderColor   = myFocusedBorderColorAnsi,
      layoutHook           = myLayoutHook myTabConfigAnsi,
      -- (1) single xmobar
      -- logHook              = myLogHookAnsi1 xmobar
      -- (2) multiple xmobar
      logHook              = updatePointer (0.5, 0.5) (0, 0) >> myLogHookAnsi2 xmobar nScreens
    } `additionalKeys` myKeysDmenuCommandAnsi

myConfigMonokai xmobar nScreens = myConfigDefault -- theme: monokai
    {
      normalBorderColor    = myNormalBorderColorMonokai,
      focusedBorderColor   = myFocusedBorderColorMonokai,
      layoutHook           = myLayoutHook myTabConfigMonokai,
      -- (1) single xmobar
      -- logHook              = myLogHookMonokai xmobar
      -- (2) multiple xmobar
      logHook              = updatePointer (0.5, 0.5) (0, 0) >> myLogHookMonokai2 xmobar nScreens
    } `additionalKeys` myKeysDmenuCommandMonokai

myConfigNord xmobar nScreens = myConfigDefault -- theme: nord
    {
      normalBorderColor    = myNormalBorderColorNord,
      focusedBorderColor   = myFocusedBorderColorNord,
      layoutHook           = myLayoutHook myTabConfigNord,
      -- (1) single xmobar
      -- logHook              = myLogHookNord1 xmobar
      -- (2) multiple xmobar
      logHook              = updatePointer (0.5, 0.5) (0, 0) >> myLogHookNord2 xmobar nScreens
    } `additionalKeys` myKeysDmenuCommandNord

myConfigSolarizedDark xmobar nScreens = myConfigDefault -- theme: solarized dark
    {
      normalBorderColor    = myNormalBorderColorSolarizedDark,
      focusedBorderColor   = myFocusedBorderColorSolarizedDark,
      -- (1) Solarized Dark
      -- layoutHook           = myLayoutHook myTabConfigSolarizedDark,
      -- (2) Solarized
      layoutHook           = myLayoutHook myTabConfigSolarized,
      -- (1) single xmobar
      -- logHook              = myLogHookSolarizedDark1 xmobar
      -- (2) multiple xmobar
      logHook              = updatePointer (0.5, 0.5) (0, 0) >> myLogHookSolarizedDark2 xmobar nScreens
    } `additionalKeys` myKeysDmenuCommandSolarizedDark

myConfigSolarizedLight xmobar nScreens = myConfigDefault -- theme: solarized light
    {
      normalBorderColor    = myNormalBorderColorSolarizedLight,
      focusedBorderColor   = myFocusedBorderColorSolarizedLight,
      -- (1) Solarized Light
      -- layoutHook           = myLayoutHook myTabConfigSolarizedLight,
      -- (2) Solarized
      layoutHook           = myLayoutHook myTabConfigSolarized,
      -- (1) single xmobar
      -- logHook              = myLogHookSolarizedLight1 xmobar
      -- (2) multiple xmobar
      logHook              = updatePointer (0.5, 0.5) (0, 0) >> myLogHookSolarizedLight2 xmobar nScreens
    } `additionalKeys` myKeysDmenuCommandSolarizedLight


-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
  -- (1) single xmobar
  -- xmobar1 <- spawnPipe xmobarCommand1
  -- xmonad $ myConfigAnsi xmobar1 1 -- theme: ansi
  -- xmonad $ myConfigMonokai xmobar1 1 -- theme: monokai
  -- xmonad $ myConfigNord xmobar1 1 -- theme: nord
  -- xmonad $ myConfigSolarizedDark xmobar1 1 -- theme: solarized dark
  -- xmonad $ myConfigSolarizedLight xmobar1 1 -- theme: solarized light
  --
  -- (2) multiple xmobar
  -- kill <- mapM_ spawn ["killall -s 9 trayer", "killall -s 9 xmobar", "killall -s 9 conky"]
  nScreens <- countScreens
  xmobar2  <- mapM (spawnPipe . xmobarCommand2) [0 .. (nScreens - 1)]
  -- xmonad $ myConfigAnsi xmobar2 nScreens -- theme: ansi
  -- xmonad $ myConfigMonokai xmobar2 nScreens -- theme: monokai
  xmonad $ myConfigNord xmobar2 nScreens -- theme: nord
  -- xmonad $ myConfigSolarizedDark xmobar2 nScreens -- theme: solarized dark
  -- xmonad $ myConfigSolarizedLight xmobar2 nScreens -- theme: solarized light
  --
  -- (3) i3status
  -- xmobar1 <- spawnPipe i3statusCommand1
  -- xmonad $ myConfigAnsi xmobar1 1 -- theme: ansi
  -- xmonad $ myConfigMonokai xmobar1 1 -- theme: edge dark
  -- xmonad $ myConfigNord xmobar1 1 -- theme: nord
  -- xmonad $ myConfigSolarizedDark xmobar1 1 -- theme: solarized dark
  -- xmonad $ myConfigSolarizedLight xmobar1 1 -- theme: solarized light

-------------------------------------------------------------------------------
-- end
-------------------------------------------------------------------------------