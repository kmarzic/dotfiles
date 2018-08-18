-- xmonad.hs
-- Last update: 2018-08-18 20:33:15 (CEST)

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.UpdatePointer
import XMonad.Config
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicHooks
import XMonad.Hooks.DynamicLog
import XMonad.Layout.Column
import XMonad.Layout.Gaps
import XMonad.Layout.Groups.Helpers
import XMonad.Layout.IndependentScreens
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Roledex
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ToggleLayouts
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Util.EZConfig
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Scratchpad
import XMonad.Util.SpawnOnce
import Graphics.X11.ExtraTypes.XF86
import System.IO
import qualified Data.Map as M
import qualified XMonad.Actions.FlexibleResize as Flex
import qualified XMonad.Layout.Groups as G
import qualified XMonad.Layout.Groups.Helpers as Group
import qualified XMonad.StackSet as W


help :: String
help = unlines
  [
    "The default modifier key is 'alt'. Keybindings:",
    "",
    "-- launching and killing programs",
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
    "-- modifying the window order",
    "mod-Shift-j          Swap the focused window with the next window",
    "mod-Shift-k          Swap the focused window with the previous window",
    "mod-Shift-l          Move the focused window to the previous Group",
    "mod-Shift-h          Move the focused window to the next Group",
    "",
    "-- resizing the master/slave ratio",
    "mod-f                Toggle full screen",
    "mod-a                Shrink resizable area",
    "mod-z                Expand resizable area",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[0/~,1..9] Move client to Workspace N",
    "mod-{w,e,r}          Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}    Move client to screen 1, 2, or 3",
    "mod-PageUp           Prev Screen",
    "mod-PageDown         Next Screen",
    "mod-Shift-PageUp     Swap Prev Screen",
    "mod-Shift-PageDown   Swap Next Screen",
    "mod-[0/~,1..9]       Switch to Workspace N",
    "mod-[                Previous Workspace",
    "mod-]                Next Workspace",
    "mod-shift-Left       Previous Workspace",
    "mod-shift-Right      Next Workspace",
    "",
    "-- floating layer support",
    "mod-t                Push window back into tiling; unfloat and re-tile it",
    "",
    "-- quit, or restart",
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
fontRegular = "xft:Monospace:pixelsize=14:antialias=true:style=regular"

fontBold :: String
fontBold = "xft:Monospace:pixelsize=14:antialias=true:style=bold"
-- fontBold = "xft:Monospace:pixelsize=12:antialias=true:style=bold"
-- fontBold = "xft:Monospace:pixelsize=13:antialias=true:style=bold"
-- fontBold = "xft:Monospace:pixelsize=14:antialias=true:style=bold"
-- fontBold = "xft:Terminus:pixelsize=12:antialias=true:style=bold"
-- fontBold = "xft:Terminus:pixelsize=13:antialias=true:style=bold"
-- fontBold = "xft:Terminus:pixelsize=14:antialias=true:style=bold"

fontTerminalScratchpad :: String
fontTerminalScratchpad = "xft:Monospace:pixelsize=14:antialias=true:style=bold"

dmenuCommandBlue :: String -- theme: blue
dmenuCommandBlue = "/usr/bin/dmenu_run -i -nf \"#ffffff\" -nb \"#222222\" -sb \"#0088cc\" -sf \"#ffffff\" -fn " ++ fontRegular ++ " -p 'Run: '"

dmenuCommandGreen :: String -- theme: green
dmenuCommandGreen = "/usr/bin/dmenu_run -i -nf \"#ffffff\" -nb \"#222222\" -sb \"#009910\" -sf \"#ffffff\" -fn " ++ fontRegular ++ " -p 'Run: '"

dmenuCommandSolarizedDark :: String -- theme: solarized dark
dmenuCommandSolarizedDark = "/usr/bin/dmenu_run -i -nf \"#2aa198\" -nb \"#002b36\" -sb \"#2aa198\" -fn " ++ fontRegular ++ " -p 'Run: '"

dmenuCommandSolarizedLight :: String -- theme: solarized light
dmenuCommandSolarizedLight = "/usr/bin/dmenu_run -i -nf \"#2aa198\" -nb \"#fdf6e3\" -sb \"#2aa198\" -fn " ++ fontRegular ++ " -p 'Run: '"

xmobarCommand1 :: String
xmobarCommand1 = "xmobar $HOME/.xmonad/xmobar.hs"

xmobarCommand2 :: ScreenId -> String
xmobarCommand2 (S s) = unwords ["xmobar", "-x", show s, "$HOME/.xmonad/xmobar.hs"]

myTerminal :: String
myTerminal = "urxvt"
-- myTerminal = "termite"
-- myTerminal = "$HOME/bin/st.solarized.light"

myTerminalScratchpad :: String
myTerminalScratchpad = "urxvt -fn " ++ fontTerminalScratchpad
-- myTerminalScratchpad = "kitty &"
-- myTerminalScratchpad = "termite --class=termscratch"
-- myTerminalScratchpad = "tilda -f " ++ fontTerminalScratchpad
-- myTerminalScratchpad = "$HOME/bin/st.solarized.light"

myModMask :: KeyMask
myModMask = mod1Mask

myFocusFollowsMouse :: Bool
-- myFocusFollowsMouse = True
myFocusFollowsMouse = False

myBorderWidth :: Dimension
myBorderWidth = 1
-- myBorderWidth = 2

myNormalBorderColorBlue :: String -- theme: blue
myNormalBorderColorBlue = "#ffffff"
myFocusedBorderColorBlue :: String
myFocusedBorderColorBlue = "#0088cc"

myNormalBorderColorGreen :: String -- theme: green
myNormalBorderColorGreen = "#ffffff"
myFocusedBorderColorGreen :: String
myFocusedBorderColorGreen = "#009900"

myNormalBorderColorSolarizedDark :: String -- theme: solarized dark
myNormalBorderColorSolarizedDark = "#002b36" -- base03
myFocusedBorderColorSolarizedDark :: String
myFocusedBorderColorSolarizedDark = "#2aa198" -- cyan

myNormalBorderColorSolarizedLight :: String -- theme: solarized light
myNormalBorderColorSolarizedLight = "#fdf6e3" -- base3
myFocusedBorderColorSolarizedLight :: String
myFocusedBorderColorSolarizedLight = "#268bd2" -- blue

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

myTabConfigBlue :: Theme -- theme: blue
myTabConfigBlue = def
  {
    activeColor = "#0088cc",
    activeTextColor = "#ffffff",
    activeBorderColor = "#000000",
    inactiveColor = "#5f676a",
    inactiveTextColor = "#dddddd",
    inactiveBorderColor = "#000000",
    urgentColor = "#900000",
    urgentTextColor = "#ffffff",
    urgentBorderColor = "#2f343a",
    fontName = fontBold
  }

myTabConfigGreen :: Theme -- theme: green
myTabConfigGreen = def
  {
    activeColor = "#009900",
    activeTextColor = "#ffffff",
    activeBorderColor = "#000000",
    inactiveColor = "#5f676a",
    inactiveTextColor = "#dddddd",
    inactiveBorderColor = "#000000",
    urgentColor = "#900000",
    urgentTextColor = "#ffffff",
    urgentBorderColor = "#2f343a",
    fontName = fontBold
  }

myTabConfigSolarized :: Theme -- theme: solarized [ light and dark ]
myTabConfigSolarized = def
  {
    activeColor = "#657b83", -- base00
    activeTextColor = "#eee8d5", -- base2
    activeBorderColor = "#93a1a1", -- base1
    inactiveColor = "#002b36", -- base03
    inactiveTextColor = "#93a1a1", -- base1
    inactiveBorderColor = "#93a1a1", -- base1
    urgentColor = "#900000",
    urgentTextColor = "#ffffff",
    urgentBorderColor = "#2f343a",
    fontName = fontBold
  }

myTabConfigSolarizedDark :: Theme -- theme: solarized dark
myTabConfigSolarizedDark = def
  {
    activeColor = "#657b83", -- base00
    activeTextColor = "#eee8d5", -- base2
    activeBorderColor = "#93a1a1", -- base1
    inactiveColor = "#002b36", -- base03
    inactiveTextColor = "#93a1a1", -- base1
    inactiveBorderColor = "#93a1a1", -- base1
    urgentColor = "#900000",
    urgentTextColor = "#ffffff",
    urgentBorderColor = "#2f343a",
    fontName = fontBold
  }

myTabConfigSolarizedLight :: Theme -- theme: solarized light
myTabConfigSolarizedLight = def
  {
    activeColor = "#859900", -- green
    activeTextColor = "#fdf6e3", -- base3
    activeBorderColor = "#002b36", -- base03
    inactiveColor = "#93a1a1", -- base1
    inactiveTextColor = "#fdf6e3", -- base3
    inactiveBorderColor = "#002b36", -- base03
    urgentColor = "#900000",
    urgentTextColor = "#ffffff",
    urgentBorderColor = "#2f343a",
    fontName = fontBold
  }

myLogHookBluePP :: PP -- theme: blue
myLogHookBluePP = def
  {
    ppCurrent         = xmobarColor "cyan" "" . wrap "[" "]",
    ppHidden          = xmobarColor "#ffffff" "",
    ppHiddenNoWindows = xmobarColor "#999999" "",
    ppTitle           = xmobarColor "green" "" . shorten 0,
    ppVisible         = wrap "(" ")",
    ppUrgent          = xmobarColor "red" "yellow",
    ppLayout          = xmobarColor "#dddddd" "" .
      ( \layout -> case layout of
          "Tabbed Simplest by Full" -> "[_]"
          "Full by Full"            -> "[ ]"
          "Tall by Full"            -> "[|]"
          "Mirror Tall by Full"     -> "[-]"
          "Roledex by Full"         -> "[@]"
      ),
    ppSep             = "  ", -- separator between each object
    ppWsSep           = " " -- separator between workspaces
  }

myLogHookGreenPP :: PP -- theme: green
myLogHookGreenPP = def
  {
    ppCurrent         = xmobarColor "green" "" . wrap "[" "]",
    ppHidden          = xmobarColor "#ffffff" "",
    ppHiddenNoWindows = xmobarColor "#999999" "",
    ppTitle           = xmobarColor "green" "" . shorten 0,
    ppVisible         = wrap "(" ")",
    ppUrgent          = xmobarColor "red" "yellow",
    ppLayout          = xmobarColor "#dddddd" "" .
      ( \layout -> case layout of
          "Tabbed Simplest by Full" -> "[_]"
          "Full by Full"            -> "[ ]"
          "Tall by Full"            -> "[|]"
          "Mirror Tall by Full"     -> "[-]"
          "Roledex by Full"         -> "[@]"
      ),
    ppSep             = "  ", -- separator between each object
    ppWsSep           = " " -- separator between workspaces
  }

myLogHookSolarizedDarkPP :: PP -- theme: solarized dark
myLogHookSolarizedDarkPP = def
  {
    ppCurrent         = xmobarColor "#002b36" "#2aa198" . wrap "[" "]", -- base03/cyan
    ppHidden          = xmobarColor "#fdf6e3" "", -- base3
    ppHiddenNoWindows = xmobarColor "#93a1a1" "", -- base1
    ppTitle           = xmobarColor "#2aa198" "" . shorten 50, -- cyan
    ppVisible         = wrap "(" ")",
    ppUrgent          = xmobarColor "#dc322f" "#b58900", -- red/yellow
    ppLayout          = xmobarColor "#002b36" "#2aa198" . -- base03/cyan
      ( \layout -> case layout of
          "Tabbed Simplest by Full" -> "[_]"
          "Full by Full"            -> "[ ]"
          "Tall by Full"            -> "[|]"
          "Mirror Tall by Full"     -> "[-]"
          "Roledex by Full"         -> "[@]"
      ),
    ppSep             = "  ", -- separator between each object
    ppWsSep           = " " -- separator between workspaces
  }

myLogHookSolarizedLightPP :: PP -- theme: solarized light
myLogHookSolarizedLightPP = def
  {
    ppCurrent         = xmobarColor "#fdf6e3" "#268bd2" . wrap "[" "]", -- base3/blue
    ppHidden          = xmobarColor "#002b36" "", -- base03
    ppHiddenNoWindows = xmobarColor "#93a1a1" "", -- base1
    ppTitle           = xmobarColor "#268bd2" "" . shorten 50, -- blue
    ppVisible         = wrap "(" ")",
    ppUrgent          = xmobarColor "#dc322f" "#b58900", -- red/yellow
    ppLayout          = xmobarColor "#fdf6e3" "#268bd2" . -- base3/blue
      ( \layout -> case layout of
          "Tabbed Simplest by Full" -> "[_]"
          "Full by Full"            -> "[ ]"
          "Tall by Full"            -> "[|]"
          "Mirror Tall by Full"     -> "[-]"
          "Roledex by Full"         -> "[@]"
      ),
    ppSep             = "  ", -- separator between each object
    ppWsSep           = " " -- separator between workspaces
  }

myLayoutHook tabConfig =
    gaps [(U,0), (D,0), (L,0), (R,0)]
  $ (flip G.group) (Full ||| Mirror (Column 1.41) ||| Mirror (Column 1))
  $ smartBorders
  $ avoidStruts
  $ toggleLayouts (noBorders $ full')
  $ toggleLayouts (noBorders $ tab2')
  $ myLayouts
  -- $ tab2' ||| full' ||| tiled' ||| mirror' ||| threecol' ||| resizetab' ||| roledex'
  where
    -- myLayouts  = tab2' ||| tiled' ||| mirror' ||| threecol' ||| full' ||| resizetab' ||| roledex'
    -- myLayouts  = tab2' ||| full' ||| tiled' ||| mirror' ||| roledex'
    myLayouts  = full' |||  tab2' ||| tiled' ||| mirror' ||| roledex'
    --
    -- tab1'      = tabbed shrinkText tabConfig
    tab2'      = tabbedAlways shrinkText tabConfig
    tiled'     = Tall nmaster delta ratio
    mirror'    = Mirror tiled'
    threecol'  = ThreeColMid nmaster delta ratio
    full'      = Full
    resizetab' = ResizableTall 1 (3/100) (1/2) []
    roledex'   = Roledex
    --
    -- The default number of windows in the master pane
    nmaster  = 1
    -- Default proportion of screen occupied by master pane
    ratio    = 1/2
    -- Percent of screen to increment by when resizing panes
    delta    = 2/100

myStartUp :: X()
myStartUp = do
  -- spawnOnce "feh --bg-scale ~/wallpapers/green/lines_spots_color_texture_50390_3840x2400.jpg"
  -- spawnOnce "setxkbmap -model pc105 -option 'eurosign:e,lv3:ralt_switch,compose:nocaps' 'hr(us)'"
  -- spawnOnce "dunst -config $HOME/.config/dunst/dunstrc"
  spawn "$HOME/.xmonad/screen_toggle.sh -x"
  spawn "$HOME/.xmonad/trayer.sh"

myManageHook :: ManageHook
myManageHook = composeAll . concat $
-- xprop | grep WM_CLASS
    [
      [className =? "Chromium" --> doShift (myWorkspaces !! 4)],
      [className =? "Chromium-browser" --> doShift (myWorkspaces !! 4)],
      [className =? "Chrome" --> doShift (myWorkspaces !! 4)],
      [className =? "Opera" --> doShift (myWorkspaces !! 4)],
      [className =? "Firefox" --> doShift (myWorkspaces !! 4)],
      [className =? "Firefox-esr" --> doShift (myWorkspaces !! 4)],
      [className =? "Vivaldi" --> doShift (myWorkspaces !! 4)],
      [className =? "Vivaldi-stable" --> doShift (myWorkspaces !! 4)],
      [className =? "Pidgin" --> doShift (myWorkspaces !! 6)],
      [className =? "Skype" --> doShift (myWorkspaces !! 6)],
      [className =? "VirtualBox Manager" --> doShift (myWorkspaces !! 7)],
      [className =? "Evolution" --> doShift (myWorkspaces !! 8)],
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

myLogHookBlue1 :: Handle -> X()
myLogHookBlue1 h = dynamicLogWithPP myLogHookBluePP
  {
    ppOutput = hPutStrLn h
  }

myLogHookBlue2a :: Handle -> ScreenId -> PP
myLogHookBlue2a h s = myLogHookBluePP
  {
    ppOutput = hPutStrLn h
  }

myLogHookBlue2 :: [Handle] -> ScreenId -> X ()
myLogHookBlue2 hs ns = mapM_ dynamicLogWithPP $ zipWith myLogHookBlue2a hs [0..ns-1]

myLogHookGreen1 :: Handle -> X()
myLogHookGreen1 h = dynamicLogWithPP myLogHookGreenPP
  {
    ppOutput = hPutStrLn h
  }

myLogHookGreen2a :: Handle -> ScreenId -> PP
myLogHookGreen2a h s = myLogHookGreenPP
  {
    ppOutput = hPutStrLn h
  }

myLogHookGreen2 :: [Handle] -> ScreenId -> X ()
myLogHookGreen2 hs ns = mapM_ dynamicLogWithPP $ zipWith myLogHookGreen2a hs [0..ns-1]

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

myManageScratchPad :: ManageHook
myManageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
  where
    h = 0.6 -- terminal height, 60%
    w = 0.6 -- terminal width, 60%
    t = 0.3 -- distance from top edge, 30%
    l = 0.1 -- distance from left edge, 10%

myKeysDmenuCommandBlue =
  [
    ((mod1Mask,                  xK_d      ), spawn dmenuCommandBlue), -- theme: blue
    ((0,                         xK_Menu   ), spawn dmenuCommandBlue)  -- theme: blue
  ]

myKeysDmenuCommandGreen =
  [
    ((mod1Mask,                  xK_d      ), spawn dmenuCommandGreen), -- theme: green
    ((0,                         xK_Menu   ), spawn dmenuCommandGreen)  -- theme: green
  ]

myKeysDmenuCommandSolarizedDark =
  [
    ((mod1Mask,                  xK_d      ), spawn dmenuCommandSolarizedDark), -- theme: solarized dark
    ((0,                         xK_Menu   ), spawn dmenuCommandSolarizedDark)  -- theme: solarized dark
  ]

myKeysDmenuCommandSolarizedLight =
  [
    ((mod1Mask,                  xK_d      ), spawn dmenuCommandSolarizedLight), -- theme: solarized light
    ((0,                         xK_Menu   ), spawn dmenuCommandSolarizedLight)  -- theme: solarized light
  ]

myKeys =
  [
    ((mod1Mask,                  xK_u      ), spawn myTerminal),
    ((mod1Mask,                  xK_Return ), spawn myTerminal),
    ((mod1Mask,                  xK_s      ), scratchPad),
    ((mod1Mask,                  xK_F4     ), kill),
    ((0,                         xK_Print  ), spawn "scrot ~/screenshot_$(date +%Y%m%d.%H%M%S).jpg"),
    ((mod1Mask,                  xK_Print  ), spawn "$HOME/bin/screenshot.sh"),
    ((mod1Mask,                  xK_q      ), spawn "$HOME/.xmonad/recompile.sh"),
    ((mod1Mask .|. shiftMask,    xK_q      ), spawn "$HOME/.xmonad/exit.sh message"),
    ((mod1Mask .|. shiftMask,    xK_slash  ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -")),
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
    ((mod1Mask,                  xK_Down   ), nextScreen), -- cycling through screens
    ((mod1Mask,                  xK_Up     ), prevScreen), -- cycling through screens
    ((mod1Mask .|. shiftMask,    xK_Down   ), swapNextScreen), -- cycling through screens
    ((mod1Mask .|. shiftMask,    xK_Up     ), swapPrevScreen), -- cycling through screens
    ((mod1Mask,                  xK_h      ), Group.focusGroupUp), -- move the focus to the previous group
    ((mod1Mask,                  xK_l      ), Group.focusGroupDown), -- move the focus to the next group
    ((mod1Mask .|. shiftMask,    xK_h      ), Group.moveToGroupUp False), -- move the focused window to the previous group
    ((mod1Mask .|. shiftMask,    xK_l      ), Group.moveToGroupDown False), -- move the focused window to the next group
    ((mod1Mask,                  xK_bracketleft), prevWS), -- previous workspace
    ((mod1Mask,                  xK_bracketright), nextWS), -- next workspace
    ((mod1Mask .|. shiftMask,    xK_Left), prevWS), -- previous workspace
    ((mod1Mask .|. shiftMask,    xK_Right), nextWS), -- next workspace
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
    ((mod1Mask .|. controlMask,  xK_g      ), spawn "gvim"),
    ((mod1Mask .|. controlMask,  xK_m      ), spawn "evolution"),
    ((mod1Mask .|. controlMask,  xK_p      ), spawn "pidgin"),
    ((mod1Mask .|. controlMask,  xK_s      ), spawn "$HOME/bin/skype"),
    ((mod1Mask .|. controlMask,  xK_v      ), spawn "VirtualBox"),
    ((mod1Mask .|. controlMask,  xK_y      ), spawn "/opt/yakyak-linux-x64/yakyak"),
    --
    ((shiftMask .|. controlMask, xK_l      ), spawn "$HOME/.xmonad/exit.sh lock"),
    ((shiftMask .|. controlMask, xK_s      ), spawn "$HOME/.xmonad/exit.sh monitor_off"),
    ((shiftMask .|. controlMask, xK_m      ), spawn "$HOME/.xmonad/screen_toggle.sh -x"),
    ((shiftMask .|. controlMask, xK_x      ), spawn "$HOME/.xmonad/exit.sh message")
  ]
  ++
  -- Replacing greedyView with view
  -- mod-[1..9] %! Switch to workspace N
  -- mod-shift-[1..9] %! Move client to workspace N
  [ ((m .|. mod1Mask, k), windows $ f i) -- Replace 'mod1Mask' with your mod key of choice.
    | (i, k) <- zip myWorkspaces ([xK_1 .. xK_9] ++ [ xK_0 ])
    -- , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)] -- default (greedyView)
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)] -- view
  ]
  ++
  -- Reorder screens
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

myConfigDefault = def
    {
      terminal             = myTerminal,
      modMask              = myModMask,
      focusFollowsMouse    = myFocusFollowsMouse,
      borderWidth          = myBorderWidth,
      workspaces           = myWorkspaces,
      startupHook          = myStartUp,
      manageHook           = myManageHook <+> manageDocks <+> dynamicMasterHook <+> myManageScratchPad,
      handleEventHook      = handleEventHook def <+> docksEventHook
    }

myConfigSolarizedBlue xmobar nScreens = myConfigDefault -- theme: blue
    {
      normalBorderColor    = myNormalBorderColorBlue,
      focusedBorderColor   = myFocusedBorderColorBlue,
      layoutHook           = myLayoutHook myTabConfigBlue,
      -- logHook              = myLogHookBlue1 xmobar,
      logHook              = updatePointer (0.5, 0.5) (0, 0) >> myLogHookBlue2 xmobar nScreens
    } `additionalKeys` myKeys
      `additionalKeys` myKeysDmenuCommandBlue
      `additionalMouseBindings` myMouse

myConfigSolarizedGreen xmobar nScreens = myConfigDefault -- theme: green
    {
      normalBorderColor    = myNormalBorderColorGreen,
      focusedBorderColor   = myFocusedBorderColorGreen,
      layoutHook           = myLayoutHook myTabConfigGreen,
      -- logHook              = myLogHookGreen1 xmobar,
      logHook              = updatePointer (0.5, 0.5) (0, 0) >> myLogHookGreen2 xmobar nScreens
    } `additionalKeys` myKeys
      `additionalKeys` myKeysDmenuCommandGreen
      `additionalMouseBindings` myMouse

myConfigSolarizedDark xmobar nScreens = myConfigDefault -- theme: solarized dark
    {
      normalBorderColor    = myNormalBorderColorSolarizedDark,
      focusedBorderColor   = myFocusedBorderColorSolarizedDark,
      -- layoutHook           = myLayoutHook myTabConfigSolarizedDark,
      layoutHook           = myLayoutHook myTabConfigSolarized,
      -- logHook              = myLogHookSolarizedDark1 xmobar,
      logHook              = updatePointer (0.5, 0.5) (0, 0) >> myLogHookSolarizedDark2 xmobar nScreens
    } `additionalKeys` myKeys
      `additionalKeys` myKeysDmenuCommandSolarizedDark
      `additionalMouseBindings` myMouse

myConfigSolarizedLight xmobar nScreens = myConfigDefault -- theme: solarized light
    {
      normalBorderColor    = myNormalBorderColorSolarizedLight,
      focusedBorderColor   = myFocusedBorderColorSolarizedLight,
      -- layoutHook           = myLayoutHook myTabConfigSolarizedLight,
      layoutHook           = myLayoutHook myTabConfigSolarized,
      -- logHook              = myLogHookSolarizedLight1 xmobar,
      logHook              = updatePointer (0.5, 0.5) (0, 0) >> myLogHookSolarizedLight2 xmobar nScreens
    } `additionalKeys` myKeys
      `additionalKeys` myKeysDmenuCommandSolarizedLight
      `additionalMouseBindings` myMouse

main :: IO ()
main = do
  -- (1) single xmobar
  -- xmobar1 <- spawnPipe xmobarCommand1
  -- xmonad $ myConfigBlue xmobar1 1 -- theme: blue
  -- xmonad $ myConfigGreen xmobar1 1 -- theme: green
  -- xmonad $ myConfigSolarizedDark xmobar1 1 -- theme: solarized dark
  -- xmonad $ myConfigSolarizedLight xmobar1 1 -- theme: solarized light
  -- (2) multiple xmobar
  kill <- mapM_ spawn ["killall -s 9 trayer", "killall -s 9 xmobar", "killall -s 9 conky"]
  nScreens <- countScreens
  xmobar2  <- mapM (spawnPipe . xmobarCommand2) [0 .. (nScreens - 1)]
  -- xmonad $ myConfigBlue xmobar2 nScreens -- theme: solarized light
  -- xmonad $ myConfigGreen xmobar2 nScreens -- theme: solarized light
  xmonad $ myConfigSolarizedDark xmobar2 nScreens -- theme: solarized light
  -- xmonad $ myConfigSolarizedLight xmobar2 nScreens -- theme: solarized light

-- end
