-------------------------------------------------------------------------------
-- xmonad.hs
-- Last update: 2021-08-21 19:38:36 (CEST)
-------------------------------------------------------------------------------

import Data.Maybe ( maybeToList )
import Data.List ( (\\) )
import XMonad hiding ( (|||) )
import XMonad.Actions.CycleWS
import XMonad.Actions.Minimize (minimizeWindow, withLastMinimized, maximizeWindowAndFocus)
import XMonad.Actions.UpdatePointer
import XMonad.Config
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicHooks
import XMonad.Hooks.DynamicLog
import XMonad.Layout hiding ( (|||) )
import XMonad.Layout.Column
import XMonad.Layout.Gaps
import XMonad.Layout.Groups.Helpers
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.IndependentScreens
import XMonad.Layout.Minimize(minimize)
import XMonad.Layout.Named
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
import XMonad.Util.NamedWindows (getName)
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
    "mod-Menu             Launch rofi",
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
    "mod-i                Increment the number of windows in the master area",
    "mod-d                Deincrement the number of windows in the master area",
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
fontRegular = "monospace:size=10:antialias=true:style=regular"
-- fontRegular = "monospace:size=11:antialias=true:style=regular"
-- fontRegular = "xft:monospace:size=12:antialias=true:style=regular"
-- fontRegular = "xft:monospace:pixelsize=13:antialias=true:style=regular"
-- fontRegular = "xft:monospace:pixelsize=14:antialias=true:style=regular"
-- fontRegular = "Liberation Mono:pixelsize=12:antialias=true:autohint=true:style=regular"

fontBold :: String
fontBold = "monospace:size=10:antialias=true:style=bold"
-- fontBold = "monospace:size=11:antialias=true:style=bold"
-- fontBold = "xft:monospace:size=12:antialias=true:style=bold"
-- fontBold = "xft:monospace:pixelsize=13:antialias=true:style=bold"
-- fontBold = "xft:monospace:pixelsize=14:antialias=true:style=bold"
-- fontBold = "xft:Liberation Mono:size=12:antialias=true:autohint=true:style=bold"

fontTerminalScratchpad :: String
fontTerminalScratchpad = "monospace:size=10:antialias=true:style=bold,Source\\ Code\\ Pro\\ Medium:size=10:antialias=true:hinting=true:style:bold"
-- fontTerminalScratchpad = "xft:monospace:size=12:antialias=true:style=bold,xft:Source\\ Code\\ Pro\\ Medium:pixelsize=18:antialias=true:hinting=true:style:bold"
-- fontTerminalScratchpad = "xft:DejaVu Sans Mono:size=12:antialias=true:autohint=true:style=regular"

dmenuCommandAnsi :: String -- theme: ansi
-- dmenuCommandAnsi = "/usr/bin/dmenu_run -i -nf \"#00ffff\" -nb \"#101010\" -sb \"#00ffff\" -sf \"#101010\" -fn " ++ fontRegular ++ " -p 'Run: '"
dmenuCommandAnsi = "$HOME/bin/dmenu_run -i -nf \"#00ffff\" -nb \"#101010\" -sb \"#00ffff\" -sf \"#101010\" -fn " ++ fontRegular ++ " -p 'Run: '"

rofiCommand :: String
rofiCommand = "rofi -show run"

xmobarCommand1 :: String
xmobarCommand1 = "xmobar $HOME/.xmonad/xmobar.hs"

xmobarCommand2 :: ScreenId -> String
xmobarCommand2 (S s) = unwords ["xmobar", "-x", show s, "$HOME/.xmonad/xmobar.hs"]

dzenCommand2 :: ScreenId -> String
dzenCommand2 (S s) = unwords ["dzen2 -x '1440' -y '0' -h '24' -w '640' -ta 'l'", "-xs", show s]

myTerminal :: String
-- myTerminal = "urxvt"
-- myTerminal = "urxvtc"
-- myTerminal = "termite"
myTerminal = "$HOME/bin/st"

myTerminalScratchpad :: String
-- myTerminalScratchpad = "urxvt -fn " ++ fontTerminalScratchpad
-- myTerminalScratchpad = "kitty &"
-- myTerminalScratchpad = "termite --class=termscratch"
-- myTerminalScratchpad = "tilda -f " ++ fontTerminalScratchpad
myTerminalScratchpad = "$HOME/bin/st -n scratchpad -f " ++ fontTerminalScratchpad ++ " &"
-- myTerminalScratchpad = "gnome-terminal &"

myModMask :: KeyMask
myModMask = mod1Mask

myFocusFollowsMouse :: Bool
-- myFocusFollowsMouse = True
myFocusFollowsMouse = False

myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth :: Dimension
myBorderWidth = 1
-- myBorderWidth = 2

myNormalBorderColorAnsi :: String -- theme: ansi
myNormalBorderColorAnsi = ansi M.! "white"
myFocusedBorderColorAnsi :: String
myFocusedBorderColorAnsi = ansi M.! "cyan"

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
    screenWorkspace 0 >>= flip whenJust (windows . W.view)
  else if nScreens == 3
  then do
    screenWorkspace 2 >>= flip whenJust (windows . W.view)
    windows $ W.greedyView "1"
    screenWorkspace 0 >>= flip whenJust (windows . W.view)
    windows $ W.greedyView "0"
    screenWorkspace 1 >>= flip whenJust (windows . W.view)
    windows $ W.greedyView "9"
    screenWorkspace 2 >>= flip whenJust (windows . W.view)
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
    h = 0.7 -- terminal height, 70%
    w = 0.7 -- terminal width, 70%
    t = 0.2 -- distance from top edge, 20%
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
    [className =? "Microsoft Teams - Preview" --> doShift (myWorkspaces !! 6)],
    [className =? "Microsoft Teams Notification" --> doShift (myWorkspaces !! 6)],
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

myLayoutHook tabConfig =
  avoidStruts
  $ (flip G.group) (Full)
  $ full' ||| tab2' ||| tiled' ||| mirror' ||| roledex'
  where
    tab2'      = named "tab2'" (spacingRaw True (Border 2 2 2 2) True (Border 2 2 2 2) True $ tabbedAlways shrinkText tabConfig)
    --
    tiled'     = named "tiled'" (spacingRaw True (Border 2 2 2 2) True (Border 2 2 2 2) True $ Tall nmaster0 delta0 ratio0)
    --
    mirror'    = named "mirror'" (Mirror tiled')
    --
    threecol'  = named "threecol'" (ThreeColMid nmaster0 delta0 ratio0)
    --
    full'      = named "full'" (gaps1 $ Full)
    --
    resizetab' = named "resizetab'" (ResizableTall 1 (3/100) (1/2) [])
    --
    roledex'   = named "roledex'" (Roledex)
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
    gaps1      = gaps [(U,4), (D,4), (L,4), (R,4)]


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
      "Mirror Spacing Tall by Full"     -> myIcon ".xmonad/icons/layout_mirror.xbm"
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

myXmobarLogHookAnsiPP :: PP -- theme: ansi
myXmobarLogHookAnsiPP = def
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

myDzen2LogHookAnsiPP :: PP -- theme: ansi
myDzen2LogHookAnsiPP = def
  {
    ppCurrent         = dzenColor (ansi M.! "cyan") "" . wrap "[" "]",
    ppHidden          = dzenColor (ansi M.! "white") "",
    ppHiddenNoWindows = dzenColor (ansi M.! "gray99") "",
    ppTitle           = dzenColor (ansi M.! "cyan") "" . shorten 50,
    ppVisible         = wrap "(" ")",
    ppUrgent          = dzenColor (ansi M.! "red") (ansi M.! "yellow"),
    ppLayout          = dzenColor (ansi M.! "green") "" . (\layout -> myPPLayout (layout)),
    ppSep             = " ", -- separator between each object
    ppWsSep           = " ", -- separator between workspaces
    -- (1)
    -- ppExtras          = [ logTitles ],
    -- ppOrder           = \(ws:l:t:ts:_) -> ws : l : t : [dzenColor "gray" "" ts]
    -- (2)
    ppExtras          = [ windowCount ],
    ppOrder           = \(ws:l:t:ts:ex) -> [ws,l,"[",dzenColor (ansi M.! "red") "" ts,"]",t] ++ ex ++ []
  }

-- ansi

myXmobarLogHookAnsi1 :: Handle -> X()
myXmobarLogHookAnsi1 h = dynamicLogWithPP myXmobarLogHookAnsiPP
  {
    ppOutput = hPutStrLn h
  }

myXmobarLogHookAnsi2a :: Handle -> ScreenId -> PP
myXmobarLogHookAnsi2a h s = myXmobarLogHookAnsiPP
  {
    ppOutput = hPutStrLn h
  }

myXmobarLogHookAnsi2 :: [Handle] -> ScreenId -> X ()
myXmobarLogHookAnsi2 hs ns = mapM_ dynamicLogWithPP $ zipWith myXmobarLogHookAnsi2a hs [0..ns-1]

myDzen2LogHookAnsi1 :: Handle -> X()
myDzen2LogHookAnsi1 h = dynamicLogWithPP myDzen2LogHookAnsiPP
  {
    ppOutput = hPutStrLn h
  }

myDzen2LogHookAnsi2a :: Handle -> ScreenId -> PP
myDzen2LogHookAnsi2a h s = myDzen2LogHookAnsiPP
  {
    ppOutput = hPutStrLn h
  }

myDzen2LogHookAnsi2 :: [Handle] -> ScreenId -> X ()
myDzen2LogHookAnsi2 hs ns = mapM_ dynamicLogWithPP $ zipWith myDzen2LogHookAnsi2a hs [0..ns-1]

-------------------------------------------------------------------------------
-- Bindings
-------------------------------------------------------------------------------

myKeysDmenuCommandAnsi =
  [
    ((mod1Mask,                  xK_p      ), spawn dmenuCommandAnsi), -- theme: ansi
    ((0,                         xK_Menu   ), spawn rofiCommand)  -- theme: ansi
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
    ((mod1Mask,                  xK_f      ), sendMessage $ JumpToLayout "full'"),
    ((mod1Mask,                  xK_t      ), sendMessage $ JumpToLayout "tiled'"),
    ((mod1Mask,                  xK_a      ), sendMessage Shrink), -- shrink resizable area
    ((mod1Mask,                  xK_z      ), sendMessage Expand), -- expand resizable area
    ((mod1Mask,                  xK_i      ), sendMessage (IncMasterN 1)),    -- Increment the number of windows in the master area
    ((mod1Mask,                  xK_d      ), sendMessage (IncMasterN (-1))), -- Deincrement the number of windows in the master area
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
    ((mod1Mask .|. shiftMask,    xK_bracketleft),   DO.moveTo Prev hiddenWS), -- previous non empty workspace
    ((mod1Mask .|. shiftMask,    xK_bracketright),  DO.moveTo Next hiddenWS), -- previous non empty workspace
    ((mod1Mask .|. controlMask,  xK_Left),   DO.moveTo Prev hiddenWS), -- previous non empty workspace
    ((mod1Mask .|. controlMask,  xK_Right),  DO.moveTo Next hiddenWS), -- previous non empty workspace
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
  -- 
  [ ((m .|. mod1Mask, k), windows $ f i) -- Replace 'mod1Mask' with your mod key of choice.
    | (i, k) <- zip myWorkspaces ([xK_1 .. xK_9] ++ [ xK_0 ])
    -- , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)] -- default (greedyView)
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)] -- view
  ]
  ++
  -- (2) Reorder screens
  -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
  -- 
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
      clickJustFocuses     = myClickJustFocuses,
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
      -- logHook              = myXmobarLogHookAnsi1 xmobar
      -- (2) multiple xmobar
      logHook              = updatePointer (0.5, 0.5) (0, 0) >> myXmobarLogHookAnsi2 xmobar nScreens
      -- (3) multiple dzen2
      -- logHook              = updatePointer (0.5, 0.5) (0, 0) >> myDzen2LogHookAnsi2 xmobar nScreens
    } `additionalKeys` myKeysDmenuCommandAnsi


-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
  -- (1) single xmobar
  -- xmobar1 <- spawnPipe xmobarCommand1
  -- xmonad $ myConfigAnsi xmobar1 1 -- theme: ansi
  --
  -- (2) multiple xmobar
  -- -- kill <- mapM_ spawn ["killall -s 9 trayer", "killall -s 9 xmobar", "killall -s 9 conky"]
  nScreens <- countScreens
  xmobar2  <- mapM (spawnPipe . xmobarCommand2) [0 .. (nScreens - 1)]
  xmonad $ myConfigAnsi xmobar2 nScreens -- theme: ansi
  --
  -- (3) multiple dzen2
  -- -- kill <- mapM_ spawn ["killall -s 9 trayer", "killall -s 9 dzen2", "killall -s 9 conky"]
  -- nScreens <- countScreens
  -- dzen2  <- mapM (spawnPipe . dzenCommand2) [0 .. (nScreens - 1)]
  -- xmonad $ myConfigAnsi dzen2 nScreens -- theme: ansi

-------------------------------------------------------------------------------
-- end
-------------------------------------------------------------------------------
