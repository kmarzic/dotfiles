-------------------------------------------------------------------------------
-- xmonad.hs
-- Last update: 2022-12-17 15:57:51 (CET)
-------------------------------------------------------------------------------

-- Base
import XMonad hiding ( (|||) )
import XMonad.Config
import XMonad.Config.Desktop
import System.IO
import System.Environment (getEnv)
import System.IO.Unsafe
import qualified XMonad.StackSet as W
import Graphics.X11.ExtraTypes.XF86

-- Actions
import XMonad.Actions.CycleWS
import XMonad.Actions.Minimize (minimizeWindow, withLastMinimized, maximizeWindowAndFocus)
import XMonad.Actions.UpdatePointer
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
import qualified XMonad.Actions.FlexibleResize as Flex

-- Data
import Data.Maybe ( maybeToList )
import Data.List ( (\\) )
import qualified Data.Map as M

-- Hooks
import XMonad.Hooks.DynamicHooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

-- Layouts
import XMonad.Layout hiding ( (|||) )
import XMonad.Layout.Column
import XMonad.Layout.Gaps
import XMonad.Layout.Groups.Helpers
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.IndependentScreens
import XMonad.Layout.Minimize(minimize)
import XMonad.Layout.Renamed
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Roledex
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ToggleLayouts
import qualified XMonad.Layout.Groups as G
import qualified XMonad.Layout.Groups.Helpers as Group

-- Utilities
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Util.NamedWindows (getName)


-------------------------------------------------------------------------------
-- Config
-------------------------------------------------------------------------------

monokai :: M.Map String String
monokai = M.fromList
  [
    ("blue",             "#0088cc"),
    ("cyan",             "#00ffff"),
    ("white",            "#ffffff")
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
    "mod-t                Toggle tiled screen",
    "mod-x                Toggle roledex screen",
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
-- fontRegular = "monospace:size=10:antialias=true:style=regular"
fontRegular = "DejaVuSansMono\\ Nerd\\ Font:size=10:antialias=true:autohint=true:style=regular"

fontBold :: String
-- fontBold = "monospace:size=10:antialias=true:style=bold"
fontBold = "DejaVuSansMono\\ Nerd\\ Font:size=10:antialias=true:autohint=true:style=bold"

fontTerminalScratchpad :: String
-- fontTerminalScratchpad = "monospace:size=10:antialias=true:style=bold,Source\\ Code\\ Pro\\ Medium:size=10:antialias=true:hinting=true:style:bold"
fontTerminalScratchpad = "DejaVu\\ Sans\\ Mono:size=9:antialias=true:autohint=true:style=regular,DejaVuSansMono\\ Nerd\\ Font:size=9:antialias=true:autohint=true:style=regular"

dmenuCommandMonokai :: String -- theme: monokai
-- dmenuCommandMonokai = "/usr/bin/dmenu_run -i -nf \"#00ffff\" -nb \"#101010\" -sb \"#00ffff\" -sf \"#101010\" -fn " ++ fontRegular ++ " -p 'Run: '"
dmenuCommandMonokai = "$HOME/bin/dmenu_run -i -nf \"#00ffff\" -nb \"#101010\" -sb \"#00ffff\" -sf \"#101010\" -fn " ++ fontRegular ++ " -p 'Run: '"

rofiCommand :: String
rofiCommand = "rofi -show run"

xmobarCommand1 :: String
xmobarCommand1 = "xmobar $HOME/.config/xmonad/xmobar.hs"

xmobarCommand2 :: ScreenId -> String
xmobarCommand2 (S s) = unwords ["xmobar", "-x", show s, "$HOME/.config/xmonad/xmobar.hs"]

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
myTerminalScratchpad = "$HOME/bin/st -n scratchpad -f" ++ fontTerminalScratchpad ++ " &"
-- myTerminalScratchpad = "gnome-terminal &"

myModMask :: KeyMask
-- myModMask = mod1Mask
myModMask = mod4Mask

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True
-- myFocusFollowsMouse = False

myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth :: Dimension
myBorderWidth = 1
-- myBorderWidth = 2

myNormalBorderColorMonokai :: String -- theme: monokai
myNormalBorderColorMonokai = monokai M.! "white"
myFocusedBorderColorMonokai :: String
myFocusedBorderColorMonokai = monokai M.! "cyan"

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
  return ()

myStartUp :: X()
myStartUp = do
  -- spawnOnce "feh --bg-scale ~/wallpapers/green/lines_spots_color_texture_50390_3840x2400.jpg"
  -- spawnOnce "setxkbmap -model pc105 -option 'eurosign:e,lv3:ralt_switch,compose:nocaps' 'hr(us)'"
  -- spawnOnce "dunst -config $HOME/.config/dunst/dunstrc"
  spawnOnce "$HOME/bin/screen.toogle.sh -x"
  spawnOnce "$HOME/bin/trayer.sh"
  setWMName "LG3D"


-------------------------------------------------------------------------------
-- Window Rules
-------------------------------------------------------------------------------

myManageScratchPad :: [NamedScratchpad]
myManageScratchPad =
  [
    NS "term" myTerminalScratchpad findTerm manageTerm
  ]
  where
    findTerm   = (resource =? "scratchpad")
    manageTerm = customFloating $ W.RationalRect l t w h
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
    [className =? "firefox" --> doShift (myWorkspaces !! 4)],
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
    [className =? "Oracle VM VirtualBox Manager" --> doShift (myWorkspaces !! 7)],
    [className =? "VirtualBox Macine" --> doShift (myWorkspaces !! 7)],
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

myLayoutHook tabConfig =
  avoidStruts
  $ (flip G.group) (Full)
  $ full' ||| tab' ||| tiled' ||| mirror' ||| roledex'
  where
    -- tab'       = named "tab'" (spacingRaw True (Border 2 2 2 2) True (Border 2 2 2 2) True $ tabbedAlways shrinkText tabConfig)
    tab'       = renamed [Replace "tabbed"] $ ( spacingRaw True (Border 2 2 2 2) True (Border 2 2 2 2) True $ tabbedAlways shrinkText tabConfig )
    --
    -- tiled'     = named "tiled'" (spacingRaw True (Border 2 2 2 2) True (Border 2 2 2 2) True $ Tall nmaster0 delta0 ratio0)
    tiled'     = renamed [Replace "tall"] $ ( spacingRaw True (Border 2 2 2 2) True (Border 2 2 2 2) True $ Tall nmaster0 delta0 ratio0 )
    --
    -- mirror'    = named "mirror'" (Mirror tiled')
    mirror'     = renamed [Replace "wide"] $ ( Mirror tiled' )
    --
    -- threecol'  = named "threecol'" (ThreeColMid nmaster0 delta0 ratio0)
    --
    -- full'      = named "full'" (gaps1 $ Full)
    full'      = renamed [Replace "full"] $ ( gaps1 $ Full )
    --
    -- resizetab' = named "resizetab'" (ResizableTall 1 (3/100) (1/2) [])
    --
    -- roledex'   = named "roledex'" (Roledex)
    roledex'   = renamed [Replace "roledex"] $ Roledex
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
   -- "tab' by Full"            -> "[_]"
   -- "full' by Full"           -> "[ ]"
   -- "tiled' by Full"          -> "[|]"
   -- "mirror' by Full"         -> "[-]"
   -- "roledex' by Full"        -> "[@]"
   -- _                         -> layout
   --
   -- (2) icon layout
   "tab' by Full"            -> myIcon ".config/xmonad/icons/layout_tabbed.xbm"
   "full' by Full"           -> myIcon ".config/xmonad/icons/layout_full.xbm"
   "tiled' by Full"          -> myIcon ".config/xmonad/icons/layout_tall.xbm"
   "mirror' by Full"         -> myIcon ".config/xmonad/icons/layout_mirror.xbm"
   "roledex' by Full"        -> myIcon ".config/xmonad/icons/layout_roledex.xbm"
   _                         -> layout

logTitles :: X (Maybe String) -- this is a Logger
logTitles =
   withWindowSet $ fmap (Just . unwords) -- fuse window names
   . traverse (fmap show . getName) -- show window names
   . (\ws -> W.index ws \\ maybeToList (W.peek ws)) -- all windows except the focused (may be slow)

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myXmobarLogHookMonokaiPP :: PP -- theme: monokai
myXmobarLogHookMonokaiPP = def
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

myDzen2LogHookMonokaiPP :: PP -- theme: monokai
myDzen2LogHookMonokaiPP = def
  {
    ppCurrent         = dzenColor "cyan" "" . wrap "[" "]",
    ppHidden          = dzenColor "#ffffff" "",
    ppHiddenNoWindows = dzenColor "#999999" "",
    ppTitle           = dzenColor "cyan" "" . shorten 50,
    ppVisible         = wrap "(" ")",
    ppUrgent          = dzenColor "red" "yellow",
    ppLayout          = dzenColor "green" "" . (\layout -> myPPLayout (layout)),
    ppSep             = " ", -- separator between each object
    ppWsSep           = " ", -- separator between workspaces
    -- (1)
    -- ppExtras          = [ logTitles ],
    -- ppOrder           = \(ws:l:t:ts:_) -> ws : l : t : [dzenColor "gray" "" ts]
    -- (2)
    ppExtras          = [ windowCount ],
    ppOrder           = \(ws:l:t:ts:ex) -> [ws,l,"[",dzenColor "red" "" ts,"]",t] ++ ex ++ []
  }

myXmobarLogHookMonokai1 :: Handle -> X()
myXmobarLogHookMonokai1 h = dynamicLogWithPP myXmobarLogHookMonokaiPP
  {
    ppOutput = hPutStrLn h
  }

myXmobarLogHookMonokai2a :: Handle -> ScreenId -> PP
myXmobarLogHookMonokai2a h s = myXmobarLogHookMonokaiPP
  {
    ppOutput = hPutStrLn h
  }

myXmobarLogHookMonokai2 :: [Handle] -> ScreenId -> X ()
myXmobarLogHookMonokai2 hs ns = mapM_ dynamicLogWithPP $ zipWith myXmobarLogHookMonokai2a hs [0..ns-1]

myDzen2LogHookMonokai1 :: Handle -> X()
myDzen2LogHookMonokai1 h = dynamicLogWithPP myDzen2LogHookMonokaiPP
  {
    ppOutput = hPutStrLn h
  }

myDzen2LogHookMonokai2a :: Handle -> ScreenId -> PP
myDzen2LogHookMonokai2a h s = myDzen2LogHookMonokaiPP
  {
    ppOutput = hPutStrLn h
  }

myDzen2LogHookMonokai2 :: [Handle] -> ScreenId -> X ()
myDzen2LogHookMonokai2 hs ns = mapM_ dynamicLogWithPP $ zipWith myDzen2LogHookMonokai2a hs [0..ns-1]


-------------------------------------------------------------------------------
-- Bindings
-------------------------------------------------------------------------------

myKeysDmenuCommandMonokai =
  [
    ((myModMask,                 xK_p      ), spawn dmenuCommandMonokai), -- theme: monokai
    ((0,                         xK_Menu   ), spawn rofiCommand)  -- theme: monokai
  ]

myKeys :: [((KeyMask, KeySym), X ())]
myKeys =
  [
    ((myModMask,                 xK_Return ), spawn myTerminal),
    ((myModMask,                 xK_s      ), namedScratchpadAction myManageScratchPad "term"),
    ((myModMask,                 xK_F4     ), kill),
    -- ((myModMask,                 xK_m      ), myStartUpScreen),
    ((0,                         xK_Print  ), spawn "scrot ~/screenshot_$(date +%Y%m%d.%H%M%S).jpg"),
    ((myModMask,                 xK_Print  ), spawn "$HOME/bin/screenshot.sh"),
    ((myModMask,                 xK_q      ), spawn "$HOME/.config/xmonad/recompile.sh"),
    ((myModMask .|. shiftMask,   xK_q      ), spawn "$HOME/bin/exit.sh message"),
    ((myModMask .|. shiftMask,   xK_slash  ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -")),
    --
    ((myModMask,                 xK_f      ), sendMessage $ JumpToLayout "full'"),
    ((myModMask,                 xK_t      ), sendMessage $ JumpToLayout "tiled'"),
    ((myModMask,                 xK_x      ), sendMessage $ JumpToLayout "roledex'"),
    ((myModMask,                 xK_a      ), sendMessage Shrink), -- shrink resizable area
    ((myModMask,                 xK_z      ), sendMessage Expand), -- expand resizable area
    ((myModMask,                 xK_i      ), sendMessage (IncMasterN 1)),    -- Increment the number of windows in the master area
    ((myModMask,                 xK_d      ), sendMessage (IncMasterN (-1))), -- Deincrement the number of windows in the master area
    --
    -- ((myModMask,                 xK_j      ), windows W.focusUp), -- switch to previous workspace
    -- ((myModMask,                 xK_k      ), windows W.focusDown), -- switch to next workspace
    ((myModMask,                 xK_j      ), Group.focusUp), -- switch to previous workspace
    ((myModMask,                 xK_k      ), Group.focusDown), -- switch to next workspace
    -- ((myModMask .|. shiftMask,   xK_j      ), windows W.swapUp),  -- swap the focused window with the previous window
    -- ((myModMask .|. shiftMask,   xK_k      ), windows W.swapDown), -- swap the focused window with the next window
    ((myModMask .|. shiftMask,   xK_j      ), Group.swapUp >> refresh),  -- swap the focused window with the previous window
    ((myModMask .|. shiftMask,   xK_k      ), Group.swapDown >> refresh), -- swap the focused window with the next window
    -- ((myModMask,                 xK_Down   ), nextScreen), -- cycling through screens
    -- ((myModMask,                 xK_Up     ), prevScreen), -- cycling through screens
    ((myModMask,                 xK_comma  ), prevScreen), -- previous screen
    ((myModMask,                 xK_period ), nextScreen), -- next screen
    ((myModMask .|. shiftMask,   xK_comma  ), shiftPrevScreen >> prevScreen), -- shift to previous screen
    ((myModMask .|. shiftMask,   xK_period ), shiftNextScreen >> nextScreen), -- shift to next screen
    -- ((myModMask .|. shiftMask,    xK_Down   ), swapNextScreen), -- cycling through screens
    -- ((myModMask .|. shiftMask,    xK_Up     ), swapPrevScreen), -- cycling through screens
    ((myModMask,                  xK_h     ), Group.focusGroupUp), -- move the focus to the previous group
    ((myModMask,                  xK_l     ), Group.focusGroupDown), -- move the focus to the next group
    ((myModMask .|. shiftMask,    xK_h     ), Group.moveToGroupUp False), -- move the focused window to the previous group
    ((myModMask .|. shiftMask,    xK_l     ), Group.moveToGroupDown False), -- move the focused window to the next group
    ((myModMask,                  xK_bracketleft), prevWS), -- previous workspace
    ((myModMask,                  xK_bracketright), nextWS), -- next workspace
    ((myModMask .|. shiftMask,    xK_Left), prevWS), -- previous workspace
    ((myModMask .|. shiftMask,    xK_Right), nextWS), -- next workspace
    ((myModMask .|. shiftMask,    xK_bracketleft),   DO.moveTo Prev hiddenWS), -- previous non empty workspace
    ((myModMask .|. shiftMask,    xK_bracketright),  DO.moveTo Next hiddenWS), -- previous non empty workspace
    ((myModMask .|. controlMask,  xK_Left),   DO.moveTo Prev hiddenWS), -- previous non empty workspace
    ((myModMask .|. controlMask,  xK_Right),  DO.moveTo Next hiddenWS), -- previous non empty workspace
    --
    ((myModMask,                  xK_m     ), withFocused minimizeWindow <+> windows W.focusDown),
    ((myModMask .|. shiftMask,    xK_m     ), withLastMinimized maximizeWindowAndFocus),
    --
    ((0, xF86XK_AudioLowerVolume           ), spawn "amixer -q set Master,0 5%- unmute"),
    ((0, xF86XK_AudioRaiseVolume           ), spawn "amixer -q set Master,0 5%+ unmute"),
    ((0, xF86XK_AudioMute                  ), spawn "amixer -q set Master,0 toggle"),
    ((0, xF86XK_MonBrightnessUp            ), spawn "$HOME/.config/xmonad/brigtness.sh inc 10"),
    ((0, xF86XK_MonBrightnessDown          ), spawn "$HOME/.config/xmonad/brigtness.sh dec 10"),
    ((0, xF86XK_ModeLock                   ), spawn "$HOME/bin/exit.sh lock"),
    ((0, xF86XK_Mail                       ), spawn "evolution"),
    ((0, xF86XK_WWW                        ), spawn "$HOME/bin/vivaldi.sh noproxy"),
    ((0, xF86XK_Terminal                   ), spawn myTerminal),
    --
    ((myModMask .|. controlMask,  xK_c     ), spawn "$HOME/bin/vivaldi.sh noproxy"),
    ((myModMask .|. controlMask,  xK_f     ), spawn "firefox"),
    ((myModMask .|. controlMask,  xK_o     ), spawn "$HOME/bin/opera.sh proxy"),
    ((myModMask .|. controlMask,  xK_g     ), spawn "gajim"),
    ((myModMask .|. controlMask,  xK_m     ), spawn "evolution"),
    ((myModMask .|. controlMask,  xK_p     ), spawn "pidgin"),
    ((myModMask .|. controlMask,  xK_s     ), spawn "$HOME/bin/skype"),
    ((myModMask .|. controlMask,  xK_t     ), spawn "thunderbird"),
    ((myModMask .|. controlMask,  xK_v     ), spawn "VirtualBox"),
    --
    ((myModMask .|. controlMask, xK_l      ), spawn "$HOME/bin/exit.sh lock"),
    ((myModMask .|. controlMask, xK_s      ), spawn "$HOME/bin/exit.sh monitor_off"),
    ((myModMask .|. controlMask, xK_m      ), spawn "$HOME/bin/screen.toogle.sh -x"),
    ((myModMask .|. controlMask, xK_x      ), spawn "$HOME/bin/exit.sh message")
  ]
  ++
  -- (1) Replacing greedyView with view
  -- mod-[1..9] %! Switch to workspace N
  -- mod-shift-[1..9] %! Move client to workspace N
  -- 
  [ ((m .|. myModMask, k), windows $ f i) -- Replace 'mod1Mask' with your mod key of choice.
    | (i, k) <- zip myWorkspaces ([xK_1 .. xK_9] ++ [ xK_0 ])
    -- , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)] -- default (greedyView)
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)] -- view
  ]
  ++
  -- (2) Reorder screens
  -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
  --
  [ ((m .|. myModMask, key), screenWorkspace sc >>= flip whenJust (windows . f)) -- Replace 'mod1Mask' with your mod key of choice.
    -- | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..] -- default map
    | (key, sc) <- zip [xK_w, xK_e, xK_r] [0,2,1] -- was [0..] *** change to match your screen order ***
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
  ]

myMouse =
  [
    ((myModMask, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)), -- Set the window to floating mode and move by dragging
    ((myModMask, button2), (\w -> focus w >> windows W.shiftMaster)),                      -- Raise the window to the top of the stack
    ((myModMask, button3), (\w -> focus w >> Flex.mouseResizeWindow w)),                   -- Set the window to floating mode and resize by dragging
    ((myModMask, button4), (\_ -> prevWS)),                                                -- Switch to previous workspace
    ((myModMask, button5), (\_ -> nextWS)),                                                -- Switch to next workspace
    ((myModMask .|. shiftMask, button4), (\_ -> shiftToPrev)),                             -- Send client to previous workspace
    ((myModMask .|. shiftMask, button5), (\_ -> shiftToNext))                              -- Send client to next workspace
  ]


-------------------------------------------------------------------------------
-- Configurations
-------------------------------------------------------------------------------

myConfigDefault = docks $ def
    {
      terminal             = myTerminal,
      modMask              = myModMask,
      focusFollowsMouse    = myFocusFollowsMouse,
      clickJustFocuses     = myClickJustFocuses,
      borderWidth          = myBorderWidth,
      workspaces           = myWorkspaces,
      startupHook          = myStartUp >> myStartUpScreen,
      manageHook           = myManageHook <+> manageDocks <+> dynamicMasterHook <+> namedScratchpadManageHook myManageScratchPad,
      -- handleEventHook      = handleEventHook def <+> docksEventHook
      handleEventHook      = handleEventHook def
    } `additionalKeys` myKeys
      `additionalMouseBindings` myMouse

myConfigMonokai xmobar nScreens = myConfigDefault -- theme: monokai
    {
      normalBorderColor    = myNormalBorderColorMonokai,
      focusedBorderColor   = myFocusedBorderColorMonokai,
      layoutHook           = myLayoutHook myTabConfigMonokai,
      -- (1) single xmobar
      -- logHook              = myXmobarLogHookMonokai xmobar
      -- (2) multiple xmobar
      logHook              = updatePointer (0.5, 0.5) (0, 0) >> myXmobarLogHookMonokai2 xmobar nScreens
      -- (3) multiple dzen2
      -- logHook              = updatePointer (0.5, 0.5) (0, 0) >> myDzen2LogHookMonokai2 xmobar nScreens
    } `additionalKeys` myKeysDmenuCommandMonokai


-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
  -- (1) single xmobar
  -- xmobar1 <- spawnPipe xmobarCommand1
  -- xmonad $ myConfigMonokai xmobar1 1 -- theme: monokai
  --
  -- (2) multiple xmobar
  -- -- kill <- mapM_ spawn ["killall -s 9 trayer", "killall -s 9 xmobar", "killall -s 9 conky"]
  nScreens <- countScreens
  xmobar2  <- mapM (spawnPipe . xmobarCommand2) [0 .. (nScreens - 1)]
  xmonad $ myConfigMonokai xmobar2 nScreens -- theme: monokai
  --
  -- (3) multiple dzen2
  -- -- kill <- mapM_ spawn ["killall -s 9 trayer", "killall -s 9 dzen2", "killall -s 9 conky"]
  -- nScreens <- countScreens
  -- dzen2  <- mapM (spawnPipe . dzenCommand2) [0 .. (nScreens - 1)]
  -- xmonad $ myConfigMonokai dzen2 nScreens -- theme: monokai

-------------------------------------------------------------------------------
-- end
-------------------------------------------------------------------------------
