-- xmonad.hs
-- Last update: 2017-12-05 08:13:51 (CET)

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Config
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicHooks
import XMonad.Hooks.DynamicLog
import XMonad.Layout.Circle
import XMonad.Layout.Decoration
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Util.EZConfig
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce
import Graphics.X11.ExtraTypes.XF86
import System.IO
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import qualified XMonad.Actions.FlexibleResize as Flex


myTerminal           = "urxvt"
myModMask            = mod1Mask
myFocusFollowsMouse  = False
myBorderWidth        = 1
myNormalBorderColor  = "#ffffff"
myFocusedBorderColor = "#0088CC"


xmobarEscape = concatMap doubleLts
  where doubleLts '<' = "<<"
        doubleLts x   = [x]

myWorkspaces :: [String]
myWorkspaces = clickable . (map xmobarEscape) $ ["1","2","3","4","5","6","7","8","9"]
  where
    clickable l = [ "<action=xdotool key alt+" ++ show (n) ++ ">" ++ ws ++ "</action>" | (i,ws) <- zip [1..9] l, let n = i ]


myStartUp :: X()
myStartUp = do
  -- spawnOnce "feh --bg-scale ~/wallpapers/gray/Minimalistic_gray_colors_2560x1600.jpg"
  -- spawnOnce "setxkbmap -model pc105 -option 'eurosign:e,lv3:ralt_switch:ctrl:nocaps' 'hr(us)'"
  spawn "$HOME/.xmonad/screen_toggle.sh -x"
  spawn "$HOME/.xmonad/trayer.sh"


myTabConfig :: Theme
myTabConfig = def
  {
    activeColor = "#0088CC",
    activeTextColor = "#ffffff",
    activeBorderColor = "#000000",
    inactiveColor = "#5f676a",  -- "#333333",
    inactiveTextColor = "#dddddd", -- "#888888",
    inactiveBorderColor = "#000000",
    urgentColor = "#900000",
    urgentTextColor = "#ffffff",
    urgentBorderColor = "#2f343a",
    -- fontName = "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
    -- fontName = "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-2"
    -- fontName = "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso10646-1"
    -- fontName = "xft:Monospace:pixelsize=12:antialias=true:style=bold"
    -- fontName = "xft:Monospace:pixelsize=13:antialias=true:style=bold"
    fontName = "xft:Monospace:pixelsize=14:antialias=true:style=bold"
    -- fontName = "xft:Terminus:pixelsize=12:antialias=true:style=bold"
    -- fontName = "xft:Terminus:pixelsize=13:antialias=true:style=bold"
    -- fontName = "xft:Terminus:pixelsize=14:antialias=true:style=bold"
  }

myLayoutHook1 = avoidStruts $ layoutHook def

myLayoutHook2 = avoidStruts
  $ mkToggle (single FULL)
  $ myLayouts
  where
    myLayouts = tab2' ||| tiled' ||| mirror' ||| threecol' ||| full'
    -- tab1'     = tabbed shrinkText myTabConfig
    tab2'     = tabbedAlways shrinkText myTabConfig
    tiled'    = Tall nmaster delta ratio
    mirror'   = Mirror tiled'
    threecol' = ThreeColMid nmaster delta ratio
    full'     = Full
    -- The default number of windows in the master pane
    nmaster  = 1
    -- Default proportion of screen occupied by master pane
    ratio    = 1/2
    -- Percent of screen to increment by when resizing panes
    delta    = 2/100


myManageHook :: ManageHook
myManageHook = composeAll . concat $
    [ [isDialog --> doFloat],
      -- [isFullscreen --> doFullFloat],
      [isFullscreen --> (doF W.focusDown <+> doFullFloat)],
      [className =? c --> doFloat | c <- myCFloats],
      [title =? t --> doFloat | t <- myTFloats],
      [resource =? r --> doFloat | r <- myRFloats],
      [resource =? i --> doIgnore | i <- myIgnores]
    ]
    where
    myCFloats = ["Nvidia-settings", "Sysinfo", "XCalc", "XFontSel", "Xmessage"]
    myTFloats = ["Downloads", "Search Engines", "Autofill Options", "Rename File", "Copying files", "Moving files", "Save As...",
                 "Preferences", "File Properties", "Replace", "Clear Private Data",
                 "Iceweasel Preferences", "Firefox Preferences"]
    myRFloats = ["buddy_list", "ticker", "gimp-toolbox", "gimp-dock", "gimp-image-window"]
    myIgnores = ["desktop", "desktop_window", "kdesktop", "Dialog"]


myLogHook1 :: Handle -> X()
myLogHook1 h = dynamicLogWithPP xmobarPP
  { ppOutput          = hPutStrLn h,
    ppCurrent         = xmobarColor "green" "" . wrap "[" "]",
    ppHidden          = xmobarColor "#ffffff" "",
    ppHiddenNoWindows = xmobarColor "#8c8c8c" "",
    ppTitle           = xmobarColor "green" "" . shorten 0,
    ppVisible         = wrap "(" ")",
    ppUrgent          = xmobarColor "red" "yellow",
    ppLayout          = xmobarColor "#dddddd" "",
    ppSep             = "  ", -- separator between each object
    ppWsSep           = " " -- separator between workspaces
  }

myLogHook2 :: Handle -> X ()
myLogHook2 h = dynamicLogWithPP $ def
  {
    ppOutput          =   hPutStrLn h
  }

myKeys =
  [
    ((mod1Mask,                  xK_u      ), spawn "urxvt"),
    ((mod1Mask,                  xK_Return ), spawn "urxvt"),
    ((mod1Mask,                  xK_d      ), spawn "/usr/bin/dmenu_run -i -p 'Run: '"),
    ((0,                         xK_Menu   ), spawn "/usr/bin/dmenu_run -i -p 'Run: '"),
    ((mod1Mask,                  xK_F4     ), kill),
    ((0,                         xK_Print  ), spawn "scrot ~/screenshot_$(date +%Y%m%d.%H%M%S).jpg"),
    --
    ((mod1Mask,                  xK_j      ), windows W.focusUp), -- switch to previous workspace
    ((mod1Mask,                  xK_k      ), windows W.focusDown), -- switch to next workspace
    ((mod1Mask .|. shiftMask,    xK_j      ), windows W.swapUp),  -- swap the focused window with the previou
    ((mod1Mask .|. shiftMask,    xK_k      ), windows W.swapDown), -- swap the focused window with the next window
    ((mod1Mask,                  xK_Down   ), nextScreen), -- cycling through screens
    ((mod1Mask,                  xK_Up     ), prevScreen), -- cycling through screens
    ((mod1Mask .|. shiftMask,    xK_Down   ), swapNextScreen), -- cycling through screens
    ((mod1Mask .|. shiftMask,    xK_Up     ), swapPrevScreen), -- cycling through screens
    ((mod1Mask,                  xK_q      ), spawn "$HOME/.xmonad/recompile.sh"),
    ((mod1Mask .|. shiftMask,    xK_slash  ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -")),
    ((mod1Mask,                  xK_f      ), sendMessage $ Toggle FULL),
    --
    ((0, xF86XK_AudioLowerVolume           ), spawn "amixer -q set Master,0 5%- unmute"),
    ((0, xF86XK_AudioRaiseVolume           ), spawn "amixer -q set Master,0 5%+ unmute"),
    ((0, xF86XK_AudioMute                  ), spawn "amixer -q set Master,0 toggle"),
    ((0, xF86XK_MonBrightnessUp            ), spawn "/usr/bin/xbacklight -inc 10"),
    ((0, xF86XK_MonBrightnessDown          ), spawn "/usr/bin/xbacklight -dec 10"),
    --
    ((mod1Mask .|. controlMask,  xK_c      ), spawn "$HOME/bin/chromium.sh noproxy"),
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
  [ ((m .|. mod1Mask, k), windows $ f i) -- Replace 'mod1Mask' with your mod key of choice.
    | (i, k) <- zip myWorkspaces [xK_1 .. xK_9]
    -- , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)] -- default (greedyView)
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
  ]
  ++
  -- Screens are in wrong order
  [ ((m .|. mod1Mask, key), screenWorkspace sc >>= flip whenJust (windows . f)) -- Replace 'mod1Mask' with your mod key of choice.
    | (key, sc) <- zip [xK_w, xK_e, xK_r] [0,2,1] -- was [0..] *** change to match your screen order ***
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
  ]


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


help :: String
help = unlines
  [
    "The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter      Launch xterminal",
    "mod-Enter            Launch xterminal",
    "mod-p                Launch dmenu",
    "mod-d                Launch dmenu",
    "mod-Shift-c          Close/kill the focused window",
    "mod-F4               Close/kill the focused window",
    "mod-Space            Rotate through the available layout algorithms",
    "mod-Shift-Space      Reset the layouts on the current workSpace to default",
    "mod-n                Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab              Move focus to the next window",
    "mod-Shift-Tab        Move focus to the previous window",
    "mod-j                Move focus to the next window",
    "mod-k                Move focus to the previous window",
    "mod-m                Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Shift-j          Swap the focused window with the next window",
    "mod-Shift-k          Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h                Shrink the master area",
    "mod-l                Expand the master area",
    "",
    "-- floating layer support",
    "mod-t                Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q          Quit xmonad",
    "mod-q                Restart xmonad",
    "mod-[0/~,1..9]       Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[0/~,1..9] Move client to workspace N",
    "mod-{w,e,r}          Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}    Move client to screen 1, 2, or 3",
    "mod-PageUp           Prev Screen",
    "mod-PageDown         Next Screen",
    "mod-Shift-PageUp     Swap Prev Screen",
    "mod-Shift-PageDown   Swap Next Screen",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1          Set the window to floating mode and move by dragging",
    "mod-button2          Raise the window to the top of the stack",
    "mod-button3          Set the window to floating mode and resize by dragging"
  ]


main :: IO ()
main = do
  xmobar1 <- spawnPipe "xmobar $HOME/.xmonad/xmobar.hs"
  -- dzen1 <- spawnPipe "dzen2 -x '1440' -y '0' -h '24' -w '640' -ta 'l' -fg '#FFFFFF' -bg '#1B1D1E'"
  -- trayer1 <- spawnPipe "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --transparent true --tint 0x191999 --height 12"
  -- trayer2 <- spawnPipe "~/.xmonad/trayer.sh"
  --
  xmonad $ def
    { terminal             = myTerminal,
      modMask              = myModMask,
      focusFollowsMouse    = myFocusFollowsMouse,
      borderWidth          = myBorderWidth,
      normalBorderColor    = myNormalBorderColor,
      focusedBorderColor   = myFocusedBorderColor,
      --
      workspaces           = myWorkspaces,
      startupHook          = myStartUp,
      layoutHook           = myLayoutHook2,
      manageHook           = myManageHook <+> manageDocks <+> dynamicMasterHook,
      logHook              = myLogHook1 xmobar1,
      handleEventHook      = handleEventHook def <+> docksEventHook
    } `additionalKeys` myKeys
      `additionalMouseBindings` myMouse

-- end
