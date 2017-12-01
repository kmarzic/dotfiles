-- xmobar.hs
-- Last update: 2017-12-01 08:01:14 (CET)

Config {
  -- font = "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1",
  -- font = "xft:Monospace:pixelsize=13:antialias=true:style=bold",
  font = "xft:Terminus:pixelsize=14:antialias=true:style=bold",
  additionalFonts = [],
  borderColor = "black",
  border = TopB,
  bgColor = "#222222",
  fgColor = "#ffffff",
  alpha = 255,
  position = Bottom,
  textOffset = -1,
  iconOffset = -1,
  lowerOnStart = True,     -- send to bottom of window stack on start
  pickBroadest = False,    -- choose widest display (multi-monitor)
  persistent = False,      -- enable/disable hiding (True = disabled)
  hideOnStart = False,     -- start with window unmapped (hidden)
  iconRoot = ".",
  allDesktops = True,      -- show on all desktops
  overrideRedirect = True, -- set the Override Redirect flag (Xlib)

  commands =
    [ Run Weather "LDZA" [ "--template" ,"<station>: <tempC>°C <rh>% <pressure> hPa",
                           "--Low"      , "18",
                           "--High"     , "25",
                           "--normal"   , "green",
                           "--high"     , "red",
                           "--low"      , "cyan"
                         ] 36000,

      Run DynNetwork     [ "--template" , "<dev>: <tx>kB/s|<rx>kB/s",
                           "--Low"      , "1000", -- units: kB/s
                           "--High"     , "5000", -- units: kB/s
                           "--low"      , "green",
                           "--normal"   , "orange",
                           "--high"     , "red"
                         ] 10,

      Run Battery        [ "--template" , "Batt: <acstatus>",
                           "--Low"      , "10",
                           "--High"     , "80",
                           "--low"      , "red",
                           "--normal"   , "orange",
                           "--high"     , "green",

                           "--", -- battery specific options
                           -- discharging status
                           "-o"   , "<left>% (<timeleft>)",
                           -- AC "on" status
                           "-O"   , "<fc=orange>Charging</fc> <left>% (<timeleft>)",
                           -- charged status
                           "-i"   , "<fc=green>Charged</fc>"
                         ] 50,

      Run MultiCpu       [ "--template" , "CPU: <total0>%|<total1>%",
                           "--Low"      , "50",
                           "--High"     , "85",
                           "--low"      , "green",
                           "--normal"   , "orange",
                           "--high"     , "red"
                         ] 10,

      Run Cpu            [ "--Low"      , "3",
                           "--High"     , "50",
                           "--normal"   , "green",
                           "--high"     , "red"
                         ] 10,

      Run CoreTemp       [ "--template" , "Temp: <core0>°C|<core1>°C",
                           "--Low"      , "70",
                           "--High"     , "80",
                           "--low"      , "green",
                           "--normal"   , "orange",
                           "--high"     , "red"
                         ] 50,

      Run Memory         [ "--template" , "Mem: <usedratio>%",
                           "--Low"      , "20",
                           "--High"     , "90",
                           "--low"      , "green",
                           "--normal"   , "orange",
                           "--high"     , "red"
                         ] 10,

      Run DiskU          [ ("/", "/: <used> <size>"),
                           -- ("/data0", "/data0 <used> <free> <size>")
                           ("sda1", "<freebar>")
                         ]
                         [ "--Low"      , "20",
                           "--High"     , "50",
                           "--minwidth" , "1",
                           "--pad"      , "3"
                         ] 50,

      Run Swap           [ "--template" ,"Swap: <usedratio>%",
                           "--Low"      , "20",
                           "--High"     , "90",
                           "--low"      , "green",
                           "--normal"   , "orange",
                           "--high"     , "red"
                         ] 10,

      Run Date           "<fc=green>%a %Y-%m-%d %H:%M:%S</fc>" "date" 10,
      Run Com            "uname" ["-s","-r"] "uname1" 36000,
      Run Com            ".xmonad/spaces.sh" ["spaces", "6"] "spaces1" 300,
      Run Com            ".xmonad/wireless.sh" [] "wireless1" 300,
      Run UnsafeStdinReader
    ],
    sepChar = "%",
    alignSep = "}{",

    -- template = "%UnsafeStdinReader% | %multicpu% | %coretemp% | %memory% * %swap% | %disku% | %dynnetwork% | %battery% }{ %LDZA% | %date%"
    -- template = "%UnsafeStdinReader% | %multicpu% | %coretemp% | %memory% %swap% | %disku% | %dynnetwork% <fc=green>%wireless1%</fc> | %battery% }{ %date% | %spaces1%"
    template = "%UnsafeStdinReader% | %multicpu% | %coretemp% | %memory% %swap% | %dynnetwork% <fc=green>%wireless1%</fc> | %battery% }{ %date% | %spaces1%"
}

-- end
