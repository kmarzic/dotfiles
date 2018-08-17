-- xmobar.hs
-- Last update: 2018-08-17 10:10:02 (CEST)
--
-- https://archives.haskell.org/projects.haskell.org/xmobar/

Config {
   -- theme: blue
   -- bgColor = "#222222",
   -- fgColor = "#ffffff",
   -- theme: green
   -- bgColor = "#222222",
   -- fgColor = "#ffffff",
   -- theme: solarized dark
   -- bgColor = "#073642", -- base02
   -- fgColor = "#fdf6e3", -- base3
   -- theme: solarized light
   bgColor = "#eee8d5", -- base2
   fgColor = "#002b36", -- base03

   -- font = "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1",
   -- font = "xft:Monospace:pixelsize=14:antialias=true:style=bold",
   font = "xft:Monospace:pixelsize=13:antialias=true:style=bold",
   -- font = "xft:Monospace:pixelsize=12:antialias=true:style=bold",
   -- font = "xft:Terminus:pixelsize=14:antialias=true:style=bold",
   -- font = "xft:Terminus:pixelsize=13:antialias=true:style=bold",
   -- font = "xft:Terminus:pixelsize=12:antialias=true:style=bold",
   -- font = "xft:Dejavu Sans Mono:pixelsize=14:antialias=true:style=bold",
   -- font = "xft:Dejavu Sans Mono:pixelsize=13:antialias=true:style=bold",
   -- font = "xft:Dejavu Sans Mono:pixelsize=12:antialias=true:style=bold",
   -- font = "xft:Inconsolata:pixelsize=14:antialias=true:style=bold",
   -- font = "xft:Inconsolata:pixelsize=13:antialias=true:style=bold",
   -- font = "xft:Inconsolata:pixelsize=12:antialias=true:style=bold",
   -- font = "xft:Dec Terminal Bold:pixelsize=14:antialias=true:style=bold",
   -- font = "xft:Dec Terminal Bold:pixelsize=13:antialias=true:style=bold",
   -- font = "xft:Dec Terminal Bold:pixelsize=12:antialias=true:style=bold",
   -- font = "xft:Courier New Bold:pixelsize=12:antialias=true:style=bold",
   additionalFonts = [ "xft:FontAwesome:pixelsize=13" ],

   borderColor = "black",
   border = TopB,
   alpha = 255,
   -- alpha = 0,
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
     [ Run Weather "LDZA"    [ "--template" ,"<tempC>°C <fc=#859900><rh></fc>% <fc=#859900><pressure></fc>hPa",
                               "--Low"      , "18",
                               "--High"     , "25",
                               "--normal"   , "#859900", -- green
                               "--high"     , "#dc322f", -- red
                               "--low"      , "#2aa198" -- cyan
                             ] 36000, -- 1h

       Run DynNetwork        [ "--template" , "<dev>: <tx>kB/s <rx>kB/s",
                               "--Low"      , "10000", -- units: 10 kB/s
                               "--High"     , "500000", -- units: 500 kB/s
                               "--low"      , "#859900", -- green
                               "--normal"   , "#cb4b16", -- orange
                               "--high"     , "#dc322f", -- red
                               "--minwidth" , "4",
                               "-c"         , " "
                             ] 10, -- 1s

       Run Wireless "wlp1s0" [ "--template" , "<fc=#2aa198><essid>:</fc><quality>%", -- cyan
                               "--Low"      , "20",
                               "--High"     , "70",
                               "--high"     , "#859900", -- green
                               "--normal"   , "#cb4b16", -- orange
                               "--low"      , "#dc322f", -- red
                               "--minwidth" , "3",
                               "--nastring" , "No Wifi"
                             ] 10, -- 1s

       Run Battery           [ "--template" , "BAT: <acstatus>",
                               "--Low"      , "10",
                               "--High"     , "80",
                               "--low"      , "#dc322f", -- red
                               "--normal"   , "#cb4b16", -- orange
                               "--high"     , "#859900", -- green

                               "--", -- battery specific options
                               -- discharging status
                               "-o"   , "<left>% (<timeleft>)",
                               -- AC "on" status
                               "-O"   , "<fc=#cb4b16>c</fc> <left>% (<timeleft>)", -- orange
                               -- charged status
                               "-i"   , "<fc=#859900>f</fc>" -- green
                             ] 50, -- 5s

       Run MultiCpu          [ "--template" , "CPU: <total0>%|<total1>%|<total2>%|<total3>%",
                               "--Low"      , "50",
                               "--High"     , "85",
                               "--low"      , "#859900", -- green
                               "--normal"   , "#cb4b16", -- orange
                               "--high"     , "#dc322f", -- red
                               "--width"    , "2"
                             ] 10, -- 1s

       Run Cpu               [ "--template" , "CPU: <total>%",
                               "--Low"      , "3",
                               "--High"     , "80",
                               "--low"      , "#859900", -- green
                               "--normal"   , "#cb4b16", -- orange
                               "--high"     , "#dc322f", -- red
                               "--width"    , "2"
                             ] 10, -- 1s

       Run CoreTemp          [ "--template" , "T: <core0>°C",
                               "--Low"      , "70",
                               "--High"     , "80",
                               "--low"      , "#859900", -- green
                               "--normal"   , "#cb4b16", -- orange
                               "--high"     , "#dc322f" -- red
                             ] 50, -- 5s

       Run Memory            [ "--template" , "RAM: <usedratio>%",
                               "--Low"      , "20",
                               "--High"     , "90",
                               "--low"      , "#859900", -- green
                               "--normal"   , "#cb4b16", -- orange
                               "--high"     , "#dc322f" -- red
                             ] 10, -- 1s

       Run Swap              [ "--template" ,"Swap: <usedratio>%",
                               "--Low"      , "20",
                               "--High"     , "90",
                               "--low"      , "#859900", -- green
                               "--normal"   , "#cb4b16", -- orange
                               "--high"     , "#dc322f" --red
                             ] 10, -- 1s

       Run DiskU             [ ("/", "/: <used> <size>"),
                               -- ("/data0", "/data0 <used> <free> <size>")
                               ("sda1", "<freebar>")
                             ]
                             [ "--Low"      , "20",
                               "--High"     , "50",
                               "--minwidth" , "1",
                               "--pad"      , "3"
                             ] 50, -- 5s

       -- theme: default / orange
       -- Run Date              "<fc=#cb4b16>%a %Y-%m-%d %H:%M:%S</fc>" "date" 10, -- 1s
       -- theme: default / cyan
       -- Run Date              "<fc=#2aa198>%a %Y-%m-%d %H:%M:%S</fc>" "date" 10, -- 1s
       -- theme: default / blue
       Run Date              "<fc=#268bd2>%a %Y-%m-%d %H:%M:%S</fc>" "date" 10, -- 1s
       -- theme: default / base3
       -- Run Date              "<fc=#fdf6e3>%a %Y-%m-%d %H:%M:%S</fc>" "date" 10, -- 1s

       Run Com               "uname" ["-s","-r"] "uname1" 36000, -- 1h
       Run Com               ".xmonad/spaces.sh" ["spaces", "6"] "spaces1" 300, -- 30s
       Run Com               ".xmonad/wireless.sh" [] "wireless1" 300, -- 30s
       -- Run StdinReader
       Run UnsafeStdinReader
     ],
     sepChar = "%",
     alignSep = "}{",

     -- template = "%UnsafeStdinReader% | %multicpu% | %coretemp% | %memory% %swap% | %disku% | %dynnetwork% | %battery% }{ %LDZA% | %date%"
     -- template = "%UnsafeStdinReader% | %multicpu% | %coretemp% | %memory% %swap% | %disku% | %dynnetwork% <fc=green>%wireless1%</fc> | %battery% }{ %date% | %spaces1%"
     -- template = "%UnsafeStdinReader% | %multicpu% | %coretemp% | %memory% %swap% | %dynnetwork% <fc=green>%wireless1%</fc> | %battery% }{ %date% | %spaces1%"
     -- template = "%UnsafeStdinReader% }{ %cpu% | %coretemp% | %memory% %swap% | %dynnetwork% %wlp1s0wi% | %battery% | %date% | %spaces1%"
     -- template = "%UnsafeStdinReader% }{ [ %cpu% %coretemp% ] [ %memory% %swap% ] [ %dynnetwork% ] [ %battery% ] [ %date% ] %spaces1%"
     template = "%UnsafeStdinReader% }{ [ %cpu% %coretemp% | %memory% %swap% | %dynnetwork% | %battery% | %LDZA% | %date% ] %spaces1%"
}

-- end
