-- xmobar.hs
-- Last update: 2023-05-05 12:20:49 (CEST)

Config {
   -- theme: ansi
   bgColor = "#101010",
   fgColor = "#f2f2f2",

   -- font = "xft:monospace:size=11:antialias=true:style=bold",
   -- font = "xft:monospace:size=11:antialias=true:style=regular",
   -- font = "xft:DejaVu Sans Mono:size=10:antialias=true:autohint=true:style=regular",
   font = "xft:DejaVuSansM Nerd Font:size=10:antialias=true:autohint=true:style=book",
   --
   additionalFonts = [ "xft:DejaVu Sans Mono:size=10:antialias=true:autohint=true", "xft:DejaVuSansM Nerd Font:size=10:regular:antialias=true:autohint=true"],

   borderColor = "black",
   border = TopB,
   -- alpha = 255,
   alpha = 180,
   -- alpha = 0,
   -- position = BottomP 2 2,
   position = BottomSize L 100 25,
   textOffset = -1,
   iconOffset = -1,
   lowerOnStart = True,     -- send to bottom of window stack on start
   pickBroadest = False,    -- choose widest display (multi-monitor)
   persistent = False,      -- enable/disable hiding (True = disabled)
   hideOnStart = False,     -- start with window unmapped (hidden)
   iconRoot = ".config/xmonad/icons/",
   allDesktops = True,      -- show on all desktops
   overrideRedirect = True, -- set the Override Redirect flag (Xlib)
   sepChar = "%",
   alignSep = "}{",

   template = " <icon=haskell_20_light.xpm/> %UnsafeStdinReader% }{ [ %cpu% %coretemp% | %memory% %swap% | %dynnetwork% | %battery% | %date% ] %spaces1%",
   -- template = " <icon=haskell_20_light.xpm/> %UnsafeStdinReader% }{ [ %cpu% %coretemp% | %memory% %swap% | %dynnetwork% | %battery% | %LDZA% | %date% ] %spaces1%",
   -- template = " <icon=haskell_20_light.xpm/> %UnsafeStdinReader% }{ [ %cpu% %coretemp% | %memory% %swap% | %dynnetwork% | %battery% | <fn=1>%forecast1%</fn> | %date% ] %spaces1%",

   commands =
     [
       Run Weather "LDZA"    [ "--template" ,"<tempC>°C <fc=green><rh></fc>% <fc=green><pressure></fc>hPa",
                               "--Low"      , "10",
                               "--High"     , "25",
                               "--low"      , "cyan", -- cyan
                               "--normal"   , "green", -- green
                               "--high"     , "red" -- red
                             ] 36000, -- 1h

       Run DynNetwork        [ -- "--template" , "<dev>: <tx>kB/s <rx>kB/s",
                               -- "--template" , "UL: <tx>kB/s DL: <rx>kB/s",
                               "--template" , "<icon=up.xbm/><tx>kB/s <icon=down.xbm/><rx>kB/s",
                               "--Low"      , "10000", -- units: 10 kB/s
                               "--High"     , "500000", -- units: 500 kB/s
                               "--low"      , "cyan", -- cyan
                               "--normal"   , "green", -- green
                               "--high"     , "red", -- red
                               "--minwidth" , "4",
                               "-c"         , " "
                             ] 10, -- 1s

       Run Wireless "wlp1s0" [ "--template" , "<fc=#2aa198><essid>:</fc><quality>%", -- cyan
                               "--Low"      , "20",
                               "--High"     , "70",
                               "--low"      , "cyan", -- cyan
                               "--normal"   , "green", -- green
                               "--high"     , "red", -- red
                               "--minwidth" , "3",
                               "--nastring" , "No Wifi"
                             ] 10, -- 1s

       Run Battery           [ -- "--template" , "BAT: <acstatus>",
                               "--template" , "<acstatus>",
                               "--Low"      , "10",
                               "--High"     , "80",
                               "--low"      , "red", -- red
                               "--normal"   , "green", -- green
                               "--high"     , "cyan", -- cyan

                               "--", -- battery specific options
                               -- discharging status
                               -- "-o"   , "<left>% (<timeleft>)",
                               "-o"   , "<icon=batt.xbm/> <left>% (<timeleft>)",
                               -- AC "on" status
                               -- "-O"   , "<fc=red>c</fc> <left>% (<timeleft>)", -- orange
                               "-O"   , "<icon=batt_on.xbm/> <left>% (<timeleft>)", -- orange
                               -- charged status
                               -- "-i"   , "<fc=green>f</fc>" -- green
                               "-i"   , "<icon=batt_idle.xbm/>" -- green
                             ] 50, -- 5s

       Run MultiCpu          [ -- "--template" , "CPU: <total0>%|<total1>%|<total2>%|<total3>%",
                               "--template" , "<icon=cpu.xbm/><total0>%|<total1>%|<total2>%|<total3>%",
                               "--Low"      , "50",
                               "--low"      , "cyan", -- cyan
                               "--normal"   , "green", -- green
                               "--high"     , "red", -- red
                               "--High"     , "85",
                               "--width"    , "2"
                             ] 10, -- 1s

       Run Cpu               [ -- "--template" , "CPU: <total>%",
                               "--template" , "<icon=cpu.xbm/><total>%",
                               "--Low"      , "3",
                               "--High"     , "80",
                               "--minwidth" , "3",
                               "--low"      , "cyan", -- cyan
                               "--normal"   , "green", -- green
                               "--high"     , "red", -- red
                               "--width"    , "3"
                             ] 10, -- 1s

       Run CoreTemp          [ -- "--template" , "<core0>°C",
                               "--template" , "<icon=temp.xbm/><core0>°C",
                               "--Low"      , "70",
                               "--High"     , "80",
                               "--minwidth" , "3",
                               "--low"      , "cyan", -- cyan
                               "--normal"   , "green", -- green
                               "--high"     , "red" -- red
                             ] 50, -- 5s

       Run Memory            [ -- "--template" , "RAM: <usedratio>%",
                               "--template" , "<icon=mem.xbm/><usedratio>%",
                               "--Low"      , "20",
                               "--High"     , "90",
                               "--minwidth" , "3",
                               "--low"      , "cyan", -- cyan
                               "--normal"   , "green", -- green
                               "--high"     , "red" -- red
                             ] 10, -- 1s

       Run Swap              [ -- "--template" ,"Swap: <usedratio>%",
                               "--template" , "<icon=mem_old.xbm/><usedratio>%",
                               "--Low"      , "20",
                               "--High"     , "90",
                               "--minwidth" , "3",
                               "--low"      , "cyan", -- cyan
                               "--normal"   , "green", -- green
                               "--high"     , "red" -- red
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
       Run Date              "<icon=calendar.xbm/> <fc=cyan>%a %Y-%m-%d %H:%M:%S</fc>" "date" 10, -- 1s

       Run Com               "uname" ["-s","-r"] "uname1" 36000, -- 1h
       Run Com               ".xmonad/spaces.sh" ["spaces", "8"] "spaces1" 300, -- 30s
       Run Com               ".xmonad/wireless.sh" [] "wireless1" 300, -- 30s
       Run Com               ".xmonad/forecast.sh" [] "forecast1" 600, -- 1m
       Run UnsafeStdinReader
     ]
}

-- end
