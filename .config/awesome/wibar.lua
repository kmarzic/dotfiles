-- -----------------------------------------------------------------------------
-- {{{ wibar
-- -----------------------------------------------------------------------------

-- Standard awesome library
local gears = require("gears")
local awful = require("awful")

-- Widget and layout library
local wibox = require("wibox")

-- Notification library
local naughty = require("naughty")

-- Theme handling library
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")

-- Local
local helpers  = require("helpers")
local menu     = require("menu")
local signals  = require("signals")

-- Vicious
-- https://github.com/vicious-widgets/vicious.git
local vicious  = require("vicious")


-- {{{ Wibar
local my_update_interval = 15
local my_update_interval_weather = 3600
local my_update_interval_network = 5
local my_update_interval_wifi = 5
local dpi = xresources.apply_dpi
local pad = helpers.pad

local mytags = {
    tags = {
        { names =  { "1", "2", "3", "4", "5", "6", "7", "8", "9", "0" },
          layout = { awful.layout.layouts[1], awful.layout.layouts[1], awful.layout.layouts[1], awful.layout.layouts[1], awful.layout.layouts[1],
                     awful.layout.layouts[1], awful.layout.layouts[1], awful.layout.layouts[1], awful.layout.layouts[1], awful.layout.layouts[1]
                   },
        },
        { names =  { "1", "2", "3", "4", "5", "6", "7", "8", "9", "0" },
          layout = { awful.layout.layouts[1], awful.layout.layouts[1], awful.layout.layouts[1], awful.layout.layouts[1], awful.layout.layouts[1],
                     awful.layout.layouts[1], awful.layout.layouts[1], awful.layout.layouts[1], awful.layout.layouts[1], awful.layout.layouts[1]
                   },
        },
        { names =  { "1", "2", "3", "4", "5", "6", "7", "8", "9", "0" },
          layout = { awful.layout.layouts[1], awful.layout.layouts[1], awful.layout.layouts[1], awful.layout.layouts[1], awful.layout.layouts[1],
                     awful.layout.layouts[1], awful.layout.layouts[1], awful.layout.layouts[1], awful.layout.layouts[1], awful.layout.layouts[1]
                   },
        },
    }
}

-- Create a wibox for each screen and add it
local taglist_buttons = gears.table.join(
    awful.button({ }, 1, function(t) t:view_only() end),
    awful.button({ modkey }, 1, function(t)
                              if client.focus then
                                  client.focus:move_to_tag(t)
                              end
                          end),
    awful.button({ }, 3, awful.tag.viewtoggle),
    awful.button({ modkey }, 3, function(t)
                              if client.focus then
                                  client.focus:toggle_tag(t)
                              end
                          end),
    awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
    awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
)

local tasklist_buttons = gears.table.join(
    awful.button({ }, 1, function (c)
                             if c == client.focus then
                                 c.minimized = true
                             else
                                 c:emit_signal(
                                     "request::activate",
                                     "tasklist",
                                     {raise = true}
                                 )
                             end
                         end),
    awful.button({ }, 3, function()
                             awful.menu.client_list({ theme = { width = 250 } })
                         end),
    awful.button({ }, 4, function ()
                             awful.client.focus.byidx(1)
                         end),
    awful.button({ }, 5, function ()
                             awful.client.focus.byidx(-1)
                         end)
)

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
local function set_wallpaper(s)
    -- Wallpaper
    if beautiful.wallpaper then
        local wallpaper = beautiful.wallpaper
        -- If wallpaper is a function, call it with the screen
        if type(wallpaper) == "function" then
            wallpaper = wallpaper(s)
        end
        gears.wallpaper.maximized(wallpaper, s, true)
    end
end

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", set_wallpaper)

awful.screen.connect_for_each_screen(function(s)
    -- Wallpaper
    -- set_wallpaper(s)

    -- (1) Each screen has its own tag table.
    awful.tag({ "1", "2", "3", "4", "5", "6", "7", "8", "9", "0" }, s, awful.layout.layouts[1])
    -- (2) Tags on all screens
    -- screen_index = s.index
    -- awful.tag(mytags.tags[screen_index].names, s, mytags.tags[screen_index].layout)

    -- Create a promptbox for each screen
    s.mypromptbox = awful.widget.prompt()

    -- Create an imagebox widget which will contain an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    s.mylayoutbox = awful.widget.layoutbox(s)
    s.mylayoutbox:buttons(
        gears.table.join(
            awful.button({ }, 1, function () awful.layout.inc( 1) end),
            awful.button({ }, 3, function () awful.layout.inc(-1) end),
            awful.button({ }, 4, function () awful.layout.inc( 1) end),
            awful.button({ }, 5, function () awful.layout.inc(-1) end)
        )
    )

    -- Create a taglist widget
    -- (1) new awesome version
    s.mytaglist = awful.widget.taglist {
        screen  = s,
        filter  = awful.widget.taglist.filter.all,
        buttons = taglist_buttons,
        style   = {
            shape = gears.shape.rectangle,
            -- shape = helpers.rrect(beautiful.border_radius),
            font  = beautiful.font,
        },
    }
    -- (2) old awesome version
    -- s.mytaglist = awful.widget.taglist(s, awful.widget.taglist.filter.all, taglist_buttons)

    -- Create a tasklist widget
    -- (1) new awesome version
    s.mytasklist = awful.widget.tasklist {
        screen  = s,
        filter  = awful.widget.tasklist.filter.currenttags,
        buttons = tasklist_buttons,
        style   = {
            -- shape = gears.shape.rectangle,
            shape = helpers.rrect(beautiful.border_radius),
            font  = beautiful.font,
        },
    }
    -- (2) old awesome version
    -- s.mytasklist = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, tasklist_buttons)

    -- System Widgets
    -- local my_keyboard_layout = awful.widget.keyboardlayout()
    -- local my_cpu_temp = awful.widget.watch([[bash -c "~/.config/awesome/watch.sh temp"]], update_interval)
    -- local my_cpu_load = awful.widget.watch([[bash -c "~/.config/awesome/watch.sh load"]], update_interval)
    -- local my_acpi = awful.widget.watch([[bash -c "~/.config/awesome/watch.sh acpi"]], update_interval)
    -- local my_mem = awful.widget.watch([[bash -c "~/.config/awesome/watch.sh mem"]], update_interval)
    -- local my_swap = awful.widget.watch([[bash -c "~/.config/awesome/watch.sh swap"]], update_interval)
    -- local my_text_clock = wibox.widget.textclock ("%a %Y-%m-%d %H:%M:%S", 1)
    -- local month_calendar = awful.widget.calendar_popup.month()
    -- month_calendar:attach(my_text_clock, "br")

    -- Vicious Widgets
    local my_cpu_temp = wibox.widget.textbox()
    -- (1) ansi
    vicious.register (my_cpu_temp, vicious.widgets.thermal, '<span>CPU Temp: </span><span color="cyan"><b>$1&#8451;</b></span>', my_update_interval, "thermal_zone0")
    -- (2) solarized white
    -- vicious.register (my_cpu_temp, vicious.widgets.thermal, '<span>CPU Temp: </span><span color="#2aa198"><b>$1&#8451;</b></span>', my_update_interval, "thermal_zone0")

    local my_cpu_load = wibox.widget.textbox()
    -- (1) ansi
    vicious.register (my_cpu_load, vicious.widgets.cpu, '<span>CPU Load: </span><span color="cyan"><b>$1%</b></span>', my_update_interval)
    -- (2) solarized white
    -- vicious.register (my_cpu_load, vicious.widgets.cpu, '<span>CPU Load: </span><span color="#2aa198"><b>$1%</b></span>', my_update_interval)

    local my_acpi_1 = wibox.widget.textbox()
    -- (1) ansi
    vicious.register (my_acpi_1, vicious.widgets.bat, '<span>Battery: </span><span color="cyan"><b>$1$2% $3 $4</b></span>', my_update_interval, "BAT0")
    -- (2) solarized white
    -- vicious.register (my_acpi_1, vicious.widgets.bat, '<span>Battery: </span><span color="#2aa198"><b>$1$2% $3 $4</b></span>', my_update_interval, "BAT0")
    --
    local my_acpi_2 = wibox.widget.textbox()
    -- (1) ansi
    vicious.register (my_acpi_2, vicious.widgets.bat, '<span></span><span color="orange">$1$2%</span>', my_update_interval, "BAT0")
    -- (2) solarized white
    -- vicious.register (my_acpi_2, vicious.widgets.bat, '<span></span><span color="#cb4b16">$1$2%</span>', my_update_interval, "BAT0")

    local my_battery_icon = wibox.widget.imagebox(beautiful.icon_batt)
    my_battery_icon.resize = true
    -- my_battery_icon.forced_width = dpi(20)
    -- my_battery_icon.forced_height = dpi(20)

    local my_mem = wibox.widget.textbox()
    -- (1) ansi
    vicious.register (my_mem, vicious.widgets.mem, '<span>RAM: </span><span color="cyan"><b>$2MB $1%</b></span>', my_update_interval)
    -- (2) solarized white
    -- vicious.register (my_mem, vicious.widgets.mem, '<span>RAM: </span><span color="#2aa198"><b>$2MB $1%</b></span>', my_update_interval)

    local my_network = wibox.widget.textbox()
    -- (***) dle6440
    -- (1) ansi
    -- vicious.register (my_network, vicious.widgets.net, '<span>Eth: </span><span color="cyan"><b>&#8593; ${eno1 up_kb}kB/s &#8595; ${eno1 down_kb}kB/s</b></span>\rWifi: <span color="cyan"><b>&#8593; ${wlan0 up_kb}kB/s &#8595; ${wlan0 down_kb}kB/s</b></span>', my_update_interval_network)
    -- (2) solarized white
    -- vicious.register (my_network, vicious.widgets.net, '<span>Eth: </span><span color="#2aa198"><b>&#8593; ${eno1 up_kb}kB/s &#8595; ${eno1 down_kb}kB/s</b></span>\rWifi: <span color="#2aa198"><b>&#8593; ${wlan0 up_kb}kB/s &#8595; ${wlan0 down_kb}kB/s</b></span>', my_update_interval_network)
    -- (***) elxa4n8pyf2
    -- (1) ansi
    vicious.register (my_network, vicious.widgets.net, '<span>Eth: </span><span color="cyan"><b>&#8593; ${enp0s31f6 up_kb}kB/s &#8595; ${enp0s31f6 down_kb}kB/s</b></span>\rWifi: <span color="cyan"><b>&#8593; ${wlp1s0 up_kb}kB/s &#8595; ${wlp1s0 down_kb}kB/s</b></span>', my_update_interval_network)
    -- (2) solarized white
    -- vicious.register (my_network, vicious.widgets.net, '<span>Eth: </span><span color="#2aa198"><b>&#8593; ${enp0s31f6 up_kb}kB/s &#8595; ${enp0s31f6 down_kb}kB/s</b></span>\rWifi: <span color="#2aa198"><b>&#8593; ${wlp1s0 up_kb}kB/s &#8595; ${wlp1s0 down_kb}kB/s</b></span>', my_update_interval_network)

    local my_wifi = wibox.widget.textbox()
    -- (***) dle6440
    -- (1) ansi
    -- vicious.register (my_wifi, vicious.widgets.wifiiw, '<span>Wifi: </span><span color="cyan"><b>${bssid}, ${ssid}, ${mode}, ${chan} ch, ${rate} (Mb/s), ${freq} MHz, ${linp}%, ${txpw} dBm, ${sign} dBM</b></span>', my_update_interval_wifi, "wlan0")
    -- (2) solarized white
    -- vicious.register (my_wifi, vicious.widgets.wifiiw, '<span>Wifi: </span><span color="#2aa198"><b>${bssid}, ${ssid}, ${mode}, ${chan} ch, ${rate} (Mb/s), ${freq} MHz, ${linp}%, ${txpw} dBm, ${sign} dBM</b></span>', my_update_interval_wifi, "wlan0")
    -- (***) elxa4n8pyf2
    -- (1) ansi
    vicious.register (my_wifi, vicious.widgets.wifiiw, '<span>Wifi: </span><span color="cyan"><b>${bssid}, ${ssid}, ${mode}, ${chan} ch, ${rate} (Mb/s), ${freq} MHz, ${linp}%, ${txpw} dBm, ${sign} dBM</b></span>', my_update_interval_wifi, "wlp1s0")
    -- (2) solarized white
    -- vicious.register (my_wifi, vicious.widgets.wifiiw, '<span>Wifi: </span><span color="#2aa198"><b>${bssid}, ${ssid}, ${mode}, ${chan} ch, ${rate} (Mb/s), ${freq} MHz, ${linp}%, ${txpw} dBm, ${sign} dBM</b></span>', my_update_interval_wifi, "wlp1s0")

    local my_text_clock = wibox.widget.textbox()
    -- (1) ansi
    vicious.register (my_text_clock, vicious.widgets.date, '<span color="cyan">%a %Y-%m-%d %H:%M:%S</span>', 1)
    -- (2) solarized white
    -- vicious.register (my_text_clock, vicious.widgets.date, '<span color="#2aa198">%a %Y-%m-%d %H:%M:%S</span>', 1)

    local month_calendar = awful.widget.calendar_popup.month({})
    function month_calendar.call_calendar(self, offset, position, screen)
        local screen = awful.screen.focused()
        awful.widget.calendar_popup.call_calendar(self, offset, position, screen)
    end
    month_calendar:attach(my_text_clock, "br" )

    local my_volume = wibox.widget.textbox()
    -- vicious.register(my_volume, vicious.widgets.volume, '<span color="cyan"> <b>$2 $1%</b></span>', 1, "Master")
    vicious.register(my_volume, vicious.widgets.volume,
        function (widget, args)
            local label = {["🔉"] = "♫", ["🔈"] = "♩"}
            -- (1) ansi
            return ('<span color="#00ff00">%s %d%%</span>'):format(label[args[2]], args[1])
            -- (2) solarized white
            -- return ('<span color="#859900">%s %d%%</span>'):format(label[args[2]], args[1])
        end, 1, "Master")

    local my_weather = wibox.widget.textbox()
    -- (1) ansi
    vicious.register(my_weather, vicious.widgets.weather, '<span>${city}: </span><span color="cyan"><b>${tempc}&#8451;, ${humid}%, ${windkmh}km/h, ${sky}, ${weather}</b></span>', my_update_interval_weather, "LDZA")
    -- (2) solarized white
    -- vicious.register(my_weather, vicious.widgets.weather, '<span>${city}: </span><span color="#2aa198"><b>${tempc}&#8451;, ${humid}%, ${windkmh}km/h, ${sky}, ${weather}</b></span>', my_update_interval_weather, "LDZA")

    -- Create a wibox that will only show the stats widget.
    -- Hidden by default. Can be toggled with a keybind
    s.stats = wibox ({
        visible = false,
        screen = s,
        ontop = true,
        shape = helpers.rrect(beautiful.border_radius),
        width = dpi(800),
        height = dpi(180),
        x = dpi(50),
        y = dpi(5),
        opacity = 0.8,
        bg = beautiful.bg_wibox,
    })
    s.stats:setup {
        pad(1),
        {
            pad(1),
            my_cpu_temp,
            my_cpu_load,
            my_acpi_1,
            my_mem,
            my_network,
            my_wifi,
            my_weather,
            pad(1),
            layout = wibox.layout.fixed.vertical,
        },
        pad(1),
        layout = wibox.layout.fixed.horizontal,
    }
    s.stats:buttons(gears.table.join(
        -- Left click - Hide stats
        awful.button({ }, 1, function ()
            s.stats.visible = false
        end)
    ))

    -- Create systray
    s.systray = wibox.widget.systray()
    -- s.systray.forced_width = dpi(80)
    -- (1) not visible
    -- s.systray.visible = false
    -- (2) visible
    s.systray.visible = true

    -- Create the wibox
    -- (1) top
    -- s.mywibox = awful.wibar({ position = "top", screen = s })
    -- (2) bottom
    s.mywibox = awful.wibar({ position = "bottom", screen = s })

    -- Add widgets to the wibox
    s.mywibox:setup {
        layout = wibox.layout.align.horizontal,
        -- (1) Left widgets
        {
            -- layout = wibox.layout.fixed.horizontal,
            layout = wibox.layout.align.horizontal,
            mylauncher,
            s.mytaglist,
            s.mypromptbox,
        },
        -- (2) Middle widget
        {
            layout = wibox.layout.align.horizontal,
            s.mytasklist,
        },
        -- (3) Right widgets
        {
            layout = wibox.layout.fixed.horizontal,
            -- layout = wibox.layout.align.horizontal,
            my_volume,
            pad(1),
            my_battery_icon,
            my_acpi_2,
            pad(1),
            my_text_clock,
            pad(1),
            s.mylayoutbox,
            s.systray,
        },
    }
end)
-- }}}

-- -----------------------------------------------------------------------------
-- }}}
-- -----------------------------------------------------------------------------
