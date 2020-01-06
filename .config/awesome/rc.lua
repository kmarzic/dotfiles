-- -----------------------------------------------------------------------------
-- rc.lua
-- -----------------------------------------------------------------------------

-- If Luocks is installed, make sure that packages installed through it are
-- founde.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")

-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")

-- Widget and layout library
local wibox = require("wibox")

-- Theme handling library
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")

-- Notification library
local naughty = require("naughty")
local menubar = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup")

-- Vicious
local vicious = require("vicious")

-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
require("awful.hotkeys_popup.keys")

-- -----------------------------------------------------------------------------
-- {{{ Error handling
-- -----------------------------------------------------------------------------
--
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify({
        preset = naughty.config.presets.critical,
        title = "Oops, there were errors during startup!",
        text = awesome.startup_errors
    })
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        naughty.notify({
            preset = naughty.config.presets.critical,
            title = "Oops, an error happened!",
            text = tostring(err)
        })
        in_error = false
    end)
end
-- -----------------------------------------------------------------------------
-- }}}
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- {{{ Variable definitions
-- -----------------------------------------------------------------------------
--
-- Themes define colours, icons, font and wallpapers.
-- beautiful.init(gears.filesystem.get_themes_dir() .. "default/theme.lua")
-- beautiful.init(gears.filesystem.get_themes_dir() .. "ansi/theme.lua")
-- beautiful.init(gears.filesystem.get_themes_dir() .. "gtk/theme.lua")
-- beautiful.init(gears.filesystem.get_themes_dir() .. "sky/theme.lua")
-- beautiful.init(gears.filesystem.get_themes_dir() .. "xresources/theme.lua")
-- beautiful.init(gears.filesystem.get_themes_dir() .. "zenburn/theme.lua")
--
-- local theme_name = "default"
local theme_name = "ansi"
-- local theme_name = "gtk"
-- local theme_name = "sky"
-- local theme_name = "xresources"
-- local theme_name = "zenburn"
local theme_dir = os.getenv("HOME") .. "/.config/awesome/themes/"
beautiful.init( theme_dir .. theme_name .. "/theme.lua" )

-- This is used later as the default terminal and editor to run.
local terminal = "urxvt"
local browser = "firefox"
local mail = "evolution"
local messenger = "gajim"
local editor = os.getenv("EDITOR") or "vim"
local editor_cmd = terminal .. " -e " .. editor

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
-- modkey = "Mod4" -- meta
local modkey = "Mod1" -- alt
local winkey = "Mod4" -- windows key

-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
    awful.layout.suit.max,
    awful.layout.suit.tile,
    -- awful.layout.suit.floating,
    -- awful.layout.suit.tile.left,
    awful.layout.suit.tile.bottom,
    -- awful.layout.suit.tile.top,
    -- awful.layout.suit.fair,
    -- awful.layout.suit.fair.horizontal,
    -- awful.layout.suit.spiral,
    -- awful.layout.suit.spiral.dwindle,
    -- awful.layout.suit.max.fullscreen,
    -- awful.layout.suit.magnifier,
    -- awful.layout.suit.corner.nw,
    -- awful.layout.suit.corner.ne,
    -- awful.layout.suit.corner.sw,
    -- awful.layout.suit.corner.se,
}

-- Naughty config
--
-- Icon size
naughty.config.defaults['icon_size'] = beautiful.notification_icon_size

-- Timeouts
naughty.config.defaults.timeout = 5
naughty.config.presets.low.timeout = 2
naughty.config.presets.critical.timeout = 12

-- Apply theme variables
naughty.config.padding = beautiful.notification_padding
naughty.config.spacing = beautiful.notification_spacing
naughty.config.defaults.margin = beautiful.notification_margin
naughty.config.defaults.border_width = beautiful.notification_border_width

naughty.config.presets.normal = {
    font         = beautiful.notification_font,
    fg           = beautiful.notification_fg,
    bg           = beautiful.notification_bg,
    border_width = beautiful.notification_border_width,
    margin       = beautiful.notification_margin,
    position     = beautiful.notification_position
}

naughty.config.presets.low = {
    font         = beautiful.notification_font,
    fg           = beautiful.notification_fg,
    bg           = beautiful.notification_bg,
    border_width = beautiful.notification_border_width,
    margin       = beautiful.notification_margin,
    position     = beautiful.notification_position
}

naughty.config.presets.ok = naughty.config.presets.low
naughty.config.presets.info = naughty.config.presets.low
naughty.config.presets.warn = naughty.config.presets.normal

-- naughty.config.presets.critical = {
--     font         = beautiful.notification_font,
--     fg           = beautiful.notification_crit_fg,
--     bg           = beautiful.notification_crit_bg,
--     border_width = beautiful.notification_border_width,
--     margin       = beautiful.notification_margin,
--     position     = beautiful.notification_position
-- }

naughty.config.presets.critical.bg = beautiful.notification_crit_bg
naughty.config.presets.critical.fg = beautiful.notification_crit_fg

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
-- -----------------------------------------------------------------------------
-- }}}
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- {{{ Menu
-- -----------------------------------------------------------------------------
--
-- Create a launcher widget and a main menu
local myawesomemenu = {
    { "hotkeys", function() hotkeys_popup.show_help(nil, awful.screen.focused()) end },
    { "manual", terminal .. " -e man awesome" },
    { "edit config", editor_cmd .. " " .. awesome.conffile },
    { "restart", awesome.restart },
    { "quit", function() awesome.quit() end },
}

local mymainmenu = awful.menu({
    items = {
        { "awesome", myawesomemenu, beautiful.awesome_icon },
        { "terminal", terminal },
        { "firefox", browser }
    }
})

local mylauncher = awful.widget.launcher({
    image = beautiful.awesome_icon,
    menu = mymainmenu
})

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- -----------------------------------------------------------------------------
-- }}}
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- {{{ Wibar
-- -----------------------------------------------------------------------------
--
local my_update_interval = 15
--
-- System Widget
-- local my_keyboard_layout = awful.widget.keyboardlayout()
-- local my_text_clock = wibox.widget.textclock ("| %a %Y-%m-%d %H:%M:%S |", 1)
-- local my_cpu_temp = awful.widget.watch([[bash -c "~/.config/awesome/watch.sh temp"]], update_interval)
-- local my_cpu_load = awful.widget.watch([[bash -c "~/.config/awesome/watch.sh load"]], update_interval)
-- local my_acpi = awful.widget.watch([[bash -c "~/.config/awesome/watch.sh acpi"]], update_interval)
-- local my_mem = awful.widget.watch([[bash -c "~/.config/awesome/watch.sh mem"]], update_interval)
-- local my_swap = awful.widget.watch([[bash -c "~/.config/awesome/watch.sh swap"]], update_interval)
--
-- Vicious Widget
local my_cpu_temp = wibox.widget.textbox()
vicious.register (my_cpu_temp, vicious.widgets.thermal, '<span>CPU: </span><span color="cyan"><b>$1&#8451;</b></span> ', my_update_interval, "thermal_zone0")

local my_cpu_load = wibox.widget.textbox()
vicious.register (my_cpu_load, vicious.widgets.cpu, '<span> </span><span color="cyan"><b>$1%</b></span> |', my_update_interval)

local my_acpi = wibox.widget.textbox()
vicious.register (my_acpi, vicious.widgets.bat, '<span> Batt: </span><span color="cyan"><b>$1$2%</b></span> |', my_update_interval, "BAT0")

local my_mem = wibox.widget.textbox()
vicious.register (my_mem, vicious.widgets.mem, '<span> RAM: </span><span color="cyan"><b>$2MB $1%</b></span> |', my_update_interval)

local my_network = wibox.widget.textbox()
-- (1) dle6440
-- vicious.register (my_network, vicious.widgets.net, '<span> Net: </span><span color="cyan"><b>${eno1 up_kb}kB/s ${eno1 down_kb}kB/s | ${wlp3s0 up_kb}kB/s ${wlp3s0 down_kb}kB/s</b></span> |', 1)
-- (2) elxa4n8pyf2
vicious.register (my_network, vicious.widgets.net, '<span> Net: </span><span color="cyan"><b>${enp0s31f6 up_kb}kB/s ${enp0s31f6 down_kb}kB/s | ${wlp1s0 up_kb}kB/s ${wlp1s0 down_kb}kB/s</b></span> |', 1)

local my_text_clock = wibox.widget.textbox()
vicious.register (my_text_clock, vicious.widgets.date, '<span color="cyan"><b> %a %Y-%m-%d %H:%M:%S</b></span> |', 1)

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
    set_wallpaper(s)

    -- (1) Each screen has its own tag table.
    -- awful.tag({ "1", "2", "3", "4", "5", "6", "7", "8", "9", "0" }, s, awful.layout.layouts[1])
    --
    -- (2) Tags on all screens
    screen_index = s.index
    awful.tag(mytags.tags[screen_index].names, s, mytags.tags[screen_index].layout)

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
        buttons = taglist_buttons
    }
    --
    -- (2) old awesome version
    -- s.mytaglist = awful.widget.taglist(s, awful.widget.taglist.filter.all, taglist_buttons)

    -- Create a tasklist widget
    -- (1) new awesome version
    s.mytasklist = awful.widget.tasklist {
        screen  = s,
        filter  = awful.widget.tasklist.filter.currenttags,
        buttons = tasklist_buttons
    }
    --
    -- (2) old awesome version
    -- s.mytasklist = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, tasklist_buttons)

    -- Create the wibox
    -- (1) top
    -- s.mywibox = awful.wibar({ position = "top", screen = s })
    --
    -- (2) bottom
    s.mywibox = awful.wibar({ position = "bottom", screen = s })

    -- Add widgets to the wibox
    s.mywibox:setup {
        layout = wibox.layout.align.horizontal,
        {   -- (1) Left widgets
            -- layout = wibox.layout.fixed.horizontal,
            layout = wibox.layout.align.horizontal,
            mylauncher,
            s.mytaglist,
            s.mypromptbox,
        },
        s.mytasklist, -- Middle widget
        {   -- (2) Right widgets
            layout = wibox.layout.fixed.horizontal,
            -- layout = wibox.layout.align.horizontal,
            my_cpu_temp,
            my_cpu_load,
            my_acpi,
            my_mem,
            -- my_swap,
            -- my_keyboard_layout,
            my_network,
            my_text_clock,
            s.mylayoutbox,
            wibox.widget.systray(),
        },
    }
end)
-- -----------------------------------------------------------------------------
-- }}}
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- {{{ Mouse bindings
-- -----------------------------------------------------------------------------
--
root.buttons(gears.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end),
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev)
))
-- -----------------------------------------------------------------------------
-- }}}
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- {{{ Key bindings
-- -----------------------------------------------------------------------------
--
globalkeys = gears.table.join(
    -- Default
    awful.key({ modkey, "Shift"   }, "/",      hotkeys_popup.show_help,
              { description = "show help", group="awesome"} ),

    awful.key({ modkey,           }, "Left",   awful.tag.viewprev,
              { description = "view previous", group = "tag"} ),
    awful.key({ modkey,           }, "Right",  awful.tag.viewnext,
              { description = "view next", group = "tag"} ),
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore,
              { description = "go back", group = "tag"} ),

    awful.key({ modkey,           }, "j", function () awful.client.focus.byidx( 1) end,
              { description = "focus next by index", group = "client"} ),
    awful.key({ modkey,           }, "k", function () awful.client.focus.byidx(-1) end,
              { description = "focus previous by index", group = "client"} ),

    awful.key({ modkey, "Control" }, "w", function () mymainmenu:show() end,
              { description = "show main menu", group = "awesome"}),

    -- Layout manipulation
    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end,
              { description = "swap with next client by index", group = "client"}),
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end,
              { description = "swap with previous client by index", group = "client"}),

    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end,
              { description = "focus the next screen", group = "screen"}),
    awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end,
              { description = "focus the previous screen", group = "screen"}),

    awful.key({ modkey,           }, "w", function () awful.screen.focus(1) end,
              { description = "Switch to physical/Xinerama screen 1", group = "screen"}),
    awful.key({ modkey,           }, "e", function () awful.screen.focus(2) end,
              { description = "Switch to physical/Xinerama screen 2", group = "screen"}),
    awful.key({ modkey,           }, "r", function () awful.screen.focus(3) end,
              { description = "Switch to physical/Xinerama screen 3", group = "screen"}),

    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto,
              { description = "jump to urgent client", group = "client"}),
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end,
        { description = "go back", group = "client"}),

    -- awful.key({ modkey,           }, "F4", function (c) c:kill() end,
    --           { description = "close", group = "awesome"}),
    awful.key({ modkey,           }, "F4", function () client.focus:kill() end,
              { description = "close", group = "awesome"}),

    -- Standard program
    awful.key({ modkey,           }, "Return", function () awful.spawn(terminal) end,
              { description = "open a terminal", group = "launcher"}),
    awful.key({ modkey,           }, "d", function () awful.spawn("dmenu_run -i -nf \"#00ffff\" -nb \"#101010\" -sb \"#00ffff\" -sf \"#101010\" -fn  \"xft:Monospace:pixelsize=14:antialias=true:style=regular\" -p 'Run: '") end,
              { description = "dmenu", group = "launcher"}),
    awful.key({ modkey, "Control" }, "f", function () awful.spawn(browser) end,
              { description = "firefox", group = "launcher"}),
    awful.key({ modkey, "Control" }, "m", function () awful.spawn(mail) end,
              { description = "mail", group = "launcher"}),
    awful.key({ modkey, "Control" }, "p", function () awful.spawn("pidgin") end,
              { description = "pidgin", group = "launcher"}),
    awful.key({ modkey, "Control" }, "g", function () awful.spawn("gajim") end,
              { description = "gajim", group = "launcher"}),
    awful.key({ modkey, "Control" }, "v", function () awful.spawn("VirtualBox") end,
              { description = "virtualbox", group = "launcher"}),
    -- awful.key({ modkey,           }, "s", function () awful.spawn("xdotool search --onlyvisible --classname scratchpad windowunmap || xdotool search --classname scratchpad windowmap || urxvt -title \"scratchpad\" -name \"scratchpad\" -geometry 120x40+100+100") end,
    --           { description = "scratchpad", group="launcher"}),
    awful.key({ modkey,           }, "s", function () awful.spawn ("scratchpad.sh") end,
              { description = "scratchpad", group="launcher"}),

    -- awful.key({ modkey, "Control" }, "r", awesome.restart,
    --           {description = "reload awesome", group = "awesome"}),
    awful.key({ modkey,           }, "q", awesome.restart,
              { description = "reload awesome", group = "awesome"}),
    -- awful.key({ modkey, "Shift"   }, "q", awesome.quit,
    --           { description = "quit awesome", group = "awesome"}),
    awful.key({ modkey, "Shift"   }, "q", function () awful.spawn("exit.sh message") end,
              { description = "quit awesome", group = "awesome"}),

    awful.key({ "Control", "Shift"   }, "l", function () awful.spawn("exit.sh lock") end,
              { description = "lock", group = "awesome"}),
    awful.key({ "Control", "Shift"   }, "s", function () awful.spawn("exit.sh monitor_off") end,
              { description = "monitor off", group = "awesome"}),
    awful.key({ "Control", "Shift"   }, "x", function () awful.spawn("exit.sh message") end,
              { description = "message", group = "awesome"}),
    awful.key({ "Control", "Shift"   }, "m", function () awful.spawn("screen_toggle.sh -x") end,
              { description = "screen toggle", group = "awesome"}),

    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)          end,
              { description = "increase master width factor", group = "layout"}),
    awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)          end,
              { description = "decrease master width factor", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1, nil, true) end,
              { description = "increase the number of master clients", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1, nil, true) end,
              { description = "decrease the number of master clients", group = "layout"}),
    awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1, nil, true)    end,
              { description = "increase the number of columns", group = "layout"}),
    awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1, nil, true)    end,
              { description = "decrease the number of columns", group = "layout"}),
    awful.key({ modkey,           }, "space", function () awful.layout.inc( 1)                end,
              { description = "select next", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(-1)                end,
              { description = "select previous", group = "layout"}),

    awful.key({ modkey, "Control" }, "n",
              function ()
                  local c = awful.client.restore()
                  -- Focus restored client
                  if c then
                    c:emit_signal(
                        "request::activate", "key.unminimize", {raise = true}
                    )
                  end
              end,
              { description = "restore minimized", group = "client"}),

    -- Prompt
    awful.key({ modkey },            "r",     function () awful.screen.focused().mypromptbox:run() end,
              { description = "run prompt", group = "launcher"}),

    awful.key({ modkey }, "x",
              function ()
                  awful.prompt.run {
                    prompt       = "Run Lua code: ",
                    textbox      = awful.screen.focused().mypromptbox.widget,
                    exe_callback = awful.util.eval,
                    history_path = awful.util.get_cache_dir() .. "/history_eval"
                  }
              end,
              { description = "lua execute prompt", group = "awesome"}),

    -- Menubar
    awful.key({ modkey }, "p", function() menubar.show() end,
              { description = "show the menubar", group = "launcher"})
)

clientkeys = gears.table.join(
    awful.key({ modkey,           }, "f",
        function (c)
            c.fullscreen = not c.fullscreen
            c:raise()
        end,
        {description = "toggle fullscreen", group = "client"}),
    awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end,
              { description = "close", group = "client"}),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ,
              { description = "toggle floating", group = "client"}),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end,
              { description = "move to master", group = "client"}),
    awful.key({ modkey,           }, "o",      function (c) c:move_to_screen()               end,
              { description = "move to screen", group = "client"}),
    awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end,
              { description = "toggle keep on top", group = "client"}),
    awful.key({ modkey,           }, "n",
        function (c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end ,
        { description = "minimize", group = "client"}),
    awful.key({ modkey,           }, "m",
        function (c)
            c.maximized = not c.maximized
            c:raise()
        end ,
        { description = "(un)maximize", group = "client"}),
    awful.key({ modkey, "Control" }, "m",
        function (c)
            c.maximized_vertical = not c.maximized_vertical
            c:raise()
        end ,
        { description = "(un)maximize vertically", group = "client"}),
    awful.key({ modkey, "Shift"   }, "m",
        function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c:raise()
        end ,
        { description = "(un)maximize horizontally", group = "client"})
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it work on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 10 do
    globalkeys = gears.table.join(globalkeys,
        -- View tag only.
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = awful.screen.focused()
                        local tag = screen.tags[i]
                        if tag then
                           tag:view_only()
                        end
                  end,
                  { description = "view tag #"..i, group = "tag"}),
        -- Toggle tag display.
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = awful.screen.focused()
                      local tag = screen.tags[i]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end,
                  { description = "toggle tag #" .. i, group = "tag"}),
        -- Move client to tag.
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:move_to_tag(tag)
                          end
                     end
                  end,
                  { description = "move focused client to tag #"..i, group = "tag"}),
        -- Toggle tag on focused client.
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:toggle_tag(tag)
                          end
                      end
                  end,
                  { description = "toggle focused client on tag #" .. i, group = "tag"})
    )
end

clientbuttons = gears.table.join(
    awful.button({ }, 1, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
    end),
    awful.button({ modkey }, 1, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
        awful.mouse.client.move(c)
    end),
    awful.button({ modkey }, 3, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
        awful.mouse.client.resize(c)
    end)
)

-- Set keys
root.keys(globalkeys)
-- -----------------------------------------------------------------------------
-- }}}
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- {{{ Rules
-- -----------------------------------------------------------------------------
--
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = {
          border_width = beautiful.border_width,
          border_color = beautiful.border_normal,
          focus = awful.client.focus.filter,
          raise = true,
          keys = clientkeys,
          buttons = clientbuttons,
          screen = awful.screen.preferred,
          size_hints_honor = false,
          honor_workarea = true,
          honor_padding = true,
          placement = awful.placement.no_overlap+awful.placement.no_offscreen
      }
    },

    -- Floating clients.
    { rule_any = {
        instance = {
          "DTA",         -- Firefox addon DownThemAll.
          "copyq",       -- Includes session name in class.
          "pinentry",
          "Exit",        -- exit.sh
          "scratchpad",  -- scratchpad.sh
        },
        class = {
          "Arandr",
          "Blueman-manager",
          "Gpick",
          "Kruler",
          "MessageWin",      -- kalarm.
          "Sxiv",
          "Tor Browser",     -- Needs a fixed window size to avoid fingerprinting by screen size.
          "Wpa_gui",
          "veromix",
          "xtightvncviewer",
          "scratchpad",      -- scratchpad.sh
        },

        -- Note that the name property shown in xprop might be set slightly after creation of the client
        -- and the name shown there might not match defined rules here.
        name = {
          "Event Tester",  -- xev.
        },
        role = {
          "AlarmWindow",   -- Thunderbird's calendar.
          "ConfigManager", -- Thunderbird's about:config.
          "pop-up",        -- e.g. Google Chrome's (detached) Developer Tools.
        }
      }, properties = { floating = true }},

    -- Add titlebars to normal clients and dialogs
    { rule_any = {
          type = { "normal", "dialog" }
      },
      -- properties = { titlebars_enabled = true }
      properties = { titlebars_enabled = false }
    },

    -- Set Firefox to always map on the tag named "2" on screen 1.
    -- { rule = { class = "Firefox" },
    --   properties = { screen = 1, tag = "2" } },

    { rule = { class = "Chromium" }, properties = { tag = "5" } },
    { rule = { class = "Chromium-browser" }, properties = { tag = "5" } },
    { rule = { class = "Chrome" }, properties = { tag = "5" } },
    { rule = { class = "Opera" }, properties = { tag = "5" } },
    { rule = { class = "Firefox" }, properties = { tag = "5" } },
    { rule = { class = "Firefox-esr" }, properties = { tag = "5" } },
    { rule = { class = "Mozilla Firefox" }, properties = { tag = "5" } },
    { rule = { class = "Vivaldi" }, properties = { tag = "5" } },
    { rule = { class = "Vivaldi-stable" }, properties = { tag = "5" } },
    { rule = { class = "Pidgin" }, properties = { tag = "7" } },
    { rule = { class = "Skype" }, properties = { tag = "7" } },
    { rule = { class = "Oracle VM Virtualbox Manager" }, properties = { tag = "8" } },
    { rule = { class = "Evolution" }, properties = { tag = "9" } },
    { rule = { class = "Mozilla Thunderbird" }, properties = { tag = "9" } },
}
-- -----------------------------------------------------------------------------
-- }}}
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- {{{ Signals
-- -----------------------------------------------------------------------------
--
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c)
    -- Set the windows at the slave,
    -- i.e. put it at the end of others instead of setting it master.
    if not awesome.startup then awful.client.setslave(c) end

    if awesome.startup
      and not c.size_hints.user_position
      and not c.size_hints.program_position then
        -- Prevent clients from being unreachable after screen count changes.
        awful.placement.no_offscreen(c)
    end
end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
    -- buttons for the titlebar
    local buttons = gears.table.join(
        awful.button({ }, 1, function()
            c:emit_signal("request::activate", "titlebar", {raise = true})
            awful.mouse.client.move(c)
        end),
        awful.button({ }, 3, function()
            c:emit_signal("request::activate", "titlebar", {raise = true})
            awful.mouse.client.resize(c)
        end)
    )

    awful.titlebar(c) : setup {
        { -- Left
            awful.titlebar.widget.iconwidget(c),
            buttons = buttons,
            layout  = wibox.layout.fixed.horizontal
        },
        { -- Middle
            { -- Title
                align  = "center",
                widget = awful.titlebar.widget.titlewidget(c)
            },
            buttons = buttons,
            -- layout  = wibox.layout.flex.horizontal
            layout  = wibox.layout.fixed.horizontal
        },
        { -- Right
            awful.titlebar.widget.floatingbutton (c),
            awful.titlebar.widget.maximizedbutton(c),
            awful.titlebar.widget.stickybutton   (c),
            awful.titlebar.widget.ontopbutton    (c),
            awful.titlebar.widget.closebutton    (c),
            layout = wibox.layout.fixed.horizontal()
        },
        -- (1)
        layout = wibox.layout.align.horizontal
        -- (2)
        -- layout = wibox.layout.fixed.horizontal,
        -- expand = "none"
    }
end)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
    c:emit_signal("request::activate", "mouse_enter", {raise = false})
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- -----------------------------------------------------------------------------
-- }}}
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- {{{ Notification
-- -----------------------------------------------------------------------------
--
local dpi = xresources.apply_dpi
local last_notification_id

-- Battery notifications
-- The signals are sent by a udev rule.
local last_battery_notification_id

awesome.connect_signal(
  "charger_plugged", function(c)
    notification = naughty.notify({
        title = "Battery status:",
        text = "Charging",
        icon = beautiful.battery_charging_icon,
        timeout = 3,
        replaces_id = last_battery_notification_id
    })
    last_battery_notification_id = notification.id
end)

awesome.connect_signal(
  "charger_unplugged", function(c)
    notification = naughty.notify({
        title = "Battery status:",
        text = "Discharging",
        icon = beautiful.battery_icon,
        timeout = 3,
        replaces_id = last_battery_notification_id
    })
    last_battery_notification_id = notification.id
end)

awesome.connect_signal(
  "battery_full", function(c)
    notification = naughty.notify({
        title = "Battery status:",
        text = "Full!",
        icon = beautiful.battery_icon,
        timeout = 3,
        replaces_id = last_battery_notification_id
    })
    last_battery_notification_id = notification.id
end)

awesome.connect_signal(
  "battery_low", function(c)
    notification = naughty.notify({
        title = "Battery status:",
        text = "Low!",
        icon = beautiful.battery_icon,
        timeout = 5,
        replaces_id = last_battery_notification_id
    })
    last_battery_notification_id = notification.id
end)

awesome.connect_signal(
  "battery_critical", function(c)
    notification = naughty.notify({
        title = "Battery status:",
        text = "Critical! Plug the cable!",
        icon = beautiful.battery_icon,
        timeout = 0,
        replaces_id = last_battery_notification_id
    })
    last_battery_notification_id = notification.id
end)

-- Battery notifications
local function trim(s)
  return s:find'^%s*$' and '' or s:match'^%s*(.*%S)'
end

local function bat_notification()
  local f_capacity = assert(io.open("/sys/class/power_supply/BAT0/capacity", "r"))
  local f_status = assert(io.open("/sys/class/power_supply/BAT0/status", "r"))

  local bat_capacity = tonumber(f_capacity:read("*all"))
  local bat_status = trim(f_status:read("*all"))

  if (bat_capacity <= 10 and bat_status == "Discharging") then
    naughty.notify({ title = "Battery Warning",
                     text = bat_capacity .."%" .. " left.\rPlug in charger!",
                     icon = beautiful.battery_icon,
                     bg = beautiful.notification_crit_bg,
                     fg = beautiful.notification_crit_fg,
                     timeout = 15,
                     position = "top_left"
    })
  end
end

battimer = timer({timeout = 120})
battimer:connect_signal("timeout", bat_notification)
battimer:start()

-- Notifications
local function send_notification(notification_title, notification_text, notification_icon, notification_timeout)
  notification = naughty.notify({
      title = notification_title,
      text = notification_text,
      icon = notification_icon,
      width = dpi(220),
      -- height = dpi(75),
      position = "top_middle",
      icon_size = dpi(50),
      timeout = notification_timeout or 1.5,
      replaces_id = last_notification_id
  })
  last_notification_id = notification.id
end

local s1 = awful.screen.focused()

-- Notification for urgent clients that appear
awful.tag.attached_connect_signal(s1, "property::urgent", function (t)
    send_notification("Client urgent:", "Tag ".. t.index, beautiful.alert_icon, 4)
end)

-- Notification for tag layout change
awful.tag.attached_connect_signal(s1, "property::layout", function ()
    local l = awful.layout.get(s1)
    if l then
        local name = awful.layout.getname(l)
        send_notification("Layout:", name, beautiful["layout_"..name])
    end
end)

-- -----------------------------------------------------------------------------
-- }}}
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- {{{ Rounded corners
-- -----------------------------------------------------------------------------

local helpers = {}

-- Create rounded rectangle shape
helpers.rrect = function(radius)
    return function(cr, width, height)
        gears.shape.rounded_rect(cr, width, height, radius)
    end
end

helpers.rbar = function()
  return function(cr, width, height)
    gears.shape.rounded_bar(cr, width, height)
  end
end

helpers.prrect = function(radius, tl, tr, br, bl)
  return function(cr, width, height)
    gears.shape.partially_rounded_rect(cr, width, height, tl, tr, br, bl, radius)
  end
end

-- Create rectangle shape
helpers.rect = function()
    return function(cr, width, height)
        gears.shape.rectangle(cr, width, height)
    end
end

-- Create circle shape
helpers.circle = function()
    return function(cr, width, height)
        gears.shape.circle(cr, width, height)
    end
end

-- Rounded corners
if beautiful.border_radius ~= 0 then
    client.connect_signal("manage", function (c, startup)
        if not c.fullscreen then
            c.shape = helpers.rrect(beautiful.border_radius)
        end
    end)

    -- Fullscreen clients should not have rounded corners
    client.connect_signal("property::fullscreen", function (c)
        if c.fullscreen then
            c.shape = helpers.rect()
        else
            c.shape = helpers.rrect(beautiful.border_radius)
        end
    end)
end
-- -----------------------------------------------------------------------------
-- }}}
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- {{{ Startup applications
-- -----------------------------------------------------------------------------
--
-- Startup applications
awful.spawn.with_shell(os.getenv("HOME") .. "/.config/awesome/screen_toggle.sh -x")
awful.spawn.with_shell(os.getenv("HOME") .. "/.config/awesome/trayer.sh")
-- -----------------------------------------------------------------------------
-- }}}
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- END
-- -----------------------------------------------------------------------------
