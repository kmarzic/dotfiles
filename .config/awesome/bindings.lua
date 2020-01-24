-- -----------------------------------------------------------------------------
-- {{{ Bindings
-- -----------------------------------------------------------------------------

-- Standard awesome library
local gears = require("gears")
local awful = require("awful")

-- Notification library
local menubar = require("menubar")

local hotkeys_popup = require("awful.hotkeys_popup")
require("awful.hotkeys_popup.keys")

-- Local
local helpers  = require("helpers")


local bindings = {}

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
-- modkey = "Mod4" -- meta
local modkey = "Mod1" -- alt
local winkey = "Mod4" -- windows key

-- {{{ Mouse bindings
bindings.mouse = {
    global = gears.table.join(
        awful.button({ }, 3, function () mymainmenu:toggle() end),
        awful.button({ }, 4, awful.tag.viewnext),
        awful.button({ }, 5, awful.tag.viewprev)
    ),

    -- clientbuttons = gears.table.join(
    --     awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    --     awful.button({ modkey }, 1, awful.mouse.client.move),
    --     awful.button({ modkey }, 3, awful.mouse.client.resize)
    -- )

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
}
-- }}}

-- {{ Key bindings
bindings.globalkeys = gears.table.join(
    -- Help
    awful.key({ modkey, "Shift"   }, "/",      hotkeys_popup.show_help,
              { description = "show help", group="awesome"} ),

    -- Menu
    awful.key({ modkey, "Control" }, "w", function () mymainmenu:show() end,
              { description = "show main menu", group = "awesome"}),

    -- History
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore,
              { description = "go back", group = "tag"} ),

    -- Restart
    -- awful.key({ modkey, "Control" }, "r", awesome.restart,
    --           {description = "reload awesome", group = "awesome"}),
    awful.key({ modkey,           }, "q", awesome.restart,
              { description = "reload awesome", group = "awesome"}),

    -- Quit
    -- awful.key({ modkey, "Shift"   }, "q", awesome.quit,
    --           { description = "quit awesome", group = "awesome"}),
    awful.key({ modkey, "Shift"   }, "q", function () awful.spawn("exit.sh message") end,
              { description = "quit awesome", group = "awesome"}),

    -- View tag (virtual workspace)
    awful.key({ modkey,           }, "Left",   awful.tag.viewprev,
              { description = "view previous", group = "tag"} ),
    awful.key({ modkey,           }, "Right",  awful.tag.viewnext,
              { description = "view next", group = "tag"} ),
    awful.key({ modkey,           }, "[",   awful.tag.viewprev,
              { description = "view previous", group = "tag"} ),
    awful.key({ modkey,           }, "]",  awful.tag.viewnext,
              { description = "view next", group = "tag"} ),

    -- Resize index on tag (virtual workspace)
    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)          end,
              { description = "increase master width factor", group = "layout"}),
    awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)          end,
              { description = "decrease master width factor", group = "layout"}),

    -- Increase / decrease columns on tag (virtual workspace)
    awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1, nil, true)    end,
              { description = "increase the number of columns", group = "layout"}),
    awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1, nil, true)    end,
              { description = "decrease the number of columns", group = "layout"}),

    -- Increase / decrease clients on tag (virtual workspace)
    awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1, nil, true) end,
              { description = "increase the number of master clients", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1, nil, true) end,
              { description = "decrease the number of master clients", group = "layout"}),

    -- Focus index on tag (virtual workspace)
    awful.key({ modkey,           }, "j", function () awful.client.focus.byidx( 1) end,
              { description = "focus next by index", group = "client"} ),
    awful.key({ modkey,           }, "k", function () awful.client.focus.byidx(-1) end,
              { description = "focus previous by index", group = "client"} ),

    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end,
              { description = "swap with next client by index", group = "client"}),
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end,
              { description = "swap with previous client by index", group = "client"}),

    -- Screen manipulation
    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end,
              { description = "focus the next screen", group = "screen"}),
    awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end,
              { description = "focus the previous screen", group = "screen"}),

    awful.key({ modkey,           }, ".", function () awful.screen.focus_relative( 1) end,
              { description = "focus the next screen", group = "screen"}),
    awful.key({ modkey,           }, ",", function () awful.screen.focus_relative(-1) end,
              { description = "focus the previous screen", group = "screen"}),

    awful.key({ modkey,           }, "w", function () awful.screen.focus(1) end,
              { description = "switch to physical/Xinerama screen 1", group = "screen"}),
    awful.key({ modkey,           }, "e", function () awful.screen.focus(2) end,
              { description = "switch to physical/Xinerama screen 2", group = "screen"}),
    awful.key({ modkey,           }, "r", function () awful.screen.focus(3) end,
              { description = "switch to physical/Xinerama screen 3", group = "screen"}),

    -- Move index to screen
    awful.key({ modkey, "Shift"   }, ",",
        function()
            local ns = client.focus.screen.index - 1
            awful.client.movetoscreen(c, ns)
        end,
        { description = "move to screen on the left", group = "client"}
    ),

    awful.key({ modkey, "Shift"   }, ".",
        function()
            local ns = client.focus.screen.index + 1
            awful.client.movetoscreen(c, ns)
        end,
        { description = "move to screen on the right", group = "client" }
    ),

    -- Urgent clients
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto,
              { description = "jump to urgent client", group = "client"}),

    -- MOD-Tab
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end,
        { description = "go back", group = "client"}
    ),

    -- Close index
    -- awful.key({ modkey,           }, "F4", function (c) c:kill() end,
    --           { description = "close", group = "awesome"}),
    awful.key({ modkey,           }, "F4", function () client.focus:kill() end,
              { description = "close", group = "awesome"}),

    -- Set floating layout
    awful.key({ modkey, }, "t", function() awful.layout.set(awful.layout.suit.floating) end,
              { description = "set floating layout", group = "tag"} ),

    -- Layout manipulation
    awful.key({ modkey, "Shift"   }, "Down",
        function ()
            local current_layout = awful.layout.getname(awful.layout.get(awful.screen.focused()))
            local c = client.focus
            -- Floating: move client to edge
            if c ~= nil and (current_layout == "floating" or c.floating) then
                helpers.move_to_edge(c, "down")
            else
                --awful.client.swap.byidx(  1)
                awful.client.swap.bydirection("down", c, nil)
            end
        end,
        { description = "swap with direction down", group = "client"}
    ),

    awful.key({ modkey, "Shift"   }, "Up",
        function ()
            local current_layout = awful.layout.getname(awful.layout.get(awful.screen.focused()))
            local c = client.focus
            -- Floating: move client to edge
            if c ~= nil and (current_layout == "floating" or c.floating) then
                --c:relative_move(  0,  -40,   0,   0)
                helpers.move_to_edge(c, "up")
            else
                --awful.client.swap.byidx( -1)
                awful.client.swap.bydirection("up", c, nil)
            end
        end,
        { description = "swap with direction up", group = "client"}
    ),

    awful.key({ modkey, "Shift"   }, "Left",
        function ()
            local current_layout = awful.layout.getname(awful.layout.get(awful.screen.focused()))
            local c = client.focus
            -- Floating: move client to edge
            if c ~= nil and (current_layout == "floating" or c.floating) then
                --c:relative_move( -40,  0,   0,   0)
                helpers.move_to_edge(c, "left")
            else
                awful.client.swap.bydirection("left", c, nil)
            end
        end,
        { description = "swap with direction left", group = "client"}
    ),

    awful.key({ modkey, "Shift"   }, "Right",
        function ()
            local current_layout = awful.layout.getname(awful.layout.get(awful.screen.focused()))
            local c = client.focus
            -- Floating: move client to edge
            if c ~= nil and (current_layout == "floating" or c.floating) then
                --c:relative_move(  40,  0,   0,   0)
                helpers.move_to_edge(c, "right")
            else
                awful.client.swap.bydirection("right", c, nil)
            end
        end,
        { description = "swap with direction right", group = "client"}
    ),

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

    awful.key({ "Control", "Shift"   }, "l", function () awful.spawn("exit.sh lock") end,
              { description = "lock", group = "awesome"}),
    awful.key({ "Control", "Shift"   }, "s", function () awful.spawn("exit.sh monitor_off") end,
              { description = "monitor off", group = "awesome"}),
    awful.key({ "Control", "Shift"   }, "x", function () awful.spawn("exit.sh message") end,
              { description = "message", group = "awesome"}),
    awful.key({ "Control", "Shift"   }, "m", function () awful.spawn("screen_toggle.sh -x") end,
              { description = "screen toggle", group = "awesome"}),

    -- Layout change
    awful.key({ modkey,           }, "space", function () awful.layout.inc( 1)                end,
              { description = "select next", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(-1)                end,
              { description = "select previous", group = "layout"}),

    -- Toggle Systray
    awful.key({ modkey            }, "=", function () awful.screen.focused().systray.visible = not awful.screen.focused().systray.visible end,
              { description = "toggle systray visibility", group = "custom"}),

    -- Toggle Stats
    awful.key({ modkey            }, "-", function () awful.screen.focused().stats.visible = not awful.screen.focused().stats.visible end,
              { description = "toggle stats visibility", group = "awesome"}),

    -- Brightness
    awful.key( { }, "XF86MonBrightnessDown",
        function()
            awful.spawn.easy_async_with_shell("light -U 10", function()
                awesome.emit_signal("brightness_changed")
            end)
        end,
        { description = "decrease brightness", group = "brightness"}
    ),
    awful.key( { }, "XF86MonBrightnessUp",
        function()
            awful.spawn.easy_async_with_shell("light -A 10", function()
                awesome.emit_signal("brightness_changed")
            end)
        end,
        { description = "increase brightness", group = "brightness"}
    ),
    awful.key( { }, "XF86ScreenSaver",
        function()
            awful.spawn.with_shell("sleep 1; xset dpms force off")
        end,
        { description = "increase brightness", group = "brightness"}
    ),

    -- Volume Control
    -- Sink is 0
    awful.key( { }, "XF86AudioMute",
        function()
            awful.spawn.with_shell("amixer sset Master 1+ toggle")
            --awful.spawn.with_shell("pactl set-sink-mute 1 toggle")
        end,
        { description = "(un)mute volume", group = "volume"}
    ),
    awful.key( { }, "XF86AudioLowerVolume",
        function()
            awful.spawn.with_shell("amixer sset Master 1+ 10%-")
            --awful.spawn.with_shell("pactl set-sink-mute 1 0; pactl set-sink-volume 1 -10%")
        end,
        { description = "lower volume", group = "volume"}
    ),
    awful.key( { }, "XF86AudioRaiseVolume",
        function()
            awful.spawn.with_shell("amixer sset Master 1+ 10%+")
            --awful.spawn.with_shell("pactl set-sink-mute 1 0; pactl set-sink-volume 1 +10%")
        end,
        { description = "raise volume", group = "volume"}
    ),

    -- Prompt
    awful.key({ modkey            }, "r",     function () awful.screen.focused().mypromptbox:run() end,
              { description = "run prompt", group = "launcher"}),

    -- Lua Run Code
    awful.key({ modkey            }, "x",
        function ()
            awful.prompt.run {
                prompt       = "Run Lua code: ",
                textbox      = awful.screen.focused().mypromptbox.widget,
                exe_callback = awful.util.eval,
                history_path = awful.util.get_cache_dir() .. "/history_eval"
            }
        end,
        { description = "lua execute prompt", group = "awesome"}
    ),

    -- Menubar
    awful.key({ modkey }, "p", function() menubar.show() end,
              { description = "show the menubar", group = "launcher"})
)

bindings.clientkeys = gears.table.join(
    -- Toggle fullscreen
    awful.key({ modkey,           }, "f",
        function (c)
            c.fullscreen = not c.fullscreen
            c:raise()
        end,
        { description = "toggle fullscreen", group = "client"}
    ),

    -- Close
    awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end,
              { description = "close", group = "client"}),

    -- Toggle floating
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ,
              { description = "toggle floating", group = "client"}),

    -- Move to master
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end,
              { description = "move to master", group = "client"}),

    -- Move to screen
    awful.key({ modkey,           }, "o",      function (c) c:move_to_screen()               end,
              { description = "move to screen", group = "client"}),

    -- Toggle keep on top
    awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end,
              { description = "toggle keep on top", group = "client"}),

    -- Minimize
    awful.key({ modkey,           }, "n",
        function (c)
            -- naughty.notify({text="minimize"})
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end ,
        { description = "minimize", group = "client"}
    ),

    -- Unminimize
    awful.key({ modkey, "Control" }, "n",
        function ()
            -- naughty.notify({text="restore minimized"})
            local c = awful.client.restore()
            -- Focus restored client
            if c then
                c:emit_signal(
                "request::activate", "key.unminimize", {raise = true}
                )
            end
        end,
        { description = "restore minimized", group = "client"}
    ),

    -- UnMaximaze
    awful.key({ modkey,           }, "m",
        function (c)
            -- naughty.notify({text="maximize"})
            c.maximized = not c.maximized
            c:raise()
        end ,
        { description = "(un)maximize", group = "client"}
    ),

    -- (Un)Maximize
    awful.key({ modkey, "Control" }, "m",
        function (c)
            c.maximized_vertical = not c.maximized_vertical
            c:raise()
        end ,
        { description = "(un)maximize vertically", group = "client"}
    ),

    -- (Un)Maximize horizontally
    awful.key({ modkey, "Shift"   }, "m",
        function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c:raise()
        end ,
        { description = "(un)maximize horizontally", group = "client"}
    )
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it work on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 10 do
    bindings.globalkeys = gears.table.join(bindings.globalkeys,
        -- View tag only.
        awful.key({ modkey }, "#" .. i + 9,
            function ()
                  local screen = awful.screen.focused()
                  local tag = screen.tags[i]
                  if tag then
                     tag:view_only()
                  end
            end,
            { description = "view tag #"..i, group = "tag"}
        ),
        -- Toggle tag display.
        awful.key({ modkey, "Control" }, "#" .. i + 9,
            function ()
                local screen = awful.screen.focused()
                local tag = screen.tags[i]
                if tag then
                   awful.tag.viewtoggle(tag)
                end
            end,
            { description = "toggle tag #" .. i, group = "tag"}
        ),
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
            { description = "move focused client to tag #"..i, group = "tag"}
        ),
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
            { description = "toggle focused client on tag #" .. i, group = "tag"}
        )
    )
end

-- Set keys
root.buttons(bindings.mouse.global)
root.keys(bindings.globalkeys)
-- }}}

return bindings

-- -----------------------------------------------------------------------------
-- }}}
-- -----------------------------------------------------------------------------
