-- -----------------------------------------------------------------------------
-- {{{ bindings
-- -----------------------------------------------------------------------------

-- Standard awesome library
local awful = require("awful")
require("awful.autofocus")

-- Notification library
local menubar = require("menubar")

-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
local hotkeys_popup = require("awful.hotkeys_popup")
require("awful.hotkeys_popup.keys")

-- Local
local helpers  = require("helpers")


-- {{{ Bindings

-- Default modkey
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
--
-- Mod4     Also called Super, Windows and Command ⌘
-- Mod1     Usually called Alt on PCs and Option on Macs
-- Shift    Both left and right shift keys
-- Control  Also called CTRL on some keyboards
--
-- modkey = "Mod4" -- meta
local modkey = "Mod1" -- alt
local winkey = "Mod4" -- windows key

-- {{{ Mouse bindings
awful.mouse.append_global_mousebindings({
    awful.button({ }, 3, function () mymainmenu:toggle() end),
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev),
})
-- }}}

-- {{ Key bindings
-- General Awesome keys
awful.keyboard.append_global_keybindings({
    -- Help
    awful.key({ modkey, "Shift"   }, "/",      hotkeys_popup.show_help,
              { description = "show help", group="awesome"} ),

    -- Menu
    awful.key({ modkey, "Control" }, "w", function () mymainmenu:show() end,
              { description = "show main menu", group = "awesome"}),

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

    -- Standard program
    awful.key({ modkey,           }, "Return", function () awful.spawn(terminal) end,
              { description = "open a terminal", group = "launcher"}),

    -- Menubar
    awful.key({ modkey,           }, "d", function () awful.spawn("dmenu_run -i -nf \"#00ffff\" -nb \"#101010\" -sb \"#00ffff\" -sf \"#101010\" -fn  \"xft:Monospace:pixelsize=14:antialias=true:style=regular\" -p 'Run: '") end,
              { description = "dmenu", group = "launcher"}),
    -- awful.key({ modkey,           }, "d", function () awful.spawn("rofi -show run") end,
    --           { description = "dmenu", group = "launcher"}),
    awful.key({                   }, "Menu", function () awful.spawn('rofi -show run') end,
              { description = "open rofi", group = "launcher"}),
})

-- Tags related keybindings
awful.keyboard.append_global_keybindings({
    -- History
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore,
              { description = "go back", group = "tag"} ),

    -- Tag (virtual workspace) Browsing / View tag (virtual workspace)
    awful.key({ modkey,           }, "Left",   awful.tag.viewprev,
              { description = "view previous", group = "tag"} ),
    awful.key({ modkey,           }, "Right",  awful.tag.viewnext,
              { description = "view next", group = "tag"} ),

    awful.key({ modkey,           }, "[",   awful.tag.viewprev,
              { description = "view previous", group = "tag"} ),
    awful.key({ modkey,           }, "]",  awful.tag.viewnext,
              { description = "view next", group = "tag"} ),
})

-- Focus related keybindings
awful.keyboard.append_global_keybindings({
    -- Focus index on tag (virtual workspace)
    awful.key({ modkey,           }, "j", function () awful.client.focus.byidx( 1) end,
              { description = "focus next by index", group = "client"} ),
    awful.key({ modkey,           }, "k", function () awful.client.focus.byidx(-1) end,
              { description = "focus previous by index", group = "client"} ),

    -- MOD-Tab
    --
    -- (1) default - cycle through previous windows
    -- awful.key({ modkey,           }, "Tab",
    --     function ()
    --         awful.client.focus.history.previous()
    --         if client.focus then
    --             client.focus:raise()
    --         end
    --     end,
    --     { description = "go back", group = "client"}
    -- ),
    --
    -- (2) cycle through all the windows
    awful.key({ modkey,           }, "Tab",
        function ()
            -- awful.client.focus.history.previous()
            awful.client.focus.byidx(-1)
            if client.focus then
                client.focus:raise()
            end
        end,
        { description = "go forward", group = "client"}
    ),
    awful.key({ modkey, "Shift"   }, "Tab",
        function ()
            -- awful.client.focus.history.previous()
            awful.client.focus.byidx(1)
            if client.focus then
                client.focus:raise()
            end
        end,
        { description = "go back", group = "client"}
    ),
    --
    -- (3) cycle through every client on a tag, even minimized ones
    -- awful.key({ modkey,           }, "Tab",
    --     function ()
    --         for c in awful.client.iterate(function (x) return true end) do
    --             client.focus = c
    --             client.focus:raise()
    --         end
    --     end,
    --     { description = "go back", group = "client"}
    -- ),

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
})

-- Layout related keybindings
awful.keyboard.append_global_keybindings({
    -- Focus index on tag (virtual workspace)
    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end,
              { description = "swap with next client by index", group = "client"}),
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end,
              { description = "swap with previous client by index", group = "client"}),

    -- Resize index on tag (virtual workspace)
    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)          end,
              { description = "increase master width factor", group = "layout"}),
    awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)          end,
              { description = "decrease master width factor", group = "layout"}),

    -- Increase / decrease clients on tag (virtual workspace)
    awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1, nil, true) end,
              { description = "increase the number of master clients", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1, nil, true) end,
              { description = "decrease the number of master clients", group = "layout"}),

    -- Increase / decrease columns on tag (virtual workspace)
    awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1, nil, true)    end,
              { description = "increase the number of columns", group = "layout"}),
    awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1, nil, true)    end,
              { description = "decrease the number of columns", group = "layout"}),

    -- Layout change
    awful.key({ modkey,           }, "space", function () awful.layout.inc( 1)                end,
              { description = "select next", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(-1)                end,
              { description = "select previous", group = "layout"}),
})

awful.keyboard.append_global_keybindings({
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


    awful.key({ modkey, "Control" }, "f", function () awful.spawn(browser) end,
              { description = "firefox", group = "launcher"}),
    awful.key({ modkey, "Control" }, "m", function () awful.spawn(mail) end,
              { description = "mail", group = "launcher"}),
    awful.key({ modkey, "Control" }, "p", function () awful.spawn("pidgin") end,
              { description = "pidgin", group = "launcher"}),
    awful.key({ modkey, "Control" }, "g", function () awful.spawn("gajim") end,
              { description = "gajim", group = "launcher"}),
    awful.key({ modkey, "Control" }, "t", function () awful.spawn("teams") end,
              { description = "teams", group = "launcher"}),
    awful.key({ modkey, "Control" }, "v", function () awful.spawn("VirtualBox") end,
              { description = "virtualbox", group = "launcher"}),
    -- awful.key({ modkey,           }, "s", function () awful.spawn("xdotool search --onlyvisible --classname scratchpad windowunmap || xdotool search --classname scratchpad windowmap || urxvt -title \"scratchpad\" -name \"scratchpad\" -geometry 120x40+100+100") end,
    --           { description = "scratchpad", group="launcher"}),
    awful.key({ modkey,           }, "s", function () awful.spawn.with_shell ("scratchpad.sh") end,
              { description = "scratchpad", group="launcher"}),
    -- awful.key({ modkey,           }, "s", function () scratch.toggle 'urxvt -title "scratchpad" -name "scratchpad" -geometry 140x40+100-100 -fn "xft:Monospace:pixelsize=14:antialias=true:style=bold"' end,
    --           { description = "scratchpad", group="launcher"}),
    -- awful.key({ modkey,           }, "s", function () scratch.drop("urxvt", "center", "center", 0.60, 0.50, true, mouse.screen) end,
    --           { description = "scratchpad", group="launcher"}),

    awful.key({ "Control", "Shift"   }, "l", function () awful.spawn.with_shell("exit.sh lock") end,
              { description = "lock", group = "awesome"}),
    awful.key({ "Control", "Shift"   }, "s", function () awful.spawn.with_shell("exit.sh monitor_off") end,
              { description = "monitor off", group = "awesome"}),
    awful.key({ "Control", "Shift"   }, "x", function () awful.spawn.with_shell("exit.sh message") end,
              { description = "message", group = "awesome"}),
    awful.key({ "Control", "Shift"   }, "m", function () awful.spawn.with_shell("screen_toggle.sh -x") end,
              { description = "screen toggle", group = "awesome"}),
    -- awful.key({ "Control", "Shift"   }, "m", function() xrandr.xrandr() end,
    --           { description = "screen toggle", group = "awesome"}),
    -- awful.key({ "Control", "Shift"   }, "m", foggy.menu,
    --           { description = "screen toggle", group = "awesome"}),

    -- Screenshots
    awful.key({                   }, "Print", function () awful.spawn.with_shell ("~/bin/screenshot.sh") end,
              { description = "select area to capture", group="screenshots"}),
    awful.key({ modkey,           }, "Print", function () awful.spawn.with_shell ("~/bin/screenshot.sh") end,
              { description = "select area to capture", group="screenshots"}),

    -- Toggle Systray
    awful.key({ modkey            }, "=", function () awful.screen.focused().systray.visible = not awful.screen.focused().systray.visible end,
              { description = "toggle systray visibility", group = "custom"}),

    -- Toggle Stats
    awful.key({ modkey            }, "-", function () awful.screen.focused().stats0.visible = not awful.screen.focused().stats0.visible end,
              { description = "toggle stats visibility", group = "custom"}),

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


    -- Menubar
    awful.key({ modkey }, "p", function() menubar.show() end,
              { description = "show the menubar", group = "launcher"})
})

awful.keyboard.append_global_keybindings({
    awful.key {
        modifiers   = { modkey },
        keygroup    = "numrow",
        description = "only view tag",
        group       = "tag",
        on_press    = function (index)
            local screen = awful.screen.focused()
            local tag = screen.tags[index]
            if tag then
                tag:view_only()
            end
        end,
    },
    awful.key {
        modifiers   = { modkey, "Control" },
        keygroup    = "numrow",
        description = "toggle tag",
        group       = "tag",
        on_press    = function (index)
            local screen = awful.screen.focused()
            local tag = screen.tags[index]
            if tag then
                awful.tag.viewtoggle(tag)
            end
        end,
    },
    awful.key {
        modifiers = { modkey, "Shift" },
        keygroup    = "numrow",
        description = "move focused client to tag",
        group       = "tag",
        on_press    = function (index)
            if client.focus then
                local tag = client.focus.screen.tags[index]
                if tag then
                    client.focus:move_to_tag(tag)
                end
            end
        end,
    },
    awful.key {
        modifiers   = { modkey, "Control", "Shift" },
        keygroup    = "numrow",
        description = "toggle focused client on tag",
        group       = "tag",
        on_press    = function (index)
            if client.focus then
                local tag = client.focus.screen.tags[index]
                if tag then
                    client.focus:toggle_tag(tag)
                end
            end
        end,
    }
})

client.connect_signal("request::default_mousebindings", function()
    awful.mouse.append_client_mousebindings({
        awful.button({ }, 1, function (c)
            c:activate { context = "mouse_click" }
        end),
        awful.button({ modkey }, 1, function (c)
            c:activate { context = "mouse_click", action = "mouse_move"  }
        end),
        awful.button({ modkey }, 3, function (c)
            c:activate { context = "mouse_click", action = "mouse_resize"}
        end),
    })
end)

client.connect_signal("request::default_keybindings", function()
    awful.keyboard.append_client_keybindings({
        awful.key({ modkey,           }, "f",
            function (c)
                c.fullscreen = not c.fullscreen
                c:raise()
            end,
            {description = "toggle fullscreen", group = "client"}),
        awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end,
                {description = "close", group = "client"}),
        awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ,
                {description = "toggle floating", group = "client"}),
        awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end,
                {description = "move to master", group = "client"}),
        awful.key({ modkey,           }, "o",      function (c) c:move_to_screen()               end,
                {description = "move to screen", group = "client"}),
        awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end,
                {description = "toggle keep on top", group = "client"}),
        awful.key({ modkey,           }, "n",
            function (c)
                -- The client currently has the input focus, so it cannot be
                -- minimized, since minimized clients can't have the focus.
                c.minimized = true
            end ,
            {description = "minimize", group = "client"}),
        awful.key({ modkey,           }, "m",
            function (c)
                c.maximized = not c.maximized
                c:raise()
            end ,
            {description = "(un)maximize", group = "client"}),
        awful.key({ modkey, "Control" }, "m",
            function (c)
                c.maximized_vertical = not c.maximized_vertical
                c:raise()
            end ,
            {description = "(un)maximize vertically", group = "client"}),
        awful.key({ modkey, "Shift"   }, "m",
            function (c)
                c.maximized_horizontal = not c.maximized_horizontal
                c:raise()
            end ,
            {description = "(un)maximize horizontally", group = "client"}),
    })
end)
-- }}}

-- -----------------------------------------------------------------------------
-- }}}
-- -----------------------------------------------------------------------------
