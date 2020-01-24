-- -----------------------------------------------------------------------------
-- {{{ signals
-- -----------------------------------------------------------------------------

-- Standard awesome library
local gears = require("gears")
local awful = require("awful")

-- Widget and layout library
local wibox = require("wibox")

-- Theme handling library
local beautiful = require("beautiful")

-- Local
local helpers  = require("helpers")


local signals = {}

-- {{{ Signals
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

-- Hide titlebars if required by the theme
client.connect_signal("manage", function (c)
    if not beautiful.titlebars_enabled then
        awful.titlebar.hide(c)
    end
end)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
    c:emit_signal("request::activate", "mouse_enter", {raise = false})
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)

-- Rounded corners
if beautiful.border_radius ~= 0 then
    client.connect_signal("manage", function (c, startup)
        if not c.fullscreen then
            c.shape = helpers.rrect(beautiful.border_radius)
        end
    end)

    -- -- Fullscreen clients should not have rounded corners
    -- client.connect_signal("property::fullscreen", function (c)
    --     if c.fullscreen then
    --         c.shape = helpers.rect()
    --     else
    --         c.shape = helpers.rrect(beautiful.border_radius)
    --     end
    -- end)

    -- Fullscreen & maximised clients should not have rounded corners
    local function no_round_corners (c)
        if c.fullscreen or c.maximized then
            c.shape = helpers.rect()
        else
            c.shape = helpers.rrect(beautiful.border_radius)
        end
    end

    client.connect_signal("property::fullscreen", no_round_corners)
    client.connect_signal("property::maximized", no_round_corners)
end

-- If the layout is not floating, every floating client that appears is centered
-- If the layout is floating, and there is no other client visible, center it
client.connect_signal("manage", function (c)
    if not awesome.startup then
        if awful.layout.get(mouse.screen) ~= awful.layout.suit.floating then
            awful.placement.centered(c,{honor_workarea=true})
        else if #mouse.screen.clients == 1 then
                awful.placement.centered(c,{honor_workarea=true})
            end
        end
    end
end)

-- When a client starts up in fullscreen, resize it to cover the fullscreen a short moment later
-- Fixes wrong geometry when titlebars are enabled
client.connect_signal("manage", function(c)
    if c.fullscreen then
        gears.timer.delayed_call(function()
            if c.valid then
                c:geometry(c.screen.geometry)
            end
        end)
    end
end)

-- Set mouse resize mode (live or after)
awful.mouse.resize.set_mode("live")
-- }}}

return signals

-- -----------------------------------------------------------------------------
-- }}}
-- -----------------------------------------------------------------------------
