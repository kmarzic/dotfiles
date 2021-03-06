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


-- {{{ Signals
-- -- Signal function to execute when a new client appears.
-- client.connect_signal("manage", function (c)
--     -- Set the windows at the slave,
--     -- i.e. put it at the end of others instead of setting it master.
--     -- if not awesome.startup then awful.client.setslave(c) end
--     if not awesome.startup then awful.client.setslave(c) end
-- 
--     if awesome.startup
--       and not c.size_hints.user_position
--       and not c.size_hints.program_position then
--         -- Prevent clients from being unreachable after screen count changes.
--         awful.placement.no_offscreen(c)
--     end
-- end)

-- Hide titlebars if required by the theme
client.connect_signal("manage", function (c)
    if not beautiful.titlebars_enabled then
        awful.titlebar.hide(c)
    end
end)

-- Enable sloppy focus, so that focus follows mouse.
-- client.connect_signal("mouse::enter", function(c)
--     c:emit_signal("request::activate", "mouse_enter", {raise = false})
-- end)
client.connect_signal("mouse::enter", function(c)
    c:activate { context = "mouse_enter", raise = false }
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

-- Removing screens: Moving all the nonempty tags to a different screen and make them volatile
tag.connect_signal("request::screen", function(t)
    clients = t:clients()
    for s in screen do
        if s ~= t.screen and clients and next(clients) then
            t.screen = s
            t.name = t.name .. "'"
            awful.tag.setvolatile(true, t)
            return
        end
    end
end)

-- -- Set mouse resize mode (live or after)
-- awful.mouse.resize.set_mode("live")
-- }}}

-- -----------------------------------------------------------------------------
-- }}}
-- -----------------------------------------------------------------------------
