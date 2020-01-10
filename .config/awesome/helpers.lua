-- -----------------------------------------------------------------------------
-- {{{ helpers
-- -----------------------------------------------------------------------------

local gears = require("gears")
local awful = require("awful")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local wibox = require("wibox")

local dpi = xresources.apply_dpi

local helpers = {}

-- {{{ Helper functions
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

-- Create info bubble shape
-- TODO
helpers.infobubble = function(radius)
  return function(cr, width, height)
    gears.shape.infobubble(cr, width, height, radius)
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

function helpers.colorize_text(txt, fg)
    return "<span foreground='" .. fg .."'>" .. txt .. "</span>"
end

function helpers.client_menu_toggle()
    local instance = nil

    return function ()
        if instance and instance.wibox.visible then
            instance:hide()
            instance = nil
        else
            instance = awful.menu.clients({ theme = { width = dpi(250) } })
        end
    end
end

function helpers.pad(size)
    local str = ""
    for i = 1, size do
        str = str .. " "
    end
    local pad = wibox.widget.textbox(str)
    return pad
end

function helpers.move_to_edge(c, direction)
    local workarea = awful.screen.focused().workarea
    local client_geometry = c:geometry()
    if direction == "up" then
        c:geometry({ nil, y = workarea.y + beautiful.screen_margin * 2, nil, nil })
    elseif direction == "down" then
        c:geometry({ nil, y = workarea.height + workarea.y - client_geometry.height - beautiful.screen_margin * 2 - beautiful.border_width * 2, nil, nil })
    elseif direction == "left" then
        c:geometry({ x = workarea.x + beautiful.screen_margin * 2, nil, nil, nil })
    elseif direction == "right" then
        c:geometry({ x = workarea.width + workarea.x - client_geometry.width - beautiful.screen_margin * 2 - beautiful.border_width * 2, nil, nil, nil })
    end
end

function helpers.create_titlebar(c, titlebar_buttons, titlebar_position, titlebar_size)
    awful.titlebar(c, {font = beautiful.titlebar_font, position = titlebar_position, size = titlebar_size}) : setup {
        {
            buttons = titlebar_buttons,
            layout  = wibox.layout.fixed.horizontal
        },
        {
            buttons = titlebar_buttons,
            layout  = wibox.layout.fixed.horizontal
        },
        {
            buttons = titlebar_buttons,
            layout = wibox.layout.fixed.horizontal
        },
        layout = wibox.layout.align.horizontal
    }
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
-- }}}

return helpers

-- -----------------------------------------------------------------------------
-- }}}
-- -----------------------------------------------------------------------------
