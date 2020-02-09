-- -----------------------------------------------------------------------------
-- {{{ notification
-- -----------------------------------------------------------------------------

-- Standard awesome library
local gears = require("gears")
local awful = require("awful")

-- Theme handling library
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")

-- Notification library
local naughty = require ("naughty")

-- Local
local helpers = require("helpers")

local dpi = xresources.apply_dpi
local last_notification_id


-- {{{ Notification
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
        naughty.notify({
            title = "Battery Warning",
            text = bat_capacity .."%" .. " left.\rPlug in charger!",
            icon = beautiful.battery_icon,
            bg = beautiful.notification_crit_bg,
            fg = beautiful.notification_crit_fg,
            timeout = 15,
            position = "top_left"
        })
    end
end

local battimer = gears.timer({timeout = 120})
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
-- }}}

-- -----------------------------------------------------------------------------
-- }}}
-- -----------------------------------------------------------------------------
