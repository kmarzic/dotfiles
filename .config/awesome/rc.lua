-- -----------------------------------------------------------------------------
-- {{{ rc.lua
-- default: https://awesomewm.org/doc/api/sample%20files/rc.lua.html
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

-- Declarative object management
local ruled = require("ruled")
local menubar = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup")
-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
require("awful.hotkeys_popup.keys")

local config_path = os.getenv("HOME") .. "/.config/awesome/"
local theme_dir = os.getenv("HOME") .. "/.config/awesome/themes/"

-- Error handling
dofile(config_path .. "error.lua")

-- Variables
-- terminal = "urxvt"
terminal = "st"
awful.util.terminal = terminal
browser = "firefox"
mail = "evolution"
messenger = "gajim"
editor = os.getenv("EDITOR") or "vim"
editor_cmd = terminal .. " -e " .. editor

-- Themes define colours, icons, font and wallpapers.
-- local theme_name = "default"
local theme_name = "ansi"
-- local theme_name = "gtk"
-- local theme_name = "sky"
-- local theme_name = "xresources"
-- local theme_name = "solarized.dark"
-- local theme_name = "solarized.light"
-- local theme_name = "zenburn"
beautiful.init( theme_dir .. theme_name .. "/theme.lua" )

-- Local
local bindings  = require("bindings")
local helpers   = require("helpers")
local layouts   = require("layouts")
local menu      = require("menu")
local notifs    = require("notifications")
local rules     = require("rules")
local signals   = require("signals")
local titlebars = require("titlebars")
local wibar     = require("wibar")

-- -- Sharedtags
-- -- https://github.com/Drauthius/awesome-sharedtags
-- local sharedtags = require("sharedtags")
-- sharedtaglist = sharedtags({
--     { name = "1", layout = awful.layout.layouts[1] },
--     { name = "2", layout = awful.layout.layouts[1] },
--     { name = "3", layout = awful.layout.layouts[1] },
--     { name = "4", layout = awful.layout.layouts[1] },
--     { name = "5", layout = awful.layout.layouts[1] },
--     { name = "6", layout = awful.layout.layouts[1] },
--     { name = "7", layout = awful.layout.layouts[1] },
--     { name = "8", layout = awful.layout.layouts[1] },
--     { name = "9", screen = 3, layout = awful.layout.layouts[1] },
--     { name = "10", screen = 2, layout = awful.layout.layouts[1] }
-- })

-- Vicious
-- https://github.com/vicious-widgets/vicious.git
local vicious  = require("vicious")

-- Startup applications
-- awful.spawn.with_shell("~/.config/awesome/screen_toggle.sh -x")
-- awful.spawn.with_shell("~/.config/awesome/trayer.sh")
awful.spawn.with_shell("/usr/bin/dropbox start")
awful.spawn.with_shell("/usr/bin/nm-applet &")

-- -----------------------------------------------------------------------------
-- }}}
-- -----------------------------------------------------------------------------
