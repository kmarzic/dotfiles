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

-- Notification library
local naughty = require("naughty")
local menubar = require("menubar")

-- Theme handling library
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")

local config_path = os.getenv("HOME") .. "/.config/awesome/"
local theme_dir = os.getenv("HOME") .. "/.config/awesome/themes/"

-- Error handling
dofile(config_path .. "error.lua")

-- Variables
terminal = "urxvt"
awful.util.terminal = terminal
browser = "firefox"
mail = "evolution"
messenger = "gajim"
editor = os.getenv("EDITOR") or "vim"
editor_cmd = terminal .. " -e " .. editor
-- sharedtaglist = {}

-- Themes define colours, icons, font and wallpapers.
-- local theme_name = "default"
local theme_name = "ansi"
-- local theme_name = "gtk"
-- local theme_name = "sky"
-- local theme_name = "xresources"
-- local theme_name = "zenburn"
-- local theme_name = "solarized.dark"
-- local theme_name = "solarized.light"
beautiful.init( theme_dir .. theme_name .. "/theme.lua" )

-- Local
local bindings = require("bindings")
local helpers  = require("helpers")
local layouts  = require("layouts")
local menu     = require("menu")
local notifs   = require("notifications")
local rules    = require("rules")
local signals  = require("signals")
local wibar    = require("wibar")

-- Vicious
local vicious  = require("vicious")

-- Startup applications
awful.spawn.with_shell(os.getenv("HOME") .. "/.config/awesome/screen_toggle.sh -x")
-- awful.spawn.with_shell(os.getenv("HOME") .. "/.config/awesome/trayer.sh")
awful.spawn.with_shell("/usr/bin/dropbox start")
awful.spawn.with_shell("/usr/bin/nm-applet &")

-- -----------------------------------------------------------------------------
-- END
-- -----------------------------------------------------------------------------
