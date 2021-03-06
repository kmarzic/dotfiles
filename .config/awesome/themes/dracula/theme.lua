-- -----------------------------------------------------------------------------
-- {{{ awesome dracula theme                      --
-- -----------------------------------------------------------------------------

local theme_assets = require("beautiful.theme_assets")

local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi

local gfs = require("gears.filesystem")
local themes_path = gfs.get_themes_dir()

local theme_name = "ansi"
local theme_dir = os.getenv("HOME") .. "/.config/awesome/themes/" .. theme_name .. "/"

local theme = {}

-- theme.font          = "sans 8"
-- theme.font          = "sans 9"
-- theme.font          = "sans 10"
theme.font          = "sans 10"
-- theme.font          = "monospace 8"
-- theme.font          = "monospace 9"
-- theme.font          = "monospace 10"
-- theme.font          = "monospace 11"
-- theme.font          = "xft:Monospace:pixelsize=6:antialias=true:style=normal"
-- theme.font          = "xft:Monospace:pixelsize=8:antialias=true:style=bold"
-- theme.font          = "Droid Sans Mono 10"

-- | Base | --
theme.bg_normal     = "#282a36"
theme.bg_focus      = "#282a37"
theme.bg_minimize   = theme.bg_normal

theme.fg_normal     = "#ffffff"
theme.fg_focus      = "#bd93f9"
theme.fg_minimize   = "#888888"

-- | Systray | --
theme.bg_systray    = "#222222"
theme.fg_systray    = "#ffffff"

-- | Urgent | --
theme.bg_urgent     = "#333333"
theme.fg_urgent     = "#ff0000"

-- | Borders | --
theme.border_width  = dpi(1)
theme.border_normal = "#444444"
theme.border_focus  = "#bd93f9"
theme.border_marked = "#91231c"

-- | Hotkeys help | --
theme.hotkeys_modifiers_fg = "#bd93f9"
theme.hotkeys_modifiers_bg = "#ff0000"
theme.hotkeys_border_color = "#bd93f9"

-- | Taglist | --
theme.taglist_spacing = dpi(1)
theme.taglist_shape_border_width = dpi(1)
theme.taglist_shape_border_color = "#777777"
theme.taglist_shape_border_width_focus = dpi(1)
theme.taglist_shape_border_color_focus = "#bd93f9"

-- | Tasklist | --
theme.tasklist_spacing = dpi(1)
theme.tasklist_shape_border_width = dpi(1)
theme.tasklist_shape_border_color = "#777777"
theme.tasklist_shape_border_width_focus = dpi(1)
theme.tasklist_shape_border_color_focus = "#bd93f9"

-- | Wibar | --
theme.wibar_border_width = dpi(1)
theme.wibar_border_color = "#666666"
theme.wibar_height = dpi(25)
theme.wibar_opacity = 0.7

-- | Wibox | --
theme.bg_wibox = "#666666"

-- | Gaps | --
theme.useless_gap   = dpi(2)
-- This could be used to manually determine how far away from the
-- screen edge the bars / notifications should be.
theme.screen_margin = dpi(2)

-- | Rounded corners | --
theme.border_radius = dpi(6)

-- There are other variable sets
-- overriding the default one when
-- defined, the sets are:
-- taglist_[bg|fg]_[focus|urgent|occupied|empty|volatile]
-- tasklist_[bg|fg]_[focus|urgent]
-- titlebar_[bg|fg]_[normal|focus]
-- tooltip_[font|opacity|fg_color|bg_color|border_width|border_color]
-- mouse_finder_[color|timeout|animate_timeout|radius|factor]
-- prompt_[fg|bg|fg_cursor|bg_cursor|font]
-- hotkeys_[bg|fg|border_width|border_color|shape|opacity|modifiers_fg|label_bg|label_fg|group_margin|font|description_font]
-- Example:
-- theme.taglist_bg_focus = "#ff0000"

-- | Generate taglist squares | --
local taglist_square_size = dpi(4)
theme.taglist_squares_sel = theme_assets.taglist_squares_sel(
    taglist_square_size, theme.fg_normal
)
theme.taglist_squares_unsel = theme_assets.taglist_squares_unsel(
    taglist_square_size, theme.fg_normal
)

-- | Variables set for theming notifications | --
-- notification_font
-- notification_[bg|fg]
-- notification_[width|height|margin]
-- notification_[border_color|border_width|shape|opacity]

-- | Variables set for theming the menu | --
-- menu_[bg|fg]_[normal|focus]
-- menu_[border_color|border_width]
theme.menu_submenu_icon = themes_path.."default/submenu.png"
theme.menu_height = dpi(15)
theme.menu_width  = dpi(100)

-- You can add as many variables as
-- you wish and access them by using
-- beautiful.variable in your rc.lua
-- theme.bg_widget = "#cc0000"

-- | Define the image to load | --
theme.titlebar_close_button_normal = themes_path.."default/titlebar/close_normal.png"
theme.titlebar_close_button_focus  = themes_path.."default/titlebar/close_focus.png"

theme.titlebar_minimize_button_normal = themes_path.."default/titlebar/minimize_normal.png"
theme.titlebar_minimize_button_focus  = themes_path.."default/titlebar/minimize_focus.png"

theme.titlebar_ontop_button_normal_inactive = themes_path.."default/titlebar/ontop_normal_inactive.png"
theme.titlebar_ontop_button_focus_inactive  = themes_path.."default/titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_active = themes_path.."default/titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_active  = themes_path.."default/titlebar/ontop_focus_active.png"

theme.titlebar_sticky_button_normal_inactive = themes_path.."default/titlebar/sticky_normal_inactive.png"
theme.titlebar_sticky_button_focus_inactive  = themes_path.."default/titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_active = themes_path.."default/titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_active  = themes_path.."default/titlebar/sticky_focus_active.png"

theme.titlebar_floating_button_normal_inactive = themes_path.."default/titlebar/floating_normal_inactive.png"
theme.titlebar_floating_button_focus_inactive  = themes_path.."default/titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_active = themes_path.."default/titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_active  = themes_path.."default/titlebar/floating_focus_active.png"

theme.titlebar_maximized_button_normal_inactive = themes_path.."default/titlebar/maximized_normal_inactive.png"
theme.titlebar_maximized_button_focus_inactive  = themes_path.."default/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_active = themes_path.."default/titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_active  = themes_path.."default/titlebar/maximized_focus_active.png"

-- theme.wallpaper = themes_path.."default/background.png"

-- You can use your own layout icons like this:
theme.layout_fairh = themes_path.."default/layouts/fairhw.png"
theme.layout_fairv = themes_path.."default/layouts/fairvw.png"
theme.layout_floating  = themes_path.."default/layouts/floatingw.png"
theme.layout_magnifier = themes_path.."default/layouts/magnifierw.png"
theme.layout_max = themes_path.."default/layouts/maxw.png"
theme.layout_fullscreen = themes_path.."default/layouts/fullscreenw.png"
theme.layout_tilebottom = themes_path.."default/layouts/tilebottomw.png"
theme.layout_tileleft   = themes_path.."default/layouts/tileleftw.png"
theme.layout_tile = themes_path.."default/layouts/tilew.png"
theme.layout_tiletop = themes_path.."default/layouts/tiletopw.png"
theme.layout_spiral  = themes_path.."default/layouts/spiralw.png"
theme.layout_dwindle = themes_path.."default/layouts/dwindlew.png"
theme.layout_cornernw = themes_path.."default/layouts/cornernww.png"
theme.layout_cornerne = themes_path.."default/layouts/cornernew.png"
theme.layout_cornersw = themes_path.."default/layouts/cornersww.png"
theme.layout_cornerse = themes_path.."default/layouts/cornersew.png"

-- Generate Awesome icon:
theme.awesome_icon = theme_assets.awesome_icon(
    theme.menu_height, theme.bg_focus, theme.fg_focus
)

-- Define the icon theme for application icons. If not set then the icons
-- from /usr/share/icons and /usr/share/icons/hicolor will be used.
theme.icon_theme = nil
theme.icon_batt = theme_dir.."icons/battery_charging.png"

-- Notifications
theme.notification_position = "top_right"
-- theme.notification_position = "top_left"
-- theme.notification_position = "top_middle"
theme.notification_border_width = dpi(1)
theme.notification_border_radius = theme.border_radius
theme.notification_border_color = theme.border_focus
theme.notification_bg = theme.bg_normal
theme.notification_fg = theme.fg_focus
theme.notification_crit_bg = theme.bg_normal
theme.notification_crit_fg = theme.fg_urgent
theme.notification_icon_size = dpi(60)
-- theme.notification_height = dpi(80)
-- theme.notification_width = dpi(320)
theme.notification_margin = dpi(15)
theme.notification_opacity = 1
theme.notification_font = theme.font
theme.notification_padding = theme.screen_margin * 2
theme.notification_spacing = theme.screen_margin * 2

return theme

-- -----------------------------------------------------------------------------
-- }}}
-- -----------------------------------------------------------------------------
-- vim: filetype=lua:expandtab:shiftwidth=4:tabstop=8:softtabstop=4:textwidth=80
