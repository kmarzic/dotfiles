----------------------------------
--  "Solarized Light" awesome theme   --
--     By Gwenhael Le Moine     --
--  Modified by Kresimir Marzic --
----------------------------------

-- {{{ Main
theme = {}

local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi

local gfs = require("gears.filesystem")
local themes_path = gfs.get_themes_dir()

theme.path = os.getenv( "HOME" ) .. "/.config/awesome/themes/solarized.light"
theme.default_themes_path = os.getenv("HOME") .. "/.config/awesome/themes"
theme.wallpaper_cmd = { "awsetbg "..theme.default_themes_path.."/sky/sky-background.png" }

theme.colors = {}
theme.colors.base03  = "#002b36"
theme.colors.base02  = "#073642"
theme.colors.base01  = "#586e75"
theme.colors.base00  = "#657b83"
theme.colors.base0   = "#839496"
theme.colors.base1   = "#93a1a1"
theme.colors.base2   = "#eee8d5"
theme.colors.base3   = "#fdf6e3"
theme.colors.yellow  = "#b58900"
theme.colors.orange  = "#cb4b16"
theme.colors.red     = "#dc322f"
theme.colors.magenta = "#d33682"
theme.colors.violet  = "#6c71c4"
theme.colors.blue    = "#268bd2"
theme.colors.cyan    = "#2aa198"
theme.colors.green   = "#859900"
-- }}}

-- {{{ Styles
-- theme.font       = "ubuntu 9"
theme.font       = "sans 10"

-- {{{ Colors
theme.bg_normal  = theme.colors.base3
theme.bg_focus   = theme.colors.base1
theme.bg_urgent  = theme.colors.red
theme.bg_systray = theme.bg_normal
--
theme.fg_normal  = theme.colors.base02
theme.fg_focus   = theme.colors.base03
theme.fg_urgent  = theme.colors.base3
theme.fg_systray  = "#ffffff"
theme.fg_minimize = "#888888"
-- }}}

-- {{{ Borders
theme.border_width  = dpi(1)
theme.border_normal = theme.bg_normal
theme.border_focus  = theme.bg_focus
theme.border_marked = theme.bg_urgent
-- }}}

-- {{{ Hotkeys help
theme.hotkeys_modifiers_fg = theme.colors.cyan
theme.hotkeys_modifiers_bg = theme.colors.red
theme.hotkeys_border_color = theme.colors.magenta
--- }}}

-- {{{ Titlebars
theme.titlebar_bg_focus  = theme.bg_focus
theme.titlebar_bg_normal = theme.bg_normal
-- }}}

-- {{{ Taglist
theme.taglist_bg = theme.bg_normal
theme.taglist_fg = theme.fg_normal
theme.taglist_spacing = dpi(1)
theme.taglist_shape_border_width = dpi(1)
theme.taglist_shape_border_color = theme.colors.base0
theme.taglist_shape_border_width_focus = dpi(1)
theme.taglist_shape_border_color_focus = theme.colors.blue
-- }}}

-- {{{ Tasklist
theme.tasklist_bg = theme.bg_normal
theme.tasklist_fg = theme.fg_normal
theme.tasklist_spacing = dpi(1)
theme.tasklist_shape_border_width = dpi(1)
theme.tasklist_shape_border_color = theme.colors.base0
theme.tasklist_shape_border_width_focus = dpi(1)
theme.tasklist_shape_border_color_focus = theme.colors.blue
-- }}}

-- {{{ Mouse finder
theme.mouse_finder_color = theme.colors.green
-- mouse_finder_[timeout|animate_timeout|radius|factor]
-- }}}

-- Gaps
theme.useless_gap   = "2"
theme.screen_margin = "2"

-- Rounded corners
theme.border_radius = dpi(6)

-- {{{ Menu
-- Variables set for theming the menu:
-- menu_[bg|fg]_[normal|focus]
-- menu_[border_color|border_width]
theme.menu_height = "15"
theme.menu_width  = "100"
-- }}}

-- {{{ Icons
-- {{{ Taglist
theme.taglist_squares_sel   = theme.path.."/taglist/squarefz.png"
theme.taglist_squares_unsel = theme.path.."/taglist/squarez.png"
--theme.taglist_squares_resize = "false"
-- }}}

-- {{{ Misc
theme.awesome_icon           = theme.default_themes_path.."/sky/awesome-icon.png"
theme.menu_submenu_icon      = theme.default_themes_path.."/default/submenu.png"
-- }}}

-- {{{ Layout
theme.layout_tile       = theme.path.."/layouts/tile.png"
theme.layout_tileleft   = theme.path.."/layouts/tileleft.png"
theme.layout_tilebottom = theme.path.."/layouts/tilebottom.png"
theme.layout_tiletop    = theme.path.."/layouts/tiletop.png"
theme.layout_fairv      = theme.path.."/layouts/fairv.png"
theme.layout_fairh      = theme.path.."/layouts/fairh.png"
theme.layout_spiral     = theme.path.."/layouts/spiral.png"
theme.layout_dwindle    = theme.path.."/layouts/dwindle.png"
theme.layout_max        = theme.path.."/layouts/max.png"
theme.layout_fullscreen = theme.path.."/layouts/fullscreen.png"
theme.layout_magnifier  = theme.path.."/layouts/magnifier.png"
theme.layout_floating   = theme.path.."/layouts/floating.png"
-- }}}

-- {{{ Titlebar
theme.titlebar_close_button_focus  = theme.default_themes_path.."/default/titlebar/close_focus.png"
theme.titlebar_close_button_normal = theme.default_themes_path.."/default/titlebar/close_normal.png"

theme.titlebar_ontop_button_focus_active  = theme.default_themes_path.."/default/titlebar/ontop_focus_active.png"
theme.titlebar_ontop_button_normal_active = theme.default_themes_path.."/default/titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_inactive  = theme.default_themes_path.."/default/titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_inactive = theme.default_themes_path.."/default/titlebar/ontop_normal_inactive.png"

theme.titlebar_sticky_button_focus_active  = theme.default_themes_path.."/default/titlebar/sticky_focus_active.png"
theme.titlebar_sticky_button_normal_active = theme.default_themes_path.."/default/titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_inactive  = theme.default_themes_path.."/default/titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_inactive = theme.default_themes_path.."/default/titlebar/sticky_normal_inactive.png"

theme.titlebar_floating_button_focus_active  = theme.default_themes_path.."/default/titlebar/floating_focus_active.png"
theme.titlebar_floating_button_normal_active = theme.default_themes_path.."/default/titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_inactive  = theme.default_themes_path.."/default/titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_inactive = theme.default_themes_path.."/default/titlebar/floating_normal_inactive.png"

theme.titlebar_maximized_button_focus_active  = theme.default_themes_path.."/default/titlebar/maximized_focus_active.png"
theme.titlebar_maximized_button_normal_active = theme.default_themes_path.."/default/titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_inactive  = theme.default_themes_path.."/default/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_inactive = theme.default_themes_path.."/default/titlebar/maximized_normal_inactive.png"
-- }}}
-- }}}

return theme
