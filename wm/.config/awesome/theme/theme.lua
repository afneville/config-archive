---------------------------
-- Default awesome theme --
---------------------------

local theme_assets = require("beautiful.theme_assets")
local xresources = require("beautiful.xresources")
local rnotification = require("ruled.notification")
local dpi = xresources.apply_dpi

local gfs = require("gears.filesystem")
local themes_path = gfs.get_themes_dir()

local theme = {}

theme.font          = "Jetbrains Mono Nerd Font 9"

theme.dir = string.format('%s/.config/awesome/theme', os.getenv('HOME'))

theme.bg_normal     = "#2e3440"
theme.bg_focus      = "#2e3440"
theme.bg_urgent     = "#2e3440"
theme.bg_minimize   = "#2e3440"
theme.bg_systray    = theme.bg_normal

theme.fg_normal     = "#aaaaaa"
theme.fg_focus      = "#ffffff"
theme.fg_urgent     = "#ffffff"
theme.fg_minimize   = "#ffffff"

theme.useless_gap   = 0
theme.border_width  = 2
theme.border_color_normal = "#3f4859"
theme.border_color_active  = "#3f4859"
theme.border_color_marked = "#3f4859"

-- Generate taglist squares:
local taglist_square_size = dpi(4)
theme.taglist_squares_sel = theme_assets.taglist_squares_sel(
    taglist_square_size, theme.fg_normal
)
theme.taglist_squares_unsel = theme_assets.taglist_squares_unsel(
    taglist_square_size, theme.fg_normal
)

theme.menu_submenu_icon = themes_path.."theme/submenu.png"
theme.menu_height = dpi(15)
theme.menu_width  = dpi(100)

theme.titlebar_close_button_normal = gfs.get_configuration_dir() .. "theme/titlebar/inactive.png"
theme.titlebar_close_button_focus  = gfs.get_configuration_dir() .. "theme/titlebar/close.png"
theme.titlebar_close_button_focus_hover = gfs.get_configuration_dir() .. "theme/titlebar/close_hover.png"

theme.titlebar_minimize_button_normal = gfs.get_configuration_dir() .. "theme/titlebar/inactive.png"
theme.titlebar_minimize_button_focus  = gfs.get_configuration_dir() .. "theme/titlebar/minimize.png"
theme.titlebar_minimize_button_focus_hover  = gfs.get_configuration_dir() .. "theme/titlebar/minimize_hover.png"

theme.titlebar_floating_button_normal_inactive = gfs.get_configuration_dir() .. "theme/titlebar/inactive.png"
theme.titlebar_floating_button_focus_inactive  = gfs.get_configuration_dir() .. "theme/titlebar/floating.png"
theme.titlebar_floating_button_normal_active = gfs.get_configuration_dir() .. "theme/titlebar/inactive.png"
theme.titlebar_floating_button_focus_active  = gfs.get_configuration_dir() .. "theme/titlebar/floating.png"
theme.titlebar_floating_button_focus_active_hover  = gfs.get_configuration_dir() .. "theme/titlebar/floating_hover.png"
theme.titlebar_floating_button_focus_inactive_hover  = gfs.get_configuration_dir() .. "theme/titlebar/floating_hover.png"

theme.icon_theme = nil

-- You can use your own layout icons like this:
theme.layout_fairh = gfs.get_configuration_dir().."theme/layouts/fairhw.png"
theme.layout_fairv = gfs.get_configuration_dir().."theme/layouts/fairvw.png"
theme.layout_floating  = gfs.get_configuration_dir().."theme/layouts/floatingw.png"
theme.layout_magnifier = gfs.get_configuration_dir().."theme/layouts/magnifierw.png"
theme.layout_max = gfs.get_configuration_dir().."theme/layouts/maxw.png"
theme.layout_fullscreen = gfs.get_configuration_dir().."theme/layouts/fullscreenw.png"
theme.layout_tilebottom = gfs.get_configuration_dir().."theme/layouts/tilebottomw.png"
theme.layout_tileleft   = gfs.get_configuration_dir().."theme/layouts/tileleftw.png"
theme.layout_tile = gfs.get_configuration_dir().."theme/layouts/tilew.png"
theme.layout_tiletop = gfs.get_configuration_dir().."theme/layouts/tiletopw.png"
theme.layout_spiral  = gfs.get_configuration_dir().."theme/layouts/spiralw.png"
theme.layout_dwindle = gfs.get_configuration_dir().."theme/layouts/dwindlew.png"
theme.layout_cornernw = gfs.get_configuration_dir().."theme/layouts/cornernww.png"
theme.layout_cornerne = gfs.get_configuration_dir().."theme/layouts/cornernew.png"
theme.layout_cornersw = gfs.get_configuration_dir().."theme/layouts/cornersww.png"
theme.layout_cornerse = gfs.get_configuration_dir().."theme/layouts/cornersew.png"

theme.awesome_icon = theme_assets.awesome_icon(
    theme.menu_height, theme.bg_focus, theme.fg_focus
)

theme.icon_theme = nil

rnotification.connect_signal('request::rules', function()
    rnotification.append_rule {
        rule       = { urgency = 'critical' },
        properties = { bg = '#ff0000', fg = '#ffffff' }
    }
end)

return theme
