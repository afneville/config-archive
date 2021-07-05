local gears = require("gears")
local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")
local variables = require("core.variables")

local items = {}

items["awesome_submenu"] = {
   { "hotkeys", function() hotkeys_popup.show_help(nil, awful.screen.focused()) end },
   { "manual", variables.terminal .. " -e man awesome" },
   { "edit config", variables.editor .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", function() awesome.quit() end }
}

items["right_click_menu"] = awful.menu({ items = { { "awesome", items["awesome_submenu"], beautiful.awesome_icon },
                                    { "open terminal", variables.terminal }
                                  }
                        })

items["awesome_launcher"] = awful.widget.launcher({ image = beautiful.awesome_icon,
                                     menu = items["right_click_menu"] })


return items
