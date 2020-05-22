-- -----------------------------------------------------------------------------
-- {{{ rules
-- -----------------------------------------------------------------------------

-- Standard awesome library
local awful = require("awful")
local ruled = require("ruled")

-- Theme handling library
local beautiful = require("beautiful")


-- {{{ Rules
-- Rules to apply to new clients.
ruled.client.connect_signal("request::rules", function()
    -- All clients will match this rule.
    ruled.client.append_rule {
        id         = "global",
        rule       = { },
        properties = {
            focus     = awful.client.focus.filter,
            raise     = true,
            screen    = awful.screen.preferred,
            placement = awful.placement.no_overlap+awful.placement.no_offscreen,
            border_width = beautiful.border_width,
            border_color = beautiful.border_normal,
            -- screen = awful.screen.preferred,
            size_hints_honor = false,
            honor_workarea = true,
            honor_padding = true,
        }
    }

    -- Floating clients
    ruled.client.append_rule {
        id       = "floating",
        rule_any = {
            instance = {
                "copyq",
                "pinentry",
            },
            class    = {
                "Arandr",
                "Blueman-manager",
                "Gpick",
                "Kruler",
                "Sxiv",
                "Tor Browser",
                "Wpa_gui",
                "veromix",
                "xtightvncviewer",
            },
            -- Note that the name property shown in xprop might be set slightly after creation of the client
            -- and the name shown there might not match defined rules here.
            name    = {
                "Event Tester",  -- xev.
            },
            role    = {
                "AlarmWindow",    -- Thunderbird's calendar.
                "ConfigManager",  -- Thunderbird's about:config.
                "pop-up",         -- e.g. Google Chrome's (detached) Developer Tools.
            }
        },
        properties = { floating = true }
    }

    -- Add titlebars to normal clients and dialogs
    ruled.client.append_rule {
        id         = "titlebars",
        rule_any   = {
            type = { "normal", "dialog" },
        },
        properties = { titlebars_enabled = true      }
    }

    -- exit.sh
    ruled.client.append_rule {
        rule_any   = {
            class = { "Exit", "Xmessage" },
        },
        properties = { floating = true, titlebars_enabled = true, sticky = true, width = 800, height = 80 }
    }

    -- scratchpad.sh
    ruled.client.append_rule {
        rule_any   = {
            name = { "scratchpad" },
        },
        properties = { floating = true, sticky = true, width = 1200, height = 800 }
    }

    -- teams notification
    ruled.client.append_rule {
        id = "teams_notification",
        rule_any = {
            name = { "Microsoft Teams Notification" },
        },
        properties = {
            titlebars_enabled = false,
            floating = true,
            focus = false,
            draw_backdrop = false,
            skip_decoration = true,
            skip_taskbar = true,
            ontop = true,
            sticky = true,
        },
    }

    -- Set Firefox to always map on the tag named "2" on screen 1.
    -- ruled.client.append_rule {
    --     rule       = { class = "Firefox"     },
    --     properties = { screen = 1, tag = "2" }
    -- }

    ruled.client.append_rule
    {
        rule_any =
        {
            class =
            {
                "Chromium",
                "Chromium-browser",
                "Chrome",
                "Opera",
                "Navigator",
                "Firefox",
                "Firefox-esr",
                "Mozilla Firefox",
            },
        },
        properties = { tag = "5" }
    }

    ruled.client.append_rule
    {
        rule_any =
        {
            class =
            {
                "Pidgin",
                "Microsoft Teams - Preview",
                "Microsoft Teams Notification",
                "Skype",
            },
        },
        properties = { tag = "7" }
    }

    ruled.client.append_rule
    {
        rule_any =
        {
            class =
            {
                "Oracle VM VirtualBox Manager",
                "VirtualBox Manager",
            },
        },
        properties = { tag = "8" }
    }

    ruled.client.append_rule
    {
        rule_any =
        {
            class =
            {
                "Evolution",
                "Mozilla Thunderbird",
            },
        },
        properties = { tag = "9" }
    }

end)
-- }}}

-- -----------------------------------------------------------------------------
-- }}}
-- -----------------------------------------------------------------------------
