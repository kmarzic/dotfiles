-- -----------------------------------------------------------------------------
-- {{{ rules
-- -----------------------------------------------------------------------------

-- Standard awesome library
local awful = require("awful")

-- Theme handling library
local beautiful = require("beautiful")

-- Local
local bindings = require("bindings")


-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = {
          border_width = beautiful.border_width,
          border_color = beautiful.border_normal,
          focus = awful.client.focus.filter,
          raise = true,
          keys = clientkeys,
          buttons = bindings.mouse.clientbuttons,
          screen = awful.screen.preferred,
          size_hints_honor = false,
          honor_workarea = true,
          honor_padding = true,
          placement = awful.placement.no_overlap+awful.placement.no_offscreen
      }
    },

    -- Floating clients.
    { rule_any = {
        instance = {
          "DTA",         -- Firefox addon DownThemAll.
          "copyq",       -- Includes session name in class.
          "pinentry",
          "Exit",        -- exit.sh
          "scratchpad",  -- scratchpad.sh
        },
        class = {
          "Arandr",
          "Blueman-manager",
          "Gpick",
          "Kruler",
          "MessageWin",      -- kalarm.
          "Sxiv",
          "Tor Browser",     -- Needs a fixed window size to avoid fingerprinting by screen size.
          "Wpa_gui",
          "veromix",
          "xtightvncviewer",
          "scratchpad",      -- scratchpad.sh
        },

        -- Note that the name property shown in xprop might be set slightly after creation of the client
        -- and the name shown there might not match defined rules here.
        name = {
          "Event Tester",  -- xev.
        },
        role = {
          "AlarmWindow",   -- Thunderbird's calendar.
          "ConfigManager", -- Thunderbird's about:config.
          "pop-up",        -- e.g. Google Chrome's (detached) Developer Tools.
        }
      }, properties = { floating = true }},

    -- Add titlebars to normal clients and dialogs
    { rule_any = {
          type = { "normal", "dialog" }
      },
      -- properties = { titlebars_enabled = true }
      properties = { titlebars_enabled = false }
    },

    -- Set Firefox to always map on the tag named "2" on screen 1.
    -- { rule = { class = "Firefox" },
    --   properties = { screen = 1, tag = "2" } },

    { rule = { class = "Chromium" }, properties = { tag = "5" } },
    { rule = { class = "Chromium-browser" }, properties = { tag = "5" } },
    { rule = { class = "Chrome" }, properties = { tag = "5" } },
    { rule = { class = "Opera" }, properties = { tag = "5" } },
    { rule = { class = "Firefox" }, properties = { tag = "5" } },
    { rule = { class = "Firefox-esr" }, properties = { tag = "5" } },
    { rule = { class = "Mozilla Firefox" }, properties = { tag = "5" } },
    { rule = { class = "Vivaldi" }, properties = { tag = "5" } },
    { rule = { class = "Vivaldi-stable" }, properties = { tag = "5" } },
    { rule = { class = "Pidgin" }, properties = { tag = "7" } },
    { rule = { class = "Skype" }, properties = { tag = "7" } },
    { rule = { class = "Oracle VM VirtualBox Manager" }, properties = { tag = "8" } },
    { rule = { class = "VirtualBox Manager" }, properties = { tag = "8" } },
    { rule = { class = "Evolution" }, properties = { tag = "9" } },
    { rule = { class = "Mozilla Thunderbird" }, properties = { tag = "9" } },
}
-- -----------------------------------------------------------------------------
-- }}}
-- -----------------------------------------------------------------------------
