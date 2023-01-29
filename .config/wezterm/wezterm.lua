local wezterm = require 'wezterm'
local act = wezterm.action

require("events")

return {
  font = wezterm.font('Victor Mono', { weight = 'Medium' }),
  color_scheme = "carbonfox",
  font_size = 14,
  line_height = 1.1,

  default_prog = { '/usr/local/bin/fish', '-l' },

  window_decorations = "RESIZE",
  window_padding = {
    left = '1cell',
    right = '1cell',
    top = '0.5cell',
    bottom = '0.5cell',
  },

  use_fancy_tab_bar = false,
  tab_bar_at_bottom = true,
  tab_max_width = 30,
  show_new_tab_button_in_tab_bar = false,

  keys = {
    { key = 'd', mods = 'CMD', action = act.SplitHorizontal },
    { key = 'd', mods = 'CMD|SHIFT', action = act.SplitVertical },
    -- forward modded enter keys
    { key = 'Enter', mods = 'CTRL', action = act.SendString '\x1b[13;5u' },
    { key = 'Enter', mods = 'SHIFT', action = act.SendString '\x1b[13;2u' },
    { key = 'Enter', mods = 'CTRL|SHIFT', action = act.SendString '\x1b[13;6u' },
  },
}
