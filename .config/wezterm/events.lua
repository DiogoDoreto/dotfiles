local wezterm = require("wezterm")
local colorschemes = wezterm.get_builtin_color_schemes()

local function get_current_working_dir(tab)
  local current_dir = tab.active_pane.current_working_dir
  local HOME_DIR = string.format("file://%s", os.getenv("HOME"))

  return current_dir == HOME_DIR and "~" or string.format("%s", string.gsub(current_dir, "(.*[/\\])(.*)", "%2"))
end

wezterm.on("format-tab-title", function(tab, tabs, panes, config, hover, max_width)
  local current_scheme = colorschemes[config.color_scheme]
  local edge_background = current_scheme.tab_bar.background

  local tab_scheme = current_scheme.tab_bar.inactive_tab
  if tab.is_active then tab_scheme = current_scheme.tab_bar.active_tab end
  local edge_foreground = tab_scheme.bg_color

  local process_name = string.gsub(tab.active_pane.foreground_process_name, "(.*[/\\])(.*)", "%2")
  local cwd = get_current_working_dir(tab)

  -- ensure that the titles fit in the available space,
  -- and that we have room for the edges.
  local title = wezterm.truncate_right(" " .. (tab.tab_index + 1) .. ": " .. process_name .. " " .. cwd, max_width - 2)
      .. " "

  return {
    { Background = { Color = tab_scheme.bg_color } },
    { Foreground = { Color = tab_scheme.fg_color } },
    { Text = title },
    { Background = { Color = edge_background } },
    { Foreground = { Color = edge_foreground } },
    { Text = " " },
  }
end)
