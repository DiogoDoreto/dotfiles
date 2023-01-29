local wezterm = require("wezterm")

local function get_current_working_dir(tab)
  local current_dir = tab.active_pane.current_working_dir
  local HOME_DIR = string.format("file://%s", os.getenv("HOME"))

  return current_dir == HOME_DIR and "~"
      or string.format("%s", string.gsub(current_dir, "(.*[/\\])(.*)", "%2"))
end

wezterm.on(
  'format-tab-title',
  function(tab, tabs, panes, config, hover, max_width)
    local edge_background = "#0c0c0c"
    local background = '#252525'
    local foreground = '#b6b8bb'

    if tab.is_active then
      background = '#7b7c7e'
      foreground = '#161616'
    end
    local edge_foreground = background

    local process_name = string.gsub(tab.active_pane.foreground_process_name, "(.*[/\\])(.*)", "%2")
    local cwd = get_current_working_dir(tab)

    -- ensure that the titles fit in the available space,
    -- and that we have room for the edges.
    local title = wezterm.truncate_right(
      " " .. (tab.tab_index + 1) .. ": " .. process_name .. " " .. cwd,
      max_width - 2
    ) .. " "

    return {
      { Background = { Color = background } },
      { Foreground = { Color = foreground } },
      { Text = title },
      { Background = { Color = edge_background } },
      { Foreground = { Color = edge_foreground } },
      { Text = " " },
    }
  end
)
