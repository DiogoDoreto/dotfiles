{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.dog.programs.wezterm;
in
{
  options.dog.programs.wezterm = {
    enable = mkEnableOption "Wezterm";
  };

  config = mkIf cfg.enable {
    programs.wezterm = {
      enable = true;
      # package = (config.lib.nixGL.wrap pkgs.wezterm);
      extraConfig = ''
        local act = wezterm.action
        local config = wezterm.config_builder()

        config.font = wezterm.font {
          family = "VictorMono Nerd Font Propo",
          weight = "Medium",
          harfbuzz_features = { 'calt=0', 'clig=0', 'liga=0' },
        }
        config.color_scheme = "Brewer (base16)"
        config.font_size = 10
        config.line_height = 1.1
        config.default_prog = { "${lib.getExe pkgs.fish}", "-l" }
        config.window_background_opacity = 0.95
        config.window_decorations = "NONE"
        config.window_padding = {
          left = "1cell",
          right = "1cell",
          top = "0.5cell",
          bottom = "0.5cell",
        }
        config.use_fancy_tab_bar = false
        config.hide_tab_bar_if_only_one_tab = true
        config.warn_about_missing_glyphs = false
        config.keys = {
          { key = 'PageUp', action = act.ScrollByPage(-0.5) },
          { key = 'PageDown', action = act.ScrollByPage(0.5) },
        }

        return config
      '';
    };
  };
}
