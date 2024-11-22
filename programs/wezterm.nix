{ config, lib, pkgs, ... }:

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
      package = (config.lib.nixGL.wrap pkgs.wezterm);
      extraConfig = ''
        local act = wezterm.action
        return {
          font = wezterm.font {
            family = "VictorMono Nerd Font Propo",
            weight = "Medium",
            harfbuzz_features = { 'calt=0', 'clig=0', 'liga=0' },
          },
          color_scheme = "duskfox",
          font_size = 10,
          line_height = 1.1,
          default_prog = { "${lib.getExe pkgs.fish}", "-l" },
          window_background_opacity = 0.97,
          window_decorations = "RESIZE",
          window_padding = {
            left = "1cell",
            right = "1cell",
            top = "0.5cell",
            bottom = "0.5cell",
          },
          use_fancy_tab_bar = false,
          hide_tab_bar_if_only_one_tab = true,
          keys = {
            { key = 'PageUp', action = act.ScrollByPage(-0.5) },
            { key = 'PageDown', action = act.ScrollByPage(0.5) },
          },
        }
      '';
    };
  };
}
