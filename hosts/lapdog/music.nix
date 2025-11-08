{ pkgs, ... }:

{
  home.packages = with pkgs; [
    ardour
    drumgizmo
    hydrogen

    # wineWow
    wineWowPackages.yabridge
    yabridge
    yabridgectl
  ];

  home.sessionVariables = {
    # DrumGizmo is a lv2 plugin
    LV2_PATH = "/etc/profiles/per-user/$USER/lib/lv2/";
  };

  # needed for yabridgectl
  home.sessionVariablesExtra = ''
    export NIX_PROFILES="$NIX_PROFILES /etc/profiles/per-user/$USER"
  '';
}
