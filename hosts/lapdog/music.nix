{ pkgs, ... }:

{
  home.packages = with pkgs; [
    ardour
    drumgizmo
    hydrogen
  ];

  home.sessionVariables = {
    # DrumGizmo is a lv2 plugin
    LV2_PATH = "/etc/profiles/per-user/$USER/lib/lv2/";
  };
}
