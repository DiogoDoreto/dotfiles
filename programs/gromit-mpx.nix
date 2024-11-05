{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.dog.programs.gromit-mpx;
in {
  options.dog.programs.gromit-mpx = {
    enable = mkEnableOption "gromit-mpx - on screen annotations";
  };

  config = mkIf cfg.enable {
    services.gromit-mpx = {
      enable = true;
      hotKey = "Insert"; # +shift=clear +ctrl=show/hide +alt=quit
      undoKey = null;

      tools = let
        pen = { device = "default"; type = "pen"; size = 5; };
        red-pen = pen // { color = "crimson"; };
        blue-pen = pen // { color = "deep sky blue"; modifiers = [ "CONTROL" ]; };
        green-pen = pen // { color = "yellow green"; modifiers = [ "2" ]; };
        all-pens = [ red-pen blue-pen green-pen ];
        make-arrow = p: p // { arrowSize = 3; modifiers = [ "SHIFT" ] ++ (p.modifiers or []); };
        all-arrows = map make-arrow all-pens;
      in all-pens ++ all-arrows ++ [
        {
          device = "default";
          type = "eraser";
          size = 75;
          modifiers = [ "3" ];
        }
      ];
    };
  };
}
