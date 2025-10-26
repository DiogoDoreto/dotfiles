{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.dog.programs.niri;
in
{
  options.dog.programs.niri = {
    enable = mkEnableOption "Niri WM";
  };

  config = mkIf cfg.enable {
    programs = {
      dankMaterialShell = {
        enable = true;
        niri.enableKeybinds = true;
        niri.enableSpawn = true;
      };

      niri.settings = {
        hotkey-overlay.skip-at-startup = true;
        xwayland-satellite.path = "${lib.getExe pkgs.xwayland-satellite-unstable}";

        input.touchpad = {
          tap = true;
          dwt = true;
          natural-scroll = true;
          # right click with 2 fingers
          click-method = "clickfinger";
        };

        layout = {
          always-center-single-column = true;
        };

        clipboard.disable-primary = true;

        screenshot-path = "~/Pictures/Screenshots/%Y-%m-%dT%H:%M:%S.png";

        binds =
          with config.lib.niri.actions;
          let
            # sh = spawn "sh" "-c";
            binds =
              {
                suffixes,
                prefixes,
                substitutions ? { },
              }:
              let
                replacer = replaceStrings (attrNames substitutions) (attrValues substitutions);
                format =
                  prefix: suffix:
                  let
                    actual-suffix =
                      if isList suffix.action then
                        {
                          action = head suffix.action;
                          args = tail suffix.action;
                        }
                      else
                        {
                          inherit (suffix) action;
                          args = [ ];
                        };

                    action = replacer "${prefix.action}-${actual-suffix.action}";
                  in
                  {
                    name = "${prefix.key}+${suffix.key}";
                    value.action.${action} = actual-suffix.args;
                  };
                pairs =
                  attrs: fn:
                  concatMap (
                    key:
                    fn {
                      inherit key;
                      action = attrs.${key};
                    }
                  ) (attrNames attrs);
              in
              listToAttrs (pairs prefixes (prefix: pairs suffixes (suffix: [ (format prefix suffix) ])));
          in
          lib.attrsets.mergeAttrsList [
            (binds {
              suffixes."H" = "column-left";
              suffixes."J" = "window-down";
              suffixes."K" = "window-up";
              suffixes."L" = "column-right";
              suffixes."Left" = "column-left";
              suffixes."Down" = "window-down";
              suffixes."Up" = "window-up";
              suffixes."Right" = "column-right";
              prefixes."Mod" = "focus";
              prefixes."Mod+Shift" = "move";
              prefixes."Mod+Ctrl" = "focus-monitor";
              prefixes."Mod+Shift+Ctrl" = "move-window-to-monitor";
              substitutions."monitor-column" = "monitor";
              substitutions."monitor-window" = "monitor";
            })
            (binds {
              suffixes."Home" = "first";
              suffixes."End" = "last";
              prefixes."Mod" = "focus-column";
              prefixes."Mod+Shift" = "move-column-to";
            })
            (binds {
              suffixes."U" = "workspace-down";
              suffixes."I" = "workspace-up";
              prefixes."Mod" = "focus";
              prefixes."Mod+Ctrl" = "move-window-to";
              prefixes."Mod+Shift" = "move";
            })
            (binds {
              suffixes = builtins.listToAttrs (
                map (n: {
                  name = toString n;
                  value = [
                    "workspace"
                    n
                  ];
                }) (range 1 9)
              );
              prefixes."Mod" = "focus";
              prefixes."Mod+Ctrl" = "move-window-to";
            })
            {
              "Mod+Slash".action = show-hotkey-overlay;
              "Mod+O".action = toggle-overview;

              "Mod+E".action = spawn "emacs";
              "Mod+T".action = spawn "wezterm";

              # "Mod+Shift+S".action = screenshot;
              # "Print".action.screenshot-screen = [ ];
              # "Mod+Print".action = screenshot-window;

              # ?
              "Mod+Insert".action = set-dynamic-cast-window;
              "Mod+Shift+Insert".action = set-dynamic-cast-monitor;
              "Mod+Delete".action = clear-dynamic-cast-target;

              "Mod+Q".action = close-window;

              "Mod+Tab".action = focus-window-down-or-column-right;
              "Mod+Shift+Tab".action = focus-window-up-or-column-left;
              "Mod+Comma".action = mkForce consume-window-into-column;
              "Mod+Period".action = expel-window-from-column;

              "Mod+R".action = switch-preset-column-width;
              "Mod+F".action = maximize-column;
              "Mod+Shift+F".action = fullscreen-window;
              "Mod+C".action = center-column;

              "Mod+Minus".action = set-column-width "-10%";
              "Mod+Plus".action = set-column-width "+10%";
              "Mod+Shift+Minus".action = set-window-height "-10%";
              "Mod+Shift+Plus".action = set-window-height "+10%";

              "Mod+Z".action = switch-focus-between-floating-and-tiling;
              "Mod+Shift+Z".action = toggle-window-floating;

              "Mod+Shift+Escape".action = toggle-keyboard-shortcuts-inhibit;
              "Mod+Shift+E".action = quit;
              "Mod+Shift+P".action = power-off-monitors;

              "Mod+Shift+Ctrl+T".action = toggle-debug-tint;
            }
          ];

        window-rules = [
          {
            matches = [
              {
                app-id = "keepassxc"; # REVIEW id
              }
            ];
            block-out-from = "screencast";
          }
          {
            matches = [
              {
                app-id = "wezterm";
              }
            ];
            default-column-width = null;
          }
        ];
      };
    };
  };
}
