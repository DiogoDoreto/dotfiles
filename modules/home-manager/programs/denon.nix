{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.dog.programs.denon;

  denon = pkgs.writeShellApplication {
    name = "denon";
    runtimeInputs = [
      pkgs.curl
    ];
    text = ''
      url="https://ha.local.doreto.com.br/api/webhook/-U18Y2qbyHIwNe929exQq5XW6"

      usage() {
        echo "Usage: denon <action>" >&2
        echo "Actions: up, down, mute" >&2
      }

      if [[ $# -ne 1 ]]; then
        usage
        exit 1
      fi

      case "$1" in
        up|down|mute)
          action="$1"
          ;;
        *)
          echo "Error: unknown action '$1'" >&2
          usage
          exit 1
          ;;
      esac

      curl \
        --silent \
        --fail \
        --max-time 10 \
        --request POST \
        --header "Content-Type: application/json" \
        --data "{\"action\":\"$action\"}" \
        "$url" || {
        echo "Error: failed to send '$action' to Home Assistant at $url" >&2
        exit 2
      }
    '';
  };
in
{
  options.dog.programs.denon = {
    enable = mkEnableOption "denon Home Assistant AVR control CLI tool";
  };

  config = mkIf cfg.enable {
    home.packages = [ denon ];
  };
}
