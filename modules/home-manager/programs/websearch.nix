{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.dog.programs.websearch;

  websearch = pkgs.writeShellApplication {
    name = "websearch";
    runtimeInputs = [
      pkgs.curl
      pkgs.jq
    ];
    text = ''
      if [[ $# -eq 0 ]]; then
        echo "Usage: websearch <query>" >&2
        echo "Example: websearch nixos home manager options" >&2
        exit 1
      fi

      query="$*"
      encoded=$(printf '%s' "$query" | jq -sRr @uri)

      response=$(curl \
        --silent \
        --fail \
        --max-time 15 \
        --header "Accept: application/json" \
        "${cfg.url}/search?q=$encoded&format=json") || {
        echo "Error: Failed to reach SearXNG at ${cfg.url}" >&2
        exit 2
      }

      count=$(printf '%s' "$response" | jq '.results | length')

      if [[ "$count" -eq 0 ]]; then
        echo "No results found for: $query"
        exit 0
      fi

      printf '%s' "$response" | jq -r '
        .results[:${toString cfg.maxResults}][]
        | "\(.title // "(no title)")\n  \(.url)\(if .content != null and .content != "" then "\n  \(.content)" else "" end)\n"
      '
    '';
  };
in
{
  options.dog.programs.websearch = {
    enable = mkEnableOption "websearch SearXNG CLI tool";

    url = mkOption {
      type = types.str;
      default = "https://search.local.doreto.com.br";
      description = "Base URL of the SearXNG instance to query.";
    };

    maxResults = mkOption {
      type = types.int;
      default = 10;
      description = "Maximum number of results to display.";
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ websearch ];
  };
}
