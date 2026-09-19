{
  curl,
  writeShellApplication,
}:

writeShellApplication {
  name = "denon";
  runtimeInputs = [
    curl
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
      --max-time 0.15 \
      --request POST \
      --header "Content-Type: application/json" \
      --data "{\"action\":\"$action\"}" \
      "$url" || {
      echo "Error: failed to send '$action' to Home Assistant at $url" >&2
      exit 2
    }
  '';
}
