#!/usr/bin/env bash
# Requires: bash 4.0+, notify-send
set -euo pipefail

# Parse arguments until -- separator
notify_args=()
while [[ $# -gt 0 ]]; do
  case "$1" in
    --) shift; break ;;
    *) notify_args+=("$1"); shift ;;
  esac
done

# Validate command provided
if [[ $# -eq 0 || ${#notify_args[@]} -eq 0 ]]; then
  echo "Usage: $0 [notify-send args without body] -- command [args...]" >&2
  exit 2
fi

# Build shell-escaped command string
printf -v cmd_str '%q ' "$@"
cmd_str=${cmd_str% }

# Execute command and capture exit code
set +e  # Temporarily disable exit on error
"$@"
rc=$?
set -e

# prepare notification text
if [[ $rc -eq 0 ]]; then
  body="Completed successfully"
else
  body="Command failed (exit $rc)"
fi

# notify (fallback to stderr if notify-send not available)
if command -v notify-send >/dev/null 2>&1; then
  notify-send "${notify_args[@]}" "$body" 2>/dev/null || true
else
  echo "WARN: notify-send not found;" >&2
  echo "${notify_args[@]} $body" >&2
fi

exit $rc
