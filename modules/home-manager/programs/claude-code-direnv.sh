#!/bin/bash
# From https://github.com/anthropics/claude-code/issues/42229

[ -n "$CLAUDE_ENV_FILE" ] || exit 0

ENV_SNAPSHOT="${CLAUDE_ENV_FILE}.snapshot"

if ! grep -qF "$ENV_SNAPSHOT" "$CLAUDE_ENV_FILE" 2>/dev/null; then
    echo ". \"$ENV_SNAPSHOT\"" >> "$CLAUDE_ENV_FILE"
fi

(
    direnv export bash 2>/dev/null
    echo "true"
) > "$ENV_SNAPSHOT"
