#!/usr/bin/env python3
"""Opt-in, isolated native Pi config merge + MCP connection smoke test.

Usage (from repo root): python3 modules/flakes/host-command-review/tests/pi-native-smoke.py \
  /nix/store/...-pi/bin/pi /nix/store/...-guest/bin/host-review-mcp
No real user config, desktop, network SSH, root command or activation is used.
"""
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile

pi, guest = (str(Path(p).resolve(strict=True)) for p in sys.argv[1:])
# Store paths reject /tmp ancestors. Keep all test state in the repo's private tmp.
root = Path(".private/tmp").resolve()
root.mkdir(parents=True, exist_ok=True)
with tempfile.TemporaryDirectory(dir=root, prefix="pi-review-smoke-") as home:
    env = {**os.environ, "HOME": home, "PI_CODING_AGENT_DIR": home + "/.pi/agent",
           "PI_OFFLINE": "1", "PI_SKIP_VERSION_CHECK": "1"}
    directory = Path(env["PI_CODING_AGENT_DIR"])
    directory.mkdir(parents=True)
    other = {"command": "/do/not/start", "enabled": False, "description": "preserve"}
    before = {"autoEnableCodemode": False, "mcpServers": {
        "unrelated": other, "host-review": {"command": "/old/owned/path"}}}
    config = directory / "mcp.json"
    config.write_text(json.dumps(before))
    command = [pi, "mcp", "add", "host-review", "--exposure", "deferred",
               "--description", "Propose/check/cancel lapdog host commands. Requires manual host reviewer and explicit session_id; output stays held until reviewed release.",
               "--", guest]
    for _ in range(2):
        subprocess.run(command, cwd=home, env=env, check=True, capture_output=True, timeout=20)
    after = json.loads(config.read_text())
    assert after["autoEnableCodemode"] is False
    assert after["mcpServers"]["unrelated"] == other
    assert after["mcpServers"]["host-review"]["command"] == guest
    result = subprocess.run([pi, "mcp", "list", "--json"], cwd=home, env=env,
                            check=True, capture_output=True, text=True, timeout=30)
    status = json.loads(result.stdout)
    assert status["errors"] == [], status
    server = next(s for s in status["servers"] if s["name"] == "host-review")
    assert server["state"] == "connected", server
    assert sorted(server["tools"]) == ["cancel", "get", "submit"], server
    assert server["exposure"] == "deferred", server
print("PASS: repeated owned-entry merge preserves unrelated config; native Pi connects all three tools")
