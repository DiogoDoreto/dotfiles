"""Check Pi's automatic extension discovery without making a model call."""

import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile

package = Path(sys.argv[1]).resolve(strict=True)
pi = Path(sys.argv[2]).resolve(strict=True)
tmp = Path(".private/tmp").resolve()
tmp.mkdir(parents=True, exist_ok=True)

with tempfile.TemporaryDirectory(prefix="pi-web-access-", dir=tmp) as directory:
    root = Path(directory)
    agent = root / "agent"
    extensions = agent / "extensions"
    extensions.mkdir(parents=True)
    (extensions / "pi-web-access").symlink_to(package, target_is_directory=True)
    result = subprocess.run(
        [str(pi), "--mode", "rpc", "--no-session"],
        input=json.dumps({"id": "smoke", "type": "get_commands"}) + "\n",
        text=True,
        capture_output=True,
        cwd=root,
        env={**os.environ, "PI_CODING_AGENT_DIR": str(agent)},
        timeout=60,
        check=True,
    )
    responses = [json.loads(line) for line in result.stdout.splitlines() if line]
    response = next(record for record in responses if record.get("id") == "smoke")
    assert response["success"], response
    commands = {
        command["name"]
        for command in response["data"]["commands"]
        if "pi-web-access" in command.get("sourceInfo", {}).get("path", "")
    }
    assert {"websearch", "curator", "search"} <= commands, (commands, result.stderr)
    assert not result.stderr.strip(), result.stderr
    print("Pi loaded pi-web-access and registered: " + ", ".join(sorted(commands)))
