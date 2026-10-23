"""Subprocess-level Nix double. Its controls live outside the Git checkout."""

import json
import os
from pathlib import Path
import sys
import time
from urllib.parse import unquote, urlparse


control_path = Path(sys.argv[1])
args = sys.argv[2:]
control = json.loads(control_path.read_text())
phase = "check" if args[:2] == ["flake", "check"] else args[0]
with control_path.with_suffix(".calls").open("a") as stream:
    stream.write(json.dumps(args) + "\n")

if phase in ("check", "build"):
    source = args[2] if phase == "check" else args[1]
    checkout = Path(unquote(urlparse(source.removeprefix("git+")).path))
    assert (checkout / "shared.txt").read_text() == "full repository\n"
    assert (checkout / ".git").is_dir()
    assert "--no-update-lock-file" in args
    assert "--no-write-lock-file" in args
    if control.get("mutation") == phase:
        (checkout / "hosts/mini/flake.lock").write_text("mutated")
    if control.get("wait") == phase:
        control_path.with_suffix(".ready").touch()
        while not control_path.with_suffix(".release").exists():
            time.sleep(0.01)
    if control.get("fail") == phase:
        sys.exit(7)
    if phase == "build":
        output = Path(control["output"])
        output.mkdir(exist_ok=True)
        root = Path(args[args.index("--out-link") + 1])
        root.symlink_to(output)
        print(json.dumps([{"drvPath": str(output) + ".drv", "outputs": {"out": str(output)}}]))
elif phase == "path-info":
    if control.get("fail") == "closure":
        sys.exit(8)
    print(json.dumps({args[-1]: {"narSize": 42, "references": []}}))
else:
    raise AssertionError(args)
