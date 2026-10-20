"""CLI dispatch fixture: logs argv and exits; never records or types."""

import json
import os
from pathlib import Path
import sys
import time

assert sys.argv[1:] == ["--toggle-transcription"], sys.argv
config = json.loads(Path(os.environ["VOICE_HANDY_TEST_CONFIG"]).read_text())
mode = config["mode"]
with Path(config["log"]).open("a") as output:
    output.write(json.dumps(sys.argv[1:]) + "\n")
if mode == "slow":
    time.sleep(.15)
elif mode == "hang":
    time.sleep(60)
elif mode == "fail":
    print("controlled dispatch failure", file=sys.stderr)
    raise SystemExit(7)
