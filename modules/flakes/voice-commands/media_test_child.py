"""Real playerctl/playerctld fixture, configured by each test's environment."""

import json
import os
import sys
import time

arguments = sys.argv[1:]
with open(os.environ["VOICE_MEDIA_LOG"], "a") as log:
    log.write(json.dumps({"args": arguments, "pid": os.getpid()}) + "\n")
if not arguments:  # foreground tracker
    if os.environ.get("VOICE_MEDIA_EXISTING_TRACKER"):
        raise SystemExit(1)
    time.sleep(60)
elif arguments == ["--list-all"]:
    time.sleep(float(os.environ.get("VOICE_MEDIA_LIST_DELAY", "0")))
    print(os.environ.get("VOICE_MEDIA_PLAYERS", "playerctld\nmpv.first\nmpv.second\nvlc"), flush=True)
    sys.exit(int(os.environ.get("VOICE_MEDIA_LIST_EXIT", "0")))
else:
    time.sleep(float(os.environ.get("VOICE_MEDIA_ACTION_DELAY", "0")))
    sys.exit(int(os.environ.get("VOICE_MEDIA_ACTION_EXIT", "0")))
