"""Deterministic process fixture; never opens audio or sends desktop input."""

import json
import os
import signal
import subprocess
import sys
import time


def emit(state, **details):
    print(json.dumps({"state": state, **details}, ensure_ascii=False), flush=True)


def late_output(signum, frame):
    emit("listening")
    emit("command", command="after-stop", generation=0)
    raise SystemExit(0)


scenario = sys.argv[1]
signal.signal(signal.SIGTERM, late_output)
if scenario in ("stubborn", "descendant", "crash-descendant", "normal-descendant"):
    signal.signal(signal.SIGTERM, signal.SIG_IGN)
if "descendant" in scenario or scenario == "stubborn":
    helper = subprocess.Popen(
        [sys.executable, "-c", "import signal,time; signal.signal(signal.SIGTERM, signal.SIG_IGN); time.sleep(60)"],
        stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    emit("helper", pid=helper.pid, leader=os.getpid(), group=os.getpgrp(), session=os.getsid(0))
if scenario == "startup":
    emit("booting")
    while True:
        signal.pause()
if scenario == "transport":
    emit("command", command="before-ready", generation=0)
    print("not json\n[]", flush=True)
    # Force separately scheduled pipe writes, including inside a UTF-8 codepoint.
    os.write(1, b'{"state": "listen')
    time.sleep(0.05)
    os.write(1, b'ing"}\n{"state":"note","text":"caf\xc3')
    time.sleep(0.05)
    os.write(1, b'\xa9"}\n')
elif scenario == "ready-batch":
    os.write(1, b'{"state":"listening"}\n{"state":"command","command":"after-ready","generation":0}\n')
else:
    emit("listening")
if scenario in ("crash-descendant", "normal-descendant", "error", "stderr", "nonzero", "crash"):
    # Exit on explicit control so the test can first observe readiness.
    sys.stdin.readline()
    if scenario == "stderr":
        print("controlled listener stderr", file=sys.stderr, flush=True)
    if scenario in ("error", "stderr"):
        emit("error", error="controlled failure")
    if scenario in ("crash", "crash-descendant"):
        os.kill(os.getpid(), signal.SIGKILL)
    raise SystemExit(0 if scenario == "normal-descendant" else 7)
for line in sys.stdin:
    control = json.loads(line)
    emit("control", **control)
    emit("command", command="scroll", generation=control["generation"])
    emit("listening")  # A duplicate must not synchronize/reset an active group.
