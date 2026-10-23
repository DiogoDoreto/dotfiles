"""Exercise the maintenance shell through fake commands, never real systemd/GC."""

import json
import os
from pathlib import Path
import signal
import subprocess
import sys
import tempfile
import time
import unittest


HERE = Path(__file__).resolve().parent
TMP = HERE.parents[2] / ".private" / "tmp"
TMP.mkdir(parents=True, exist_ok=True)

STUB = r'''
import json
import os
from pathlib import Path
import subprocess
import sys

base = Path(os.environ["TEST_MAINTENANCE"])
state = json.loads((base / "state.json").read_text())
name, args = Path(sys.argv[0]).name, sys.argv[1:]
with (base / "events.jsonl").open("a") as file:
    file.write(json.dumps(dict(command=name, args=args,
                               token="NIX_CACHE_JOB_TOKEN" in os.environ,
                               credentials="CREDENTIALS_DIRECTORY" in os.environ)) + "\n")
if name == "systemctl":
    if args[0] == "show":
        prop = next(arg.split("=", 1)[1] for arg in args if arg.startswith("--property="))
        print(state.get(prop, {"LoadState": "loaded", "ActiveState": "active", "Result": "success"}[prop]))
    elif args[0] == "stop":
        state["ActiveState"] = "inactive"
        (base / "state.json").write_text(json.dumps(state))
        sys.exit(state.get("stop_exit", 0))
    elif args[-1] == "nixos-generation-cleanup.service":
        sys.exit(state.get("gc_exit", 0))
    else:
        sys.exit(state.get("restart_exit", 0))
elif name == "timeout":
    assert args[:3] == ["--foreground", "--kill-after=10s", "24h"]
    if state.get("timeout_exit"):
        state["ActiveState"] = "deactivating"
        (base / "state.json").write_text(json.dumps(state))
        sys.exit(state["timeout_exit"])
    sys.exit(subprocess.call(args[3:]))
elif name == "setpriv":
    assert args[:6] == ["--reuid=chungus-nix", "--regid=chungus-nix", "--clear-groups",
                        "--bounding-set=-all", "--inh-caps=-all", "--no-new-privs"]
    sys.exit(subprocess.call(args[6:]))
elif name == "nix-cache-roots":
    assert args == ["reconcile", "--apply"]
    assert os.environ["NIX_CACHE_JOB_TOKEN"] == "test-repository-read-token"
    assert "CREDENTIALS_DIRECTORY" not in os.environ
    if state.get("wait_for_signal"):
        import time
        (base / "reconcile-entered").touch()
        time.sleep(30)
    sys.exit(state.get("reconcile_exit", 0))
else:
    raise AssertionError(name)
'''


class Maintenance(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory(dir=TMP)
        self.addCleanup(self.tmp.cleanup)
        self.base = Path(self.tmp.name)
        self.bin = self.base / "bin"
        self.bin.mkdir()
        for name in ("systemctl", "timeout", "setpriv", "nix-cache-roots"):
            path = self.bin / name
            path.write_text(f"#!{sys.executable}\n{STUB}")
            path.chmod(0o700)
        self.credential = self.base / "repository-token"
        self.credential.write_text("test-repository-read-token\n")

    def run_maintenance(self, **state):
        (self.base / "state.json").write_text(json.dumps(state))
        result = subprocess.run(["bash", str(HERE.parent / "maintenance.sh")],
                                env=self.environment(),
                                text=True, capture_output=True, timeout=15)
        self.read_events()
        self.assertNotIn("test-repository-read-token", result.stdout + result.stderr)
        return result

    def environment(self):
        return {**os.environ, "PATH": f"{self.bin}:{os.environ['PATH']}",
                "CREDENTIALS_DIRECTORY": str(self.base),
                "TEST_MAINTENANCE": str(self.base),
                "NIX_CACHE_JOB_TOKEN": "must-not-leak"}

    def read_events(self):
        events = self.base / "events.jsonl"
        self.events = [json.loads(line) for line in events.read_text().splitlines()] if events.exists() else []
        for event in self.events:
            if event["command"] in ("systemctl", "timeout"):
                self.assertFalse(event["token"], event)

    def operations(self):
        return [event for event in self.events if event["command"] != "systemctl"
                or "show" not in event["args"]]

    def test_drain_reconcile_gc_then_restore_with_token_only_for_reconcile(self):
        result = self.run_maintenance()
        self.assertEqual(result.returncode, 0, result.stderr)
        events = self.operations()
        self.assertEqual([event["command"] for event in events],
                         ["timeout", "systemctl", "setpriv", "nix-cache-roots", "systemctl", "systemctl"])
        self.assertEqual(events[-2]["args"], ["start", "nixos-generation-cleanup.service"])
        self.assertEqual(events[-1]["args"], ["start", "forgejo-runner-chungus.service"])
        self.assertTrue(all(not event["token"] for event in self.events
                            if event["command"] in ("systemctl", "timeout")))
        self.assertFalse(next(event for event in events if event["command"] == "setpriv")["credentials"])

    def test_inactive_runner_is_not_started_after_maintenance(self):
        result = self.run_maintenance(ActiveState="inactive")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(self.operations()[-1]["args"], ["start", "nixos-generation-cleanup.service"])

    def test_reconciliation_failure_skips_gc_and_restores_runner(self):
        result = self.run_maintenance(reconcile_exit=1)
        self.assertNotEqual(result.returncode, 0)
        self.assertFalse(any("nixos-generation-cleanup.service" in event["args"] for event in self.events))
        self.assertEqual(self.operations()[-1]["args"], ["start", "forgejo-runner-chungus.service"])

    def test_gc_failure_still_restores_runner_and_reports_failure(self):
        result = self.run_maintenance(gc_exit=1)
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(self.operations()[-1]["args"], ["start", "forgejo-runner-chungus.service"])

    def test_stop_failure_skips_cleanup_and_queues_restoration(self):
        result = self.run_maintenance(stop_exit=1)
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(self.operations()[-1]["args"], ["--no-block", "start", "forgejo-runner-chungus.service"])
        self.assertFalse(any(event["command"] == "nix-cache-roots" for event in self.events))

    def test_drain_timeout_never_reconciles_or_runs_gc(self):
        result = self.run_maintenance(timeout_exit=124)
        self.assertEqual(result.returncode, 124)
        self.assertEqual(self.operations()[-1]["args"], ["--no-block", "start", "forgejo-runner-chungus.service"])
        self.assertFalse(any(event["command"] == "nix-cache-roots" for event in self.events))
        self.assertFalse(any("nixos-generation-cleanup.service" in event["args"] for event in self.events))

    def test_unclean_runner_result_aborts_even_if_stop_command_succeeded(self):
        result = self.run_maintenance(Result="signal")
        self.assertNotEqual(result.returncode, 0)
        self.assertFalse(any(event["command"] == "nix-cache-roots" for event in self.events))

    def test_transitional_runner_fails_before_stopping_or_cleaning(self):
        result = self.run_maintenance(ActiveState="deactivating")
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(self.operations(), [])

    def test_failed_runner_is_not_silently_started_or_cleaned(self):
        result = self.run_maintenance(ActiveState="failed")
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(self.operations(), [])

    def test_missing_runner_unit_fails_before_stopping_or_cleaning(self):
        result = self.run_maintenance(LoadState="not-found")
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(self.operations(), [])

    def test_missing_or_empty_credential_fails_before_stopping(self):
        self.credential.unlink()
        result = self.run_maintenance()
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(self.events, [])
        self.credential.touch()
        result = self.run_maintenance()
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(self.events, [])

    def test_restore_failure_is_reported(self):
        result = self.run_maintenance(restart_exit=1)
        self.assertNotEqual(result.returncode, 0)

    def test_termination_restores_runner_without_starting_gc(self):
        (self.base / "state.json").write_text(json.dumps(dict(wait_for_signal=True)))
        process = subprocess.Popen(["bash", str(HERE.parent / "maintenance.sh")],
                                   env=self.environment(), start_new_session=True,
                                   text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        try:
            deadline = time.monotonic() + 10
            while not (self.base / "reconcile-entered").exists():
                if process.poll() is not None or time.monotonic() > deadline:
                    self.fail("maintenance did not reach reconciliation")
                time.sleep(0.01)
            # Like systemd's default control-group termination: signal the shell
            # and its current child, but no real units or external processes.
            os.killpg(process.pid, signal.SIGTERM)
            stdout, stderr = process.communicate(timeout=10)
            self.assertEqual(process.returncode, 143, stdout + stderr)
            self.assertNotIn("test-repository-read-token", stdout + stderr)
            self.read_events()
            self.assertEqual(self.operations()[-1]["args"], ["start", "forgejo-runner-chungus.service"])
            self.assertFalse(any("nixos-generation-cleanup.service" in event["args"] for event in self.events))
        finally:
            if process.poll() is None:
                os.killpg(process.pid, signal.SIGKILL)
                process.communicate(timeout=10)


if __name__ == "__main__":
    unittest.main()
