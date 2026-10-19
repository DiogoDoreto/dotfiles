"""Lifetime guarantees through real child processes, with only QCoreApplication."""

from contextlib import redirect_stdout
import ctypes
import io
import json
import os
from pathlib import Path
import signal
import subprocess
import sys
import tempfile
import time
import unittest

from PySide6.QtCore import QCoreApplication, QTimer
from PySide6.QtTest import QTest

from listener import ListenerLifetime
from session import CommandSession
from scroll import ScrollController


app = QCoreApplication.instance() or QCoreApplication([])
CHILD = str(Path(__file__).with_name("listener_test_child.py"))


class ListenerTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        # Reap orphaned fixture descendants ourselves instead of trusting PID 1.
        cls.libc = ctypes.CDLL(None, use_errno=True)
        old = ctypes.c_int()
        if cls.libc.prctl(37, ctypes.byref(old), 0, 0, 0) or cls.libc.prctl(36, 1, 0, 0, 0):
            raise OSError(ctypes.get_errno(), "prctl child subreaper")
        cls.old_subreaper = old.value

    @classmethod
    def tearDownClass(cls):
        cls.libc.prctl(36, cls.old_subreaper, 0, 0, 0)

    def setUp(self):
        self.log = io.StringIO()
        self.redirect = redirect_stdout(self.log)
        self.redirect.__enter__()
        self.lifetimes = []
        self.reaped = set()

    def tearDown(self):
        try:
            for lifetime in self.lifetimes:
                lifetime.cleanup()
                lifetime.deleteLater()
            # A failing assertion must still kill/reap every fixture descendant.
            for event in self.events("helper"):
                pid = event["pid"]
                if pid in self.reaped:
                    continue
                try:
                    os.kill(pid, signal.SIGKILL)
                except ProcessLookupError:
                    pass
                try:
                    os.waitpid(pid, 0)
                except ChildProcessError:
                    pass
            QTest.qWait(1)
        finally:
            self.redirect.__exit__(None, None, None)

    def events(self, state):
        events = []
        for line in self.log.getvalue().splitlines():
            try:
                event = json.loads(line)
            except json.JSONDecodeError:
                continue
            if isinstance(event, dict) and event.get("state") == state:
                events.append(event)
        return events

    def wait(self, predicate, timeout=3000):
        deadline = time.monotonic() + timeout / 1000
        while not predicate() and time.monotonic() < deadline:
            QTest.qWait(10)
        self.assertTrue(predicate(), self.log.getvalue())

    def listener(self, scenario="transport", program=None, stop_timeout_ms=150):
        lifetime = ListenerLifetime(program or sys.executable, ["-u", CHILD, scenario],
                                    stop_timeout_ms=stop_timeout_ms)
        self.lifetimes.append(lifetime)
        return lifetime

    def test_fragmented_transport_readiness_controls_and_stop_gating(self):
        lifetime = self.listener()
        commands, ready, states = [], [], []
        lifetime.command.connect(lambda command, generation: commands.append((command, generation)))
        lifetime.ready.connect(lambda: (ready.append(True), lifetime.send_control("normal", 23)))
        lifetime.state_changed.connect(states.append)
        lifetime.start()
        lifetime.send_control("grid", 999)  # Not buffered across readiness.
        self.wait(lambda: len(commands) == 1 and bool(self.events("note")))
        self.assertEqual(ready, [True])
        self.assertEqual(commands, [("scroll", 23)])
        self.assertEqual(self.events("note"), [{"state": "note", "text": "café"}])
        self.assertEqual(self.events("control"), [{"state": "control", "mode": "normal", "generation": 23}])
        lifetime.stop()
        lifetime.send_control("normal", 24)
        lifetime.start()  # A start during stop must not resurrect the listener.
        self.wait(lambda: lifetime.state == "Paused")
        self.assertEqual(commands, [("scroll", 23)])
        self.assertEqual(ready, [True])
        self.assertTrue(self.events("command")[-1]["command"] == "after-stop")
        self.assertEqual(states, ["Starting", "Listening", "Stopping", "Paused"])

    def test_stop_during_qprocess_start_and_before_readiness(self):
        for immediate in (True, False):
            with self.subTest(immediate=immediate):
                lifetime = self.listener("startup")
                ready, commands = [], []
                lifetime.ready.connect(lambda: ready.append(True))
                lifetime.command.connect(lambda *args: commands.append(args))
                lifetime.start()
                if not immediate:
                    self.wait(lambda: bool(self.events("booting")))
                lifetime.stop()
                self.wait(lambda: lifetime.state == "Paused")
                self.assertEqual(ready, [])
                self.assertEqual(commands, [])

    def test_failed_start_can_retry_without_stale_transport(self):
        with tempfile.TemporaryDirectory() as directory:
            executable = Path(directory) / "python"
            lifetime = self.listener(program=str(executable))
            lifetime.start()
            self.wait(lambda: lifetime.state == "Error")
            self.assertTrue(lifetime.last_error)
            executable.symlink_to(sys.executable)
            lifetime.start()
            self.wait(lambda: lifetime.state == "Listening")
            self.assertEqual(lifetime.last_error, "")
            lifetime.stop()
            self.wait(lambda: lifetime.state == "Paused")

    def test_stop_from_ready_suppresses_rest_of_output_batch(self):
        lifetime = self.listener("ready-batch")
        commands = []
        lifetime.command.connect(lambda *args: commands.append(args))
        lifetime.ready.connect(lambda: (lifetime.send_control("normal", 0), lifetime.stop()))
        lifetime.start()
        self.wait(lambda: lifetime.state == "Paused")
        self.assertEqual(commands, [])

    def test_shutdown_observer_can_cancel_before_process_creation(self):
        lifetime = self.listener()
        states, done = [], []
        lifetime.shutdown_complete.connect(lambda: done.append(True))

        def changed(state):
            states.append(state)
            if state == "Starting":
                lifetime.shutdown()

        lifetime.state_changed.connect(changed)
        lifetime.start()
        self.assertEqual(states, ["Starting", "Stopping", "Paused"])
        self.assertEqual(done, [True])
        self.assertEqual(self.log.getvalue(), "")

    def test_shutdown_of_failed_start_and_idle_lifetime(self):
        with tempfile.TemporaryDirectory() as directory:
            lifetime = self.listener(program=str(Path(directory) / "missing"))
            done = []
            lifetime.shutdown_complete.connect(lambda: done.append(True))
            lifetime.start()
            lifetime.shutdown()
            self.wait(lambda: bool(done))
            self.assertEqual(lifetime.state, "Error")
            lifetime.cleanup()
            self.assertEqual(done, [True])
        idle = self.listener()
        done = []
        idle.shutdown_complete.connect(lambda: done.append(True))
        idle.shutdown()
        idle.start()
        idle.cleanup()
        self.assertEqual(idle.state, "Paused")
        self.assertEqual(done, [True])

    def test_normal_error_and_crash_exit_outcomes(self):
        for scenario, expected in (("normal-descendant", "Paused"), ("nonzero", "Error"),
                                   ("error", "Error"), ("crash", "Error")):
            with self.subTest(scenario=scenario):
                lifetime = self.listener(scenario)
                lifetime.start()
                self.wait(lambda: lifetime.state == "Listening")
                lifetime.send_control("normal", 0)
                self.wait(lambda: lifetime.state == expected)
                if scenario == "error":
                    self.assertEqual(lifetime.last_error, "controlled failure")
                elif expected == "Error":
                    self.assertIn("Listener exited with code", lifetime.last_error)
                if scenario == "normal-descendant":
                    self.assert_helper_dead(self.events("helper")[-1])

    def test_restart_delivers_session_generation_without_owning_it(self):
        lifetime = self.listener()
        session = CommandSession()
        scroll = ScrollController(session, "unused", session)
        self.addCleanup(scroll.cleanup)
        session.command("scroll", 0)
        session.cancel("test")
        self.assertEqual(session.generation, 2)
        lifetime.ready.connect(session.synchronize)
        session.changed.connect(lifetime.send_control)
        commands = []
        lifetime.command.connect(lambda command, generation: commands.append((command, generation)))
        for count in (1, 2):
            lifetime.start()
            self.wait(lambda: len(commands) == count)
            self.assertEqual(commands[-1], ("scroll", 2))
            self.assertEqual(session.generation, 2)
            lifetime.stop()
            self.wait(lambda: lifetime.state == "Paused")
        self.assertEqual([event["generation"] for event in self.events("control")], [2, 2])

    def test_stderr_is_forwarded_alongside_stdout_error_diagnostics(self):
        # QProcess forwards the OS descriptor, not Python's sys.stderr object.
        probe = """
import sys
from PySide6.QtCore import QCoreApplication
from listener import ListenerLifetime
app = QCoreApplication([])
lifetime = ListenerLifetime(sys.executable, ["-u", sys.argv[1], "stderr"])
lifetime.ready.connect(lambda: lifetime.send_control("normal", 0))
lifetime.state_changed.connect(lambda state: app.quit() if state == "Error" else None)
lifetime.start()
app.exec()
print(lifetime.last_error)
lifetime.cleanup()
"""
        result = subprocess.run([sys.executable, "-B", "-c", probe, CHILD],
                                capture_output=True, text=True, timeout=5)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("controlled listener stderr", result.stderr)
        self.assertIn('"state": "error"', result.stdout)
        self.assertTrue(result.stdout.rstrip().endswith("controlled failure"))

    def assert_helper_dead(self, event):
        pid = event["pid"]
        result = []

        def reaped():
            if result:
                return True
            reaped_pid, status = os.waitpid(pid, os.WNOHANG)
            if reaped_pid:
                result.append(status)
                self.reaped.add(pid)
            return bool(result)

        self.wait(reaped)
        self.assertEqual(os.waitstatus_to_exitcode(result[0]), -signal.SIGKILL)

    def test_escalation_is_responsive_and_cleans_surviving_descendant(self):
        lifetime = self.listener("stubborn", stop_timeout_ms=500)
        lifetime.start()
        self.wait(lambda: lifetime.state == "Listening")
        helper = self.events("helper")[-1]
        self.assertEqual(helper["leader"], helper["group"])
        self.assertEqual(helper["leader"], helper["session"])
        self.assertNotEqual(helper["group"], os.getpgrp())
        ticked = []
        lifetime.stop()
        QTimer.singleShot(20, lambda: ticked.append(True))
        self.wait(lambda: bool(ticked))
        self.assertEqual(lifetime.state, "Stopping", "grace must leave Qt responsive")
        self.wait(lambda: lifetime.state == "Paused")
        self.assert_helper_dead(helper)

    def test_crashed_leader_does_not_leave_descendant(self):
        lifetime = self.listener("crash-descendant")
        lifetime.start()
        self.wait(lambda: lifetime.state == "Listening")
        helper = self.events("helper")[-1]
        lifetime.send_control("normal", 0)
        self.wait(lambda: lifetime.state == "Error")
        self.assert_helper_dead(helper)

    def test_shutdown_during_start_is_terminal_and_completes_once(self):
        lifetime = self.listener("startup")
        done = []
        lifetime.shutdown_complete.connect(lambda: done.append(True))
        lifetime.start()
        lifetime.shutdown()
        lifetime.shutdown()
        self.wait(lambda: bool(done))
        lifetime.start()
        self.assertEqual(lifetime.state, "Paused")
        self.assertEqual(done, [True])

    def test_cleanup_escalates_without_event_loop_and_is_idempotent(self):
        lifetime = self.listener("stubborn")
        lifetime.start()
        self.wait(lambda: lifetime.state == "Listening")
        helper = self.events("helper")[-1]
        before = time.monotonic()
        lifetime.cleanup()
        self.assertLess(time.monotonic() - before, 2.5)
        self.assertEqual(lifetime.state, "Paused")
        self.assert_helper_dead(helper)
        lifetime.cleanup()


if __name__ == "__main__":
    unittest.main()
