"""Generation-safe controller media dispatch through actual asynchronous QProcesses."""

import itertools
import json
import os
from pathlib import Path
import sys
import tempfile
import time
import unittest
from unittest.mock import patch

from PySide6.QtCore import QTimer
from PySide6.QtTest import QTest
from PySide6.QtWidgets import QApplication

from desktop import DesktopCommands
from features import Features
from tray import TrayController


class MediaTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.root = Path(self.temp.name)
        self.child = self.root / "playerctl"
        self.child.write_text(f"#!{sys.executable}\n" + Path(__file__).with_name("media_test_child.py").read_text())
        self.child.chmod(0o755)
        self.environment = patch.dict(os.environ, {"VOICE_MEDIA_LOG": str(self.root / "log")})
        self.environment.start()
        self.desktop = DesktopCommands(QApplication.instance(), "unused", execute=True,
                                       features=Features(mouse_grid=True))
        self.media = self.desktop.media
        self.media.playerctl = self.media.playerctld = str(self.child)
        self.media.timeout_ms = 500
        self.events = []
        self.media.log_event = lambda state, **details: self.events.append((state, details))
        self.session = self.desktop.session
        self.media.startup.setInterval(1)

    def tearDown(self):
        self.desktop.cleanup()
        self.desktop.deleteLater()
        QTest.qWait(1)
        self.environment.stop()
        self.temp.cleanup()

    def wait(self, predicate, timeout=3000):
        deadline = time.monotonic() + timeout / 1000
        while not predicate() and time.monotonic() < deadline:
            QTest.qWait(5)
        self.assertTrue(predicate(), (self.calls(), self.events))

    def calls(self):
        path = self.root / "log"
        return [json.loads(line)["args"] for line in path.read_text().splitlines()] if path.exists() else []

    def send(self, command, generation=None):
        self.session.command(command, self.session.generation if generation is None else generation)

    def test_sequential_concrete_selection_and_action_failure_has_no_fallback(self):
        os.environ["VOICE_MEDIA_ACTION_EXIT"] = "1"
        self.session.synchronize()
        for command in ("play", "pause", "next", "previous"):
            self.send(command)
        self.wait(lambda: len(self.events) == 4)
        self.assertEqual([state for state, _ in self.events], ["action-failed"] * 4)
        self.assertEqual([call for call in self.calls() if call],
                         [call for command in ("play", "pause", "next", "previous")
                          for call in (["--list-all"], ["--player", "mpv.first", command])])

    def test_filter_no_player_and_failed_listing_never_invoke_action(self):
        self.session.synchronize()
        self.media.player = "vlc"
        self.send("play")
        self.wait(lambda: len(self.events) == 1)
        self.assertEqual(self.calls()[-1], ["--player", "vlc", "play"])
        self.media.player = "mpv"
        self.send("next")
        self.wait(lambda: len(self.events) == 2)
        self.assertEqual(self.calls()[-1], ["--player", "mpv.first", "next"])
        self.media.player = "missing"
        self.send("pause")
        self.wait(lambda: len(self.events) == 3)
        self.assertEqual(self.events[-1][0], "no-player")
        os.environ["VOICE_MEDIA_LIST_EXIT"] = "1"
        self.send("previous")
        self.wait(lambda: len(self.events) == 4)
        self.assertEqual(self.events[-1][0], "action-failed")
        self.assertEqual(self.calls()[-2:], [["--list-all"], ["--list-all"]])

    def test_mode_and_generation_gating_media_preserves_active_groups(self):
        self.media.execute = False
        self.session.synchronize()
        with patch.object(QApplication.instance(), "platformName", return_value="wayland"):
            for entry, mode in ((None, "normal"), ("scroll", "scroll"), ("click", "grid")):
                if entry:
                    self.send(entry)
                self.assertEqual(self.session.mode, mode)
                self.send("play", self.session.generation - 1)
                self.send("play", True)
                before = self.session.mode, self.session.generation
                self.send("play")
                self.assertEqual((self.session.mode, self.session.generation), before)
        self.assertEqual(len(self.events), 3)
        self.session.features = Features(media=False)
        self.send("pause")
        self.assertEqual(len(self.events), 3)
        self.assertEqual(self.calls(), [])

    def test_cancellation_during_listing_and_action_cannot_consume_new_request(self):
        for stage in ("LIST", "ACTION"):
            with self.subTest(stage=stage):
                self.events.clear()
                os.environ[f"VOICE_MEDIA_{stage}_DELAY"] = "60"
                self.session.synchronize()
                baseline = len(self.calls())
                self.send("play")
                self.send("next")
                wanted = ["--list-all"] if stage == "LIST" else ["--player", "mpv.first", "play"]
                self.wait(lambda: wanted in self.calls()[baseline:])
                generation = self.session.generation
                self.send("scroll")
                self.assertEqual((self.session.mode, self.session.generation), ("scroll", generation + 1))
                os.environ.pop(f"VOICE_MEDIA_{stage}_DELAY")
                self.send("pause")
                self.wait(lambda: len(self.events) == 1)
                self.assertEqual((self.events[0][0], self.events[0][1]["command"]), ("executed", "pause"))
                self.assertNotIn(["--player", "mpv.first", "next"], self.calls())
                self.wait(lambda: len(self.media.processes) == 1)  # tracker only

    def test_timeout_and_stop_are_responsive_and_reap_helpers(self):
        os.environ["VOICE_MEDIA_LIST_DELAY"] = "60"
        self.media.timeout_ms = 100
        self.session.synchronize()
        ticks = []
        timer = QTimer()
        timer.setInterval(5)
        timer.timeout.connect(lambda: ticks.append(True))
        timer.start()
        try:
            self.send("play")
            self.wait(lambda: bool(self.events))
            self.assertGreater(len(ticks), 5)
            self.assertEqual(self.events[0][1]["error"], "Media call timed out")
            self.send("pause")
            self.media.stop()
            self.wait(lambda: not self.media.processes)
            self.assertFalse(self.media.startup.isActive())
            self.assertFalse(self.media.queue)
            self.assertIsNone(self.media.current)
            self.assertIsNone(self.media.tracker)
            self.send("next")
            QTest.qWait(20)
            self.assertEqual(len(self.events), 1)
        finally:
            timer.stop()

    def test_help_cancels_inflight_and_queued_media_but_keeps_tracker_alive(self):
        for stage in ("LIST", "ACTION"):
            with self.subTest(stage=stage):
                self.events.clear()
                os.environ[f"VOICE_MEDIA_{stage}_DELAY"] = "60"
                self.session.synchronize()
                baseline = len(self.calls())
                self.send("play")
                self.send("next")
                wanted = ["--list-all"] if stage == "LIST" else ["--player", "mpv.first", "play"]
                self.wait(lambda: wanted in self.calls()[baseline:])
                tracker = self.media.tracker
                generation = self.session.generation
                presented_states = []
                def presented():
                    presented_states.append((bool(self.media.queue), self.media.current,
                                             self.media.live, self.media.tracker is tracker))
                self.session.help_requested.connect(presented)
                self.send("show help")
                self.session.help_requested.disconnect(presented)
                self.assertEqual(presented_states, [(False, None, True, True)])
                self.assertEqual(self.session.generation, generation + 1)
                os.environ.pop(f"VOICE_MEDIA_{stage}_DELAY")
                self.send("pause", generation)  # stale recognition cannot enqueue a new action
                self.assertFalse(self.media.queue)
                self.send("pause")
                self.wait(lambda: len(self.events) == 1)
                self.assertEqual((self.events[0][0], self.events[0][1]["command"]), ("executed", "pause"))
                self.assertNotIn(["--player", "mpv.first", "next"], self.calls())
                self.wait(lambda: len(self.media.processes) == 1)
                self.assertIs(self.media.tracker, tracker)

    def test_failed_start_and_lifetime_restart(self):
        self.media.playerctl = str(self.root / "missing")
        self.session.synchronize()
        self.send("play")
        self.wait(lambda: len(self.events) == 1)
        self.assertEqual(self.events[0][0], "action-failed")
        self.media.stop()
        self.wait(lambda: not self.media.processes)
        self.media.playerctl = str(self.child)
        self.session.synchronize()
        self.send("pause")
        self.wait(lambda: len(self.events) == 2)
        self.assertEqual(self.events[-1][0], "executed")

    def test_starting_process_cancellation_and_existing_tracker_exit(self):
        # No event-loop turn between start and stop: both helper and calls may
        # still be in QProcess.Starting, with no PID available to kill yet.
        self.session.synchronize()
        old_tracker = self.media.tracker
        self.media.stop()
        self.session.synchronize()
        self.assertIs(self.media.tracker, old_tracker)
        self.wait(lambda: self.media.tracker is not None and self.media.tracker is not old_tracker)
        self.media.stop()
        self.wait(lambda: not self.media.processes)
        os.environ["VOICE_MEDIA_EXISTING_TRACKER"] = "1"
        self.session.synchronize()
        self.wait(lambda: self.media.settled)
        self.send("play")
        self.send("scroll")
        self.assertEqual(self.session.mode, "scroll")
        self.send("pause")
        self.wait(lambda: len(self.events) == 1 and not self.media.processes)
        self.assertEqual((self.events[0][0], self.events[0][1]["command"]), ("executed", "pause"))
        self.assertIsNone(self.media.tracker)
        self.assertFalse(self.media.pending_calls)

    def test_action_timeout_advances_queue_without_fallback(self):
        os.environ["VOICE_MEDIA_ACTION_DELAY"] = "60"
        # Allow interpreter startup under concurrent Nix builds; the deliberate
        # action stall, rather than fixture startup, must trigger each timeout.
        self.media.timeout_ms = 1000
        self.session.synchronize()
        self.send("play")
        self.send("next")
        self.wait(lambda: len(self.events) == 2, timeout=6000)
        self.assertEqual([details["command"] for _, details in self.events], ["play", "next"])
        self.assertTrue(all(state == "action-failed" and details["error"] == "Media call timed out"
                            for state, details in self.events))
        self.assertEqual([call for call in self.calls() if call],
                         [["--list-all"], ["--player", "mpv.first", "play"],
                          ["--list-all"], ["--player", "mpv.first", "next"]])

    def test_real_groups_keep_navigation_timeout_and_generation_on_media(self):
        desktop = DesktopCommands(QApplication.instance(), "unused", features=Features(mouse_grid=True))
        events = []
        desktop.media.log_event = lambda state, **details: events.append((state, details))
        try:
            desktop.session.synchronize()
            with patch.object(QApplication.instance(), "platformName", return_value="wayland"):
                for group in ("scroll", "click"):
                    desktop.command(group, desktop.session.generation)
                    if group == "click":
                        desktop.command("three", desktop.session.generation)
                    mode, generation = desktop.session.mode, desktop.session.generation
                    for command in ("play", "pause", "next", "previous"):
                        desktop.command(command, generation)
                    self.assertEqual((desktop.session.mode, desktop.session.generation), (mode, generation))
                    self.assertTrue(desktop.scroll.active if group == "scroll" else desktop.grid.active)
                self.assertEqual(len(events), 8)
                self.assertFalse(desktop.media.processes)
        finally:
            desktop.cleanup()
            desktop.deleteLater()

    def test_tray_lifetime_pause_crash_quit_stop_media_and_resume_fresh(self):
        app = QApplication.instance()
        desktop = DesktopCommands(app, "unused", execute=True, features=Features(scroll=False))
        desktop.media.cleanup()
        desktop.media.deleteLater()
        desktop.media = desktop.session.media = self.media
        child = str(Path(__file__).with_name("listener_test_child.py"))
        controller = TrayController(app, sys.executable, ["-u", child, "transport"], desktop)
        try:
            for ending in ("pause", "crash", "quit"):
                controller.start()
                self.wait(lambda: controller.listener.state == "Listening")
                self.assertTrue(self.media.live)
                os.environ["VOICE_MEDIA_LIST_DELAY"] = "60"
                baseline = len(self.calls())
                desktop.command("play", desktop.session.generation)
                desktop.command("next", desktop.session.generation)
                self.wait(lambda: ["--list-all"] in self.calls()[baseline:])
                if ending == "pause":
                    controller.stop()
                elif ending == "crash":
                    controller.listener._process.kill()
                else:
                    controller.quit()
                self.wait(lambda: controller.listener.state in ("Paused", "Error") and not self.media.processes)
                self.assertFalse(self.media.live)
                self.assertFalse(self.media.queue)
                self.assertIsNone(self.media.current)
                self.assertFalse(self.media.startup.isActive())
                self.assertFalse(self.events)
        finally:
            controller.cleanup()
            # The test owns media's QObject lifetime, not desktop's.
            desktop.media = desktop.session.media = None
            controller.deleteLater()
            desktop.deleteLater()

    def test_all_feature_combinations_preview_and_disabled_helpers_absent(self):
        for media, scroll, grid, execute in itertools.product((False, True), repeat=4):
            with self.subTest(media=media, scroll=scroll, grid=grid, execute=execute):
                desktop = DesktopCommands(QApplication.instance(), "unused", execute=execute,
                                          features=Features(media, scroll, grid))
                try:
                    self.assertEqual(desktop.media is not None, media)
                    self.assertEqual(desktop.scroll is not None, scroll)
                    self.assertEqual(desktop.grid is not None, grid)
                    if not execute or not media:
                        desktop.session.synchronize()
                        desktop.command("play", desktop.session.generation)
                        if desktop.media:
                            self.assertFalse(desktop.media.processes)
                            self.assertIsNone(desktop.media.tracker)
                finally:
                    desktop.cleanup()
                    desktop.deleteLater()
