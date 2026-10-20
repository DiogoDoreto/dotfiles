"""Feature combinations across recognition, CLI assembly, and action boundaries."""

from contextlib import nullcontext
import itertools
import json
import os
import sys
from types import SimpleNamespace
import unittest
from unittest.mock import Mock, patch

from PySide6.QtWidgets import QApplication

from desktop import DesktopCommands
import runner
import tray
from vocabulary import phrases_for


class FeatureTests(unittest.TestCase):
    def test_recognition_excludes_disabled_commands_in_every_mode(self):
        recognizer = runner.VoskRecognizer(
            SimpleNamespace(model=os.environ["VOICE_TEST_MODEL"], verbose=False),
            phrases_for("hey computer"))
        for media, scroll, grid in itertools.product((False, True), repeat=3):
            for mode in ("normal", "scroll", "grid"):
                with self.subTest(media=media, scroll=scroll, grid=grid, mode=mode):
                    recognizer.set_phrases(phrases_for("hello computer", grid, mode,
                                                     media=media, scroll=scroll))
                    for text, expected in [
                        ("hello computer play", ["play"] if media else []),
                        ("hello computer pause", ["pause"] if media else []),
                        ("hello computer scroll", ["scroll"] if scroll else []),
                        ("hello computer click", ["click"] if grid else []),
                        ("up up down", ["up", "up", "down"] if scroll and mode == "scroll" else []),
                        ("three", ["three"] if grid and mode == "grid" else []),
                        ("cancel", ["cancel"] if (scroll and mode == "scroll") or (grid and mode == "grid") else []),
                        ("hey computer play", []),
                        ("play", []),
                        ("hello computer", []),
                        ("please open the window", []),
                    ]:
                        self.assertEqual(recognizer.result(json.dumps({"text": text})), expected, text)

    def run_listener(self, flags, control=None):
        # Bypass recognition: dispatch must reject disabled/out-of-mode actions
        # even if a decoder produces an unexpected result.
        recognizer = Mock()
        recognizer.feed.return_value = ["play", "pause", "next", "previous", "scroll", "click", "up", "one", "unknown"]
        read = Mock(side_effect=[bytes(3200), b""])
        events = []
        with patch.object(sys, "argv", ["listener", "--model", "unused", *flags]), patch.object(
                runner, "VoskRecognizer", return_value=recognizer), patch.object(
                runner, "audio", return_value=nullcontext((read, Mock()))), patch.object(
                runner, "event", side_effect=lambda state, **details: events.append((state, details))), patch.object(
                runner, "media") as media, patch.object(runner.subprocess, "Popen") as process, patch.object(
                runner.time, "sleep"), patch.object(runner.signal, "signal"), patch.object(
                runner.select, "select", return_value=([sys.stdin], [], [])), patch.object(
                runner.os, "read", return_value=json.dumps(control).encode() + b"\n"):
            status = runner.main()
        return status, events, media, process

    def test_listener_dispatch_and_media_helper_feature_matrix(self):
        for media, scroll, grid, execute in itertools.product((False, True), repeat=4):
            with self.subTest(media=media, scroll=scroll, grid=grid, execute=execute):
                flags = ["--media" if media else "--no-media",
                         "--scroll" if scroll else "--no-scroll",
                         "--mouse-grid" if grid else "--no-mouse-grid",
                         "--execute" if execute else "--no-execute"]
                status, events, dispatch, process = self.run_listener(flags)
                self.assertEqual(status, 0)
                commands = [details["command"] for state, details in events if state == "command"]
                self.assertEqual(commands, (["play", "pause", "next", "previous"] if media else [])
                                 + (["scroll"] if scroll else []) + (["click"] if grid else []))
                self.assertEqual(dispatch.call_count, 4 if media and execute else 0)
                self.assertEqual(process.call_count, int(media and execute))

    def test_disabled_controller_modes_are_rejected(self):
        for mode, flag in [("scroll", "--no-scroll"), ("grid", "--no-mouse-grid")]:
            with self.subTest(mode=mode):
                status, events, dispatch, process = self.run_listener(
                    [flag, "--no-media", "--execute", "--control-stdin"],
                    {"mode": mode, "generation": 1})
                self.assertEqual(status, 1)
                self.assertIn(("error", {"error": "Invalid controller mode/generation"}), events)
                dispatch.assert_not_called()
                process.assert_not_called()

    def test_standalone_defaults_and_existing_mouse_grid_flag(self):
        for flags, expected in [([], ["play", "pause", "next", "previous", "scroll"]),
                                (["--mouse-grid"], ["play", "pause", "next", "previous", "scroll", "click"])]:
            with self.subTest(flags=flags):
                status, events, dispatch, process = self.run_listener(flags)
                self.assertEqual(status, 0)
                self.assertEqual([details["command"] for state, details in events if state == "command"], expected)
                dispatch.assert_not_called()
                process.assert_not_called()

    def test_tray_passes_effective_features_and_execution_to_both_processes(self):
        app = QApplication.instance()
        for media, scroll, grid, execute in itertools.product((False, True), repeat=4):
            flags = ["--media" if media else "--no-media",
                     "--scroll" if scroll else "--no-scroll",
                     "--mouse-grid" if grid else "--no-mouse-grid",
                     "--execute" if execute else "--no-execute"]
            with self.subTest(flags=flags), patch.object(sys, "argv", [
                    "tray", "--listener", "listener", "--pointer", "pointer", "--start-paused", *flags]), patch.object(
                    tray, "QApplication", return_value=app), patch.object(
                    app, "exec", return_value=0), patch.object(
                    tray.signal, "signal"), patch.object(
                    tray.QSystemTrayIcon, "isSystemTrayAvailable", return_value=True), patch.object(
                    tray, "TrayController") as controller, patch("desktop.DesktopCommands") as desktop:
                self.assertEqual(tray.main(), 0)
                kwargs = desktop.call_args.kwargs
                self.assertEqual((kwargs["execute"], kwargs["scroll"], kwargs["mouse_grid"]),
                                 (execute, scroll, grid))
                arguments = controller.call_args.args[2]
                for flag in flags:
                    self.assertIn(flag, arguments)

    def test_disabled_desktop_controllers_are_absent_and_cannot_dispatch(self):
        app = QApplication.instance()
        for scroll, grid in itertools.product((False, True), repeat=2):
            with self.subTest(scroll=scroll, grid=grid):
                desktop = DesktopCommands(app, "must-not-run", execute=True, scroll=scroll, mouse_grid=grid)
                try:
                    self.assertEqual(desktop.scroll is not None, scroll)
                    self.assertEqual(desktop.grid is not None, grid)
                    if not scroll:
                        desktop.command("scroll", 0)
                        desktop.command("up", 0)
                        self.assertEqual(desktop.session.mode, "normal")
                    if not grid:
                        desktop.command("click", 0)
                        desktop.command("one", 0)
                        self.assertEqual(desktop.session.mode, "normal")
                    # A disabled entry must not close or reset an enabled group.
                    if scroll and not grid:
                        desktop.command("scroll", 0)
                        generation = desktop.session.generation
                        desktop.command("click", generation)
                        desktop.command("one", generation)
                        self.assertEqual((desktop.session.mode, desktop.session.generation), ("scroll", generation))
                    if grid and not scroll:
                        with patch.object(app, "platformName", return_value="wayland"):
                            desktop.command("click", 0)
                        generation = desktop.session.generation
                        desktop.command("scroll", generation)
                        desktop.command("up", generation)
                        self.assertEqual((desktop.session.mode, desktop.session.generation), ("grid", generation))
                    desktop.cancel("test")
                    desktop.session.synchronize()
                finally:
                    desktop.cleanup()
                    desktop.deleteLater()
