"""Scroll-group state, recognition boundaries, and optional real desktop input."""

import json
import io
import os
import subprocess
import struct
import sys
from types import SimpleNamespace
import unittest
from unittest.mock import patch
import wave

from PySide6.QtCore import QProcess
from PySide6.QtTest import QTest
from PySide6.QtWidgets import QApplication, QWidget

from desktop import DesktopCommands
from runner import VoskRecognizer
from tray import display_state
from vocabulary import phrases_for


app = QApplication.instance() or QApplication([])
app.setQuitOnLastWindowClosed(False)


class ScrollTests(unittest.TestCase):
    def setUp(self):
        self.desktop = DesktopCommands(app, os.environ["VOICE_TEST_POINTER"], mouse_grid=True)
        self.events = []
        self.desktop.scroll.log_event = lambda state, **details: self.events.append((state, details))
        self.desktop.grid.log_event = lambda state, **details: self.events.append((state, details))
        self.transitions = []
        self.desktop.session.changed.connect(lambda mode, generation: self.transitions.append((mode, generation)))

    def tearDown(self):
        self.desktop.cleanup()
        self.desktop.deleteLater()
        QTest.qWait(1)

    def say(self, command, generation=None):
        self.desktop.command(command, self.desktop.session.generation if generation is None else generation)

    def test_tray_labels_active_scroll_group(self):
        self.assertEqual(display_state("Listening", True), "Scrolling")
        self.assertEqual(display_state("Listening", False), "Listening")
        self.assertEqual(display_state("Paused", True), "Paused")
        self.assertEqual(display_state("Error", True), "Error")

    def test_repeated_preview_and_cancel(self):
        self.assertEqual(self.desktop.scroll.timer.interval(), 60_000)
        self.say("up")
        self.say("down")
        self.assertEqual(self.events, [])
        self.say("scroll")
        generation = self.desktop.session.generation
        for command in ["up", "up", "down", "down", "up"]:
            self.say(command)
        directions = [details["direction"] for state, details in self.events if state == "scroll-would-scroll"]
        self.assertEqual(directions, ["up", "up", "down", "down", "up"])
        self.assertTrue(self.desktop.scroll.active)
        self.assertEqual(self.desktop.scroll.process.processId(), 0)
        self.say("cancel")
        self.assertEqual(self.desktop.session.mode, "normal")
        self.assertFalse(self.desktop.scroll.timer.isActive())
        count = len(self.events)
        self.say("up", generation)
        self.say("scroll", generation)
        self.assertEqual(len(self.events), count, "Stale results must not reopen the group or scroll")

    def test_inactivity_timeout_restarts_on_direction(self):
        self.desktop.scroll.timer.setInterval(100)
        self.say("scroll")
        QTest.qWait(70)
        self.say("down")
        QTest.qWait(70)
        self.assertTrue(self.desktop.scroll.active)
        QTest.qWait(80)
        self.assertEqual(self.desktop.session.mode, "normal")
        self.assertEqual(self.events[-1], ("scroll-closed", {"reason": "timeout"}))

    def test_switching_groups_uses_one_generation(self):
        screen = app.screens()[0]
        with patch.object(app, "platformName", return_value="wayland"), patch.object(
                app, "screens", return_value=[screen, screen]):
            self.say("click")
            self.assertEqual(self.events[-1][0], "grid-monitors")
            monitor_generation = self.desktop.session.generation
            self.say("two")
            self.say("three", monitor_generation)
            self.assertEqual(self.desktop.grid.path, [], "A monitor-stage result must not refine the grid")
            self.say("three")
            self.assertEqual(self.desktop.grid.path, [3])
            self.say("back")
            self.say("back")
            self.assertEqual(self.events[-1][0], "grid-monitors")
            self.say("one")
            grid_generation = self.desktop.session.generation
            self.say("scroll")
            self.assertFalse(self.desktop.grid.active)
            self.assertEqual(self.desktop.grid.windows, [])
            self.assertTrue(self.desktop.scroll.active)
            scroll_generation = self.desktop.session.generation
            self.say("click", grid_generation)
            self.assertTrue(self.desktop.scroll.active)
            self.say("click")
            self.assertFalse(self.desktop.scroll.active)
            self.assertTrue(self.desktop.grid.active)
            self.assertFalse(self.desktop.scroll.timer.isActive())
            self.assertEqual(self.transitions, [("grid", 1), ("grid", 2), ("grid", 3), ("grid", 4),
                                                ("scroll", 5), ("grid", 6)],
                              "Group switches must not pass through normal mode")
            self.say("up", scroll_generation)
            self.assertEqual(self.desktop.grid.path, [])
            self.desktop.cancel("listener-exited")
            self.assertEqual(self.transitions[-1], ("normal", 7))
            self.assertEqual(self.desktop.session.mode, "normal")
            self.assertFalse(self.desktop.grid.active)

    def test_lifecycle_cancels_committed_click_after_overlay_closed(self):
        grid = self.desktop.grid
        grid.execute = True
        grid.timer.setInterval(5000)
        grid.click_process.setProgram(sys.executable)
        set_arguments = grid.click_process.setArguments
        with patch.object(app, "platformName", return_value="wayland"), patch.object(
                grid.click_process, "setArguments",
                side_effect=lambda unused: set_arguments(["-c", "import time; time.sleep(60)"])):
            for event in ["scroll", "pause", "restart"]:
                with self.subTest(event=event):
                    self.say("click")
                    self.say("click")
                    QTest.qWait(30)
                    self.assertIsNotNone(grid.pending_click)
                    self.assertFalse(grid.active)
                    old_generation = self.desktop.session.generation
                    if event == "scroll":
                        self.say("scroll")
                    elif event == "pause":
                        self.desktop.cancel("listening-stopped")
                    else:
                        self.desktop.session.synchronize()
                    QTest.qWait(50)
                    self.assertIsNone(grid.pending_click)
                    self.assertEqual(grid.click_process.state(), QProcess.ProcessState.NotRunning)
                    self.assertEqual(self.desktop.scroll.active, event == "scroll")
                    self.assertIn(("grid-click-cancelled", {"reason": {
                        "scroll": "mode-changed", "pause": "listening-stopped", "restart": "listener-restarted",
                    }[event]}), self.events)
                    self.assertEqual(self.desktop.session.generation, old_generation + (event == "scroll"))
        self.assertFalse(any(state == "executed" for state, details in self.events))

    def test_failed_grid_entry_cancels_scroll_and_returns_to_normal_once(self):
        scroll = self.desktop.scroll
        scroll.execute = True
        scroll.process.setProgram(sys.executable)
        set_arguments = scroll.process.setArguments
        with patch.object(scroll.process, "setArguments",
                          side_effect=lambda unused: set_arguments(["-c", "import time; time.sleep(60)"])):
            self.say("scroll")
            self.say("up")
            self.say("down")
        QTest.qWait(30)
        generation = self.desktop.session.generation
        with patch.object(app, "screens", return_value=[]):
            self.say("click")
            self.assertEqual(self.transitions, [("scroll", 1), ("normal", 2)])
            self.assertFalse(self.desktop.grid.active)
            self.assertFalse(scroll.timer.isActive())
            self.assertEqual(scroll.process.state(), QProcess.ProcessState.NotRunning)
            self.assertEqual(list(scroll.queue), [])
            self.assertIn("grid-error", [state for state, details in self.events])
            count = len(self.events)
            self.say("scroll", generation)
            self.say("down", generation)
            self.assertEqual(len(self.events), count)
            self.say("click")
            self.assertEqual(self.events[-1][0], "grid-error")
            self.assertEqual(self.transitions, [("scroll", 1), ("normal", 2)],
                             "Failed entry from normal must not reset recognition again")
        self.assertFalse(any(state == "executed" for state, details in self.events))

    def test_display_change_and_timeout_close_groups_and_reject_old_results(self):
        self.desktop.grid.timer.setInterval(50)
        self.desktop.scroll.timer.setInterval(50)
        with patch.object(app, "platformName", return_value="wayland"):
            for entry in ["click", "scroll"]:
                for cause in ["timeout", "display-change"]:
                    with self.subTest(entry=entry, cause=cause):
                        self.say(entry)
                        generation = self.desktop.session.generation
                        if cause == "timeout":
                            QTest.qWait(100)
                        else:
                            screen = app.screens()[0]
                            screen.geometryChanged.emit(screen.geometry())
                        self.assertFalse(self.desktop.grid.active)
                        self.assertFalse(self.desktop.scroll.active)
                        self.assertEqual(self.desktop.grid.windows, [])
                        self.assertFalse(self.desktop.scroll.timer.isActive())
                        self.assertEqual(self.transitions[-1], ("normal", generation + 1))
                        count = len(self.events)
                        for command in ["click", "scroll", "one", "up"]:
                            self.say(command, generation)
                        self.assertEqual(len(self.events), count)

    def test_restart_synchronizes_normal_and_preserves_generation_safety(self):
        session = self.desktop.session
        # Initial readiness publishes the zero snapshot without an unnecessary reset.
        session.synchronize()
        self.assertEqual(self.transitions, [("normal", 0)])
        with patch.object(app, "platformName", return_value="wayland"):
            self.say("click")
            self.say("three")
            generation = session.generation
            session.synchronize()
            self.assertEqual(self.transitions, [("normal", 0), ("grid", 1), ("normal", 2)])
            self.assertFalse(self.desktop.grid.active)
            self.assertEqual(self.desktop.grid.windows, [])
            count = len(self.events)
            self.say("click", 0)  # A new listener can have queued results before it reads control.
            self.say("click", generation)
            self.assertEqual(len(self.events), count)
            self.say("scroll")
            self.say("down")
            self.assertEqual(self.events[-1], ("scroll-would-scroll", {"direction": "down"}))
            self.desktop.cancel("listener-exited")
            session.synchronize()
            self.assertEqual(self.transitions[-2:], [("normal", 4), ("normal", 4)],
                             "Readiness resends the current snapshot, not generation zero")
            count = len(self.events)
            self.say("scroll", 0)
            self.say("up", 3)
            self.assertEqual(len(self.events), count)

    def test_cancel_discards_inflight_and_queued_actions(self):
        scroll = self.desktop.scroll
        scroll.execute = True
        scroll.process.setProgram(sys.executable)
        set_arguments = scroll.process.setArguments
        with patch.object(scroll.process, "setArguments",
                          side_effect=lambda unused: set_arguments(["-c", "import time; time.sleep(60)"])):
            self.say("scroll")
            for command in ["up", "down", "up"]:
                self.say(command)
        QTest.qWait(30)
        self.assertEqual(scroll.current, "up")
        self.assertEqual(list(scroll.queue), ["down", "up"])
        self.say("cancel")
        QTest.qWait(50)
        self.assertEqual(scroll.process.state(), QProcess.ProcessState.NotRunning)
        self.assertIsNone(scroll.current)
        self.assertEqual(list(scroll.queue), [])
        self.assertFalse(any(state == "executed" for state, details in self.events))

    def test_scroll_recognition_and_direction_sequences(self):
        recognizer = VoskRecognizer(SimpleNamespace(model=os.environ["VOICE_TEST_MODEL"], verbose=False),
                                    phrases_for("hey computer", True))
        final = lambda text: recognizer.result(json.dumps({"text": text}))
        self.assertEqual(final("hey computer scroll"), ["scroll"])
        for text in ["up", "down", "cancel", "up up down"]:
            self.assertEqual(final(text), [])
        recognizer.set_phrases(phrases_for("hey computer", True, "scroll"))
        for text in ["up", "down", "cancel"]:
            self.assertEqual(final(text), [text])
        self.assertEqual(final("up up down down up"), ["up", "up", "down", "down", "up"])
        self.assertEqual(final("please go down"), [])
        self.assertEqual(final("one"), [])
        self.assertEqual(final("hey computer click"), ["click"])
        self.assertEqual(final("hey computer pause"), ["pause"])
        recognizer.set_phrases(phrases_for("hey computer", True, "grid"))
        self.assertEqual(final("up down"), [])
        recognizer.set_phrases(phrases_for("hey computer"))
        self.assertEqual(final("up"), [])

    def test_pointer_rejects_invalid_scroll_direction(self):
        result = subprocess.run([os.environ["VOICE_TEST_POINTER"], "--scroll", "sideways"],
                                capture_output=True, text=True, timeout=5)
        self.assertEqual(result.returncode, 1)
        self.assertIn("Scroll direction must be up or down", result.stderr)

    def test_spoken_direction_sequence_preserves_repetitions(self):
        wav = subprocess.check_output([os.environ["VOICE_TEST_SPEECH"], "--stdout", "-v", "en-us", "-s", "140",
                                       "up, up, down, down, up"], timeout=5)
        with wave.open(io.BytesIO(wav)) as source:
            rate = source.getframerate()
            samples = [value for (value,) in struct.iter_unpack("<h", source.readframes(1_000_000))]
        samples = [samples[min(len(samples) - 1, int(index * rate / 16000))]
                   for index in range(int(len(samples) * 16000 / rate))]
        pcm = struct.pack(f"<{len(samples)}h", *samples) + bytes(32000)
        recognizer = VoskRecognizer(SimpleNamespace(model=os.environ["VOICE_TEST_MODEL"], verbose=False),
                                    phrases_for("hey computer", mode="scroll"))
        commands = []
        for offset in range(0, len(pcm), 3200):
            commands.extend(recognizer.feed(pcm[offset:offset + 3200]))
        commands.extend(recognizer.finish())
        self.assertEqual(commands, ["up", "up", "down", "down", "up"])


@unittest.skipUnless(os.environ.get("VOICE_GRID_DESKTOP_TEST") == "1", "requires isolated KWin Wayland")
class ScrollDesktopTests(unittest.TestCase):
    def test_real_repeated_scroll_at_current_pointer(self):
        class Target(QWidget):
            def __init__(self):
                super().__init__()
                self.wheels = []

            def wheelEvent(self, event):
                self.wheels.append((event.angleDelta().y(), event.globalPosition()))

        target = Target()
        desktop = DesktopCommands(app, os.environ["VOICE_TEST_POINTER"], execute=True)
        try:
            target.showFullScreen()
            QTest.qWait(300)
            position = target.geometry().center()
            pointer = QProcess()
            pointer.start(os.environ["VOICE_TEST_POINTER"], [str(position.x()), str(position.y())])
            QTest.qWait(250)
            self.assertEqual(pointer.exitCode(), 0)
            self.assertTrue(target.isActiveWindow())
            desktop.command("scroll", desktop.session.generation)
            for command in ["up", "up", "down", "down", "up"]:
                desktop.command(command, desktop.session.generation)
            for unused in range(250):
                QTest.qWait(20)
                if len(target.wheels) == 5 and desktop.scroll.current is None:
                    break
            # Qt scales wheel magnitudes on fractional-scale outputs; verify direction,
            # repetition, and stable pointer targeting rather than an app-specific distance.
            self.assertEqual([1 if delta > 0 else -1 if delta < 0 else 0 for delta, point in target.wheels],
                             [1, 1, -1, -1, 1])
            self.assertEqual(len({abs(delta) for delta, point in target.wheels}), 1)
            for delta, point in target.wheels:
                self.assertAlmostEqual(point.x(), position.x(), delta=1)
                self.assertAlmostEqual(point.y(), position.y(), delta=1)
            self.assertTrue(desktop.scroll.active)
            self.assertTrue(target.isActiveWindow())
            desktop.command("cancel", desktop.session.generation)
            self.assertEqual(desktop.session.mode, "normal")
        finally:
            desktop.cleanup()
            desktop.deleteLater()
            target.close()
