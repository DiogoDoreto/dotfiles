"""Behavior checks; VOICE_GRID_DESKTOP_TEST=1 also verifies real KWin input delivery."""

import os
import json
from pathlib import Path
import subprocess
import sys
from types import SimpleNamespace
import unittest
from unittest.mock import patch

from PySide6.QtTest import QTest
from PySide6.QtCore import QProcess
from PySide6.QtWidgets import QApplication, QWidget

from grid import GridController, Region
from runner import VoskRecognizer, audio
from vocabulary import GRID_COMMANDS, phrases_for

app = QApplication([])
app.setQuitOnLastWindowClosed(False)
DESKTOP = os.environ.get("VOICE_GRID_DESKTOP_TEST") == "1"


class GridTests(unittest.TestCase):
    def setUp(self):
        self.grid = GridController(app, os.environ["VOICE_TEST_POINTER"], timeout=0.1)
        self.events = []
        self.modes = []
        self.grid.log_event = lambda state, **details: self.events.append((state, details))
        self.grid.mode_changed.connect(lambda mode, generation: self.modes.append((mode, generation)))
        # Offscreen checks exercise real QML/controller code without claiming a Wayland test.
        self.platform = patch.object(app, "platformName", return_value="wayland") if not DESKTOP else None
        if self.platform:
            self.platform.start()

    def tearDown(self):
        self.grid.cleanup()
        if self.grid.engine:
            self.grid.engine.deleteLater()
        self.grid.deleteLater()
        QTest.qWait(1)
        if self.platform:
            self.platform.stop()

    def say(self, command, generation=None):
        self.grid.command(command, self.grid.generation if generation is None else generation)

    def test_narrow_back_reset_and_click_preview(self):
        self.say("one")
        self.assertFalse(self.grid.active)
        self.say("click")
        self.assertTrue(self.grid.active)
        root = self.grid.history[-1]
        self.say("three")
        self.say("seven")
        self.assertEqual(self.grid.history[-1], root.cell(3).cell(7))
        self.assertEqual(self.grid.path, [3, 7])
        self.say("back")
        self.assertEqual(self.grid.history[-1], root.cell(3))
        self.say("reset")
        self.assertEqual(self.grid.history, [root])
        self.say("back")
        self.assertEqual(self.grid.history, [root])
        self.say("click")
        self.assertFalse(self.grid.active)
        self.assertEqual(self.events[-1][0], "grid-would-click")
        self.assertEqual(self.grid.click_process.processId(), 0)
        self.assertEqual([mode for mode, generation in self.modes], ["grid", "normal"])

    def test_timeout_cancel_and_stale_commands(self):
        self.say("click")
        generation = self.grid.generation
        QTest.qWait(180)
        self.assertFalse(self.grid.active)
        self.say("click", generation)
        self.assertFalse(self.grid.active, "Late recognition must not reopen the overlay")
        self.say("click")
        self.say("cancel")
        self.assertFalse(self.grid.active)
        self.assertFalse(self.grid.window.isVisible())
        self.assertFalse(any(state == "grid-would-click" for state, details in self.events))

    def test_pixel_limit_and_display_change(self):
        self.say("click")
        for unused in range(20):
            self.say("nine")
        self.assertGreaterEqual(min(self.grid.history[-1].width, self.grid.history[-1].height), 1)
        self.assertTrue(any(state == "grid-limit" for state, details in self.events))
        self.grid.screen.geometryChanged.emit(self.grid.screen.geometry())
        self.assertFalse(self.grid.active)

    def test_pause_cancels_committed_but_pending_input(self):
        self.say("click")
        self.grid.execute = True
        self.grid.click_process.setProgram(sys.executable)
        set_arguments = self.grid.click_process.setArguments
        with patch.object(self.grid.click_process, "setArguments",
                          side_effect=lambda unused: set_arguments(["-c", "import time; time.sleep(60)"])):
            self.say("click")
        QTest.qWait(30)
        self.assertIsNotNone(self.grid.pending_click)
        self.grid.cancel("listening-stopped")
        QTest.qWait(50)
        self.assertEqual(self.grid.click_process.state(), QProcess.ProcessState.NotRunning)
        self.assertIsNone(self.grid.pending_click)
        self.assertFalse(any(state == "executed" for state, details in self.events))

    @unittest.skipUnless(DESKTOP, "requires an isolated KWin Wayland session")
    def test_real_absolute_click_and_focus(self):
        class Target(QWidget):
            def __init__(self):
                super().__init__()
                self.clicks = []

            def mousePressEvent(self, event):
                self.clicks.append(event.globalPosition())

        target = Target()
        target.showFullScreen()
        QTest.qWait(300)
        self.assertTrue(target.isActiveWindow())
        pointer = QProcess()
        pointer.start(os.environ["VOICE_TEST_POINTER"], ["120", "120"])
        QTest.qWait(250)
        self.assertEqual(pointer.exitCode(), 0)
        self.assertEqual(len(target.clicks), 1, "Pointer helper must deliver input before the overlay opens")
        target.clicks.clear()
        self.grid.execute = True
        self.grid.timer.setInterval(5000)
        self.say("click")
        QTest.qWait(100)
        self.assertTrue(self.grid.window.isVisible())
        self.assertTrue(target.isActiveWindow(), "Overlay must not take focus")
        # Actual input must pass through the visible overlay to the fullscreen target.
        pointer.start(os.environ["VOICE_TEST_POINTER"], ["100", "100"])
        QTest.qWait(250)
        self.assertEqual(pointer.exitCode(), 0)
        self.assertEqual(len(target.clicks), 1)
        self.assertAlmostEqual(target.clicks[-1].x(), 100, delta=1)
        self.say("three")
        self.say("seven")
        expected = self.grid.history[-1].center
        self.say("click")
        QTest.qWait(300)
        self.assertEqual(len(target.clicks), 2)
        self.assertAlmostEqual(target.clicks[-1].x(), expected[0], delta=1)
        self.assertAlmostEqual(target.clicks[-1].y(), expected[1], delta=1)
        self.assertEqual(self.events[-1][0], "executed")
        self.assertTrue(target.isActiveWindow())
        # A finite synthetic input stream must drain its committed click before EOF exit.
        synthetic = QProcess()
        tray = str(Path(sys.modules[GridController.__module__].__file__).with_name("tray.py"))
        synthetic.start(sys.executable, [tray, "--listener", "unused", "--pointer",
                                        os.environ["VOICE_TEST_POINTER"], "--grid-stdin", "--execute"])
        synthetic.write(b"click\n")
        QTest.qWait(300)
        self.assertEqual(synthetic.state(), QProcess.ProcessState.Running,
                         bytes(synthetic.readAllStandardError()).decode())
        synthetic.write(b"3\n7\nclick\n")
        synthetic.closeWriteChannel()
        for unused in range(100):
            QTest.qWait(20)
            if synthetic.state() == QProcess.ProcessState.NotRunning:
                break
        self.assertEqual(synthetic.state(), QProcess.ProcessState.NotRunning)
        self.assertEqual(synthetic.exitCode(), 0, bytes(synthetic.readAllStandardError()).decode())
        self.assertEqual(len(target.clicks), 3)
        self.assertIn('"state": "executed"', bytes(synthetic.readAllStandardOutput()).decode())
        target.close()


class StartupTests(unittest.TestCase):
    def test_overlay_frame_without_egl(self):
        environment = dict(os.environ)
        # Offscreen Qt normally picks software automatically. Request Wayland's
        # RHI/OpenGL default explicitly so the test catches the desktop failure.
        environment["QT_QUICK_BACKEND"] = "rhi"
        environment["QSG_RHI_BACKEND"] = "opengl"
        environment["QT_QPA_PLATFORM"] = "offscreen"
        environment["QT_FORCE_STDERR_LOGGING"] = "1"
        grid = sys.modules[GridController.__module__].__file__
        result = subprocess.run([sys.executable, "-B", os.environ["VOICE_TEST_RENDERING_PROBE"], grid],
                                env=environment, capture_output=True, text=True, timeout=10)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("Overlay frame and crosshair rendered without EGL", result.stdout)

    def test_grid_stdin_after_older_desktop_crypto(self):
        tray = str(Path(sys.modules[GridController.__module__].__file__).with_name("tray.py"))
        result = subprocess.run([sys.executable, "-B", os.environ["VOICE_TEST_STARTUP_PROBE"],
                                 tray, os.environ["VOICE_TEST_OLD_CRYPTO"]],
                                stdin=subprocess.DEVNULL, capture_output=True, text=True, timeout=10)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn('"state": "grid-stdin"', result.stdout)


class RecognitionTests(unittest.TestCase):
    def test_mode_boundary_discards_upstream_capture_and_closes_processes(self):
        processes = []
        popen = subprocess.Popen

        def capture(*unused, **kwargs):
            # The unread half of each real pipe simulates speech queued before cancellation.
            marker = len(processes) + 1
            process = popen([sys.executable, "-c",
                             f"import sys,time; sys.stdout.buffer.write(bytes([{marker}])*6400); "
                             "sys.stdout.buffer.flush(); time.sleep(60)"], **kwargs)
            processes.append(process)
            return process

        with patch("runner.subprocess.Popen", side_effect=capture):
            with audio(SimpleNamespace(wav=None, source=None)) as (read, reset):
                self.assertEqual(read(), bytes([1]) * 3200)
                reset()
                self.assertIsNotNone(processes[0].poll())
                self.assertEqual(read(), bytes([2]) * 3200, "Old buffered PCM must not enter the new mode")
        self.assertTrue(all(process.poll() is not None for process in processes))

    def test_mode_specific_exact_final_results(self):
        args = SimpleNamespace(model=os.environ["VOICE_TEST_MODEL"], verbose=False)
        recognizer = VoskRecognizer(args, phrases_for("hey computer", True))
        model = recognizer.model
        for text in ["click", "one", "hey computer", "ordinary speech", "hey computer click one"]:
            self.assertEqual(recognizer.result(json.dumps({"text": text})), [])
        self.assertEqual(recognizer.result('{"text":"hey computer click"}'), ["click"])
        self.assertEqual(recognizer.result('{"text":"hey computer play"}'), ["play"])
        recognizer.set_phrases(phrases_for("hey computer", True, "grid"))
        for command in GRID_COMMANDS:
            self.assertEqual(recognizer.result(json.dumps({"text": command})), [command])
        self.assertEqual(recognizer.result('{"text":"one two"}'), [])
        self.assertEqual(recognizer.feed(bytes(3200)), [])
        recognizer.set_phrases(phrases_for("hey computer", True))
        self.assertIs(recognizer.model, model)
        self.assertEqual(recognizer.result('{"text":"click"}'), [])

    def test_feature_disabled_keeps_media_only_grammar(self):
        self.assertEqual(set(phrases_for("hey computer").values()), {"play", "pause", "next", "previous"})
        region = Region(0, 0, 1920, 1080).cell(3).cell(7)
        self.assertAlmostEqual(region.center[0], 1386.6666667)
        self.assertAlmostEqual(region.center[1], 300)


if __name__ == "__main__":
    unittest.main()
