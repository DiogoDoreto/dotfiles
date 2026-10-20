"""Help through public desktop/tray interfaces, with real widgets and pending input."""

import itertools
import json
import os
from pathlib import Path
import sys
import tempfile
import time
import unittest
from unittest.mock import patch

from PySide6.QtCore import QProcess
from PySide6.QtTest import QTest
from PySide6.QtWidgets import QApplication, QTextBrowser

from desktop import DesktopCommands
from features import Features
from tray import TrayController


class HelpTests(unittest.TestCase):
    def setUp(self):
        self.app = QApplication.instance()
        self.desktop = DesktopCommands(self.app, "unused", features=Features(mouse_grid=True))

    def tearDown(self):
        self.desktop.cleanup()
        self.desktop.deleteLater()
        QTest.qWait(1)

    def say(self, command):
        self.desktop.command(command, self.desktop.session.generation)

    def test_typing_tray_help_pause_crash_quit_and_display_teardown(self):
        def wait(predicate):
            deadline = time.monotonic() + 3
            while not predicate() and time.monotonic() < deadline:
                QTest.qWait(5)
            self.assertTrue(predicate())

        for cause in ("help", "pause", "crash", "quit-start", "quit-stop", "display", "group"):
            with self.subTest(cause=cause), tempfile.TemporaryDirectory() as temp, patch.dict(
                    os.environ, {"VOICE_HANDY_TEST_CONFIG": str(Path(temp) / "config.json")}):
                desktop = DesktopCommands(self.app, "unused", execute=True,
                                          features=Features(False, False, False, True), wake="hello computer",
                                          handy=os.environ["VOICE_TEST_HANDY"])
                typing = desktop.typing
                log = Path(temp) / "calls"
                Path(os.environ["VOICE_HANDY_TEST_CONFIG"]).write_text(json.dumps({"log": str(log), "mode": "slow"}))
                script = 'import time; print(\'{"state":"listening"}\',flush=True); time.sleep(60)'
                controller = TrayController(self.app, sys.executable, ["-u", "-c", script], desktop)
                try:
                    with patch.object(self.app, "quit") as quit_app:
                        controller.start()
                        wait(lambda: controller.listener.state == "Listening")
                        desktop.command("type", desktop.session.generation)
                        if cause != "quit-start":
                            wait(lambda: typing.state == "waiting")
                            self.assertIn("waiting for speech (10s)", controller.status_action.text())
                        if cause == "help":
                            action = next(a for a in controller.menu.actions() if a.text() == "Help")
                            action.trigger()
                            text = desktop.help.window.findChild(QTextBrowser).toPlainText()
                            for phrase in ("hello computer type", "10 seconds", "3 continuous seconds", "already be running",
                                           "Wait for Handy to finish", "300 ms"):
                                self.assertIn(phrase, text)
                            self.assertEqual(desktop.session.mode, "normal")
                        elif cause == "pause":
                            controller.stop()
                        elif cause == "crash":
                            controller.listener._process.kill()
                            wait(lambda: controller.listener.state == "Error")
                        elif cause.startswith("quit"):
                            if cause == "quit-stop":
                                desktop.cancel("test-stop")
                            controller.quit()
                            quit_app.assert_not_called()
                        elif cause == "display":
                            screen = self.app.screens()[0]
                            screen.geometryChanged.emit(screen.geometry())
                        else:
                            desktop.session._enter("normal")
                        wait(lambda: not typing.busy)
                        self.assertEqual(len(log.read_text().splitlines()), 2)
                        self.assertEqual(desktop.session.mode, "normal")
                        if cause.startswith("quit"):
                            wait(lambda: quit_app.called)
                        else:
                            quit_app.assert_not_called()
                finally:
                    controller.cleanup()
                    desktop.cleanup()
                    controller.menu.deleteLater()
                    controller.deleteLater()
                    desktop.deleteLater()
                    QTest.qWait(1)

    def test_content_only_enabled_features_and_resolved_operation(self):
        for media, scroll, grid, execute in itertools.product((False, True), repeat=4):
            with self.subTest(media=media, scroll=scroll, grid=grid, execute=execute):
                desktop = DesktopCommands(self.app, "unused", features=Features(media, scroll, grid),
                                          execute=execute, wake="hello computer", grid_timeout=37)
                try:
                    desktop.show_help()
                    text = desktop.help.window.findChild(QTextBrowser).toPlainText()
                    self.assertIn("hello computer show help", text)
                    self.assertNotIn("hey computer", text)
                    self.assertIn("Pause listening / Resume listening", text)
                    self.assertEqual("Execution enabled" in text, execute)
                    self.assertEqual("Preview only" in text, not execute)
                    self.assertEqual("hello computer play" in text, media)
                    self.assertEqual("hello computer scroll" in text, scroll)
                    self.assertEqual("hello computer click" in text, grid)
                    self.assertEqual("37 inactive seconds" in text, grid)
                finally:
                    desktop.cleanup()
                    desktop.deleteLater()

    def test_voice_help_in_all_modes_cancels_before_show_and_rejects_stale(self):
        snapshots = []
        session = self.desktop.session
        session.changed.connect(lambda mode, generation: snapshots.append((mode, generation)))
        with patch.object(self.app, "platformName", return_value="wayland"):
            for entry in (None, "scroll", "click"):
                if entry:
                    self.say(entry)
                generation = session.generation
                presented_states = []
                def presented():
                    presented_states.append((session.mode, session.generation, self.desktop.grid.active,
                                             self.desktop.scroll.timer.isActive(), bool(self.desktop.grid.windows)))
                session.help_requested.connect(presented)
                self.say("show help")
                session.help_requested.disconnect(presented)
                self.assertEqual(presented_states, [("normal", generation + 1, False, False, False)])
                self.assertEqual(snapshots[-1], ("normal", generation + 1))
                window = self.desktop.help.window
                window.close()
                self.desktop.command("show help", generation)
                self.desktop.command("show help", True)
                self.assertFalse(window.isVisible())
                self.assertEqual(session.generation, generation + 1)

    def test_tray_help_reuses_window_while_paused_or_error_without_starting_listener(self):
        controller = TrayController(self.app, "must-not-start", [], self.desktop)
        try:
            action = next(action for action in controller.menu.actions() if action.text() == "Help")
            for state in ("Paused", "Error"):
                controller.listener.state = state
                controller.listener_state_changed(state)
                with patch.object(controller.listener, "start") as start:
                    action.trigger()
                    window = self.desktop.help.window
                    self.assertTrue(window.isVisible())
                    window.showMinimized()
                    action.trigger()
                    self.assertIs(self.desktop.help.window, window)
                    self.assertFalse(window.isMinimized())
                    window.close()
                    action.trigger()
                    self.assertIs(self.desktop.help.window, window)
                    self.assertTrue(window.isVisible())
                    self.assertEqual(controller.listener.state, state)
                    self.assertEqual(controller.listener._process.state(), QProcess.ProcessState.NotRunning)
                    start.assert_not_called()
            self.assertFalse(self.app.quitOnLastWindowClosed())
        finally:
            controller.cleanup()
            controller.menu.deleteLater()
            controller.deleteLater()

    def test_help_cancels_active_and_queued_scroll_before_presentation(self):
        scroll = self.desktop.scroll
        scroll.execute = True
        scroll.process.setProgram(sys.executable)
        set_arguments = scroll.process.setArguments
        with patch.object(scroll.process, "setArguments", side_effect=lambda unused:
                          set_arguments(["-c", "import time; time.sleep(60)"])):
            self.say("scroll")
            self.say("up")
            self.say("down")
        QTest.qWait(30)
        self.assertTrue(scroll.queue)
        self.assertNotEqual(scroll.process.state(), QProcess.ProcessState.NotRunning)
        presented_states = []
        def presented():
            presented_states.append((bool(scroll.queue), scroll.current, scroll.process.state()))
        self.desktop.session.help_requested.connect(presented)
        self.say("show help")
        self.desktop.session.help_requested.disconnect(presented)
        self.assertEqual(presented_states, [(False, None, QProcess.ProcessState.NotRunning)])

    def test_help_cancels_committed_click_after_overlay_closed(self):
        grid = self.desktop.grid
        grid.execute = True
        grid.click_process.setProgram(sys.executable)
        set_arguments = grid.click_process.setArguments
        with patch.object(self.app, "platformName", return_value="wayland"), patch.object(
                grid.click_process, "setArguments", side_effect=lambda unused:
                set_arguments(["-c", "import time; time.sleep(60)"])):
            self.say("click")
            self.say("click")
        QTest.qWait(30)
        self.assertFalse(grid.active)
        self.assertIsNotNone(grid.pending_click)
        self.say("show help")
        self.assertIsNone(grid.pending_click)
        QTest.qWait(50)  # Existing grid cancellation kills asynchronously.
        self.assertEqual(grid.click_process.state(), QProcess.ProcessState.NotRunning)
