"""Qt mouse-grid lifetime/presentation; targeting rules live in navigation."""

import json
import os
from pathlib import Path

from PySide6.QtCore import QObject, QProcess, QSocketNotifier, QTimer, QUrl, Slot
from PySide6.QtQml import QQmlApplicationEngine, QQmlComponent
from PySide6.QtQuick import QQuickWindow

from vocabulary import GRID_COMMANDS, NUMBERS
from session import CommandSession
from features import Features
from navigation import GridNavigation, Monitor


# This overlay is plain 2D geometry/text. Choose its renderer before any Quick
# window exists so opening the grid never depends on desktop EGL/OpenGL support.
QQuickWindow.setSceneGraphBackend("software")


class GridController(QObject):
    def __init__(self, app, pointer, execute=False, timeout=20, session=None):
        super().__init__(app)
        self.app = app
        self.pointer = pointer
        self.execute = execute
        self.session = session if session is not None else CommandSession(self, Features(mouse_grid=True))
        self.session.grid = self
        self.navigation = None
        self.screens = []
        self.windows = []
        self.screen = None
        self.engine = None
        self.component = None
        self.window = None
        self.pending_click = None
        self.click_process = QProcess(self)
        self.click_process.setProgram(pointer)
        self.click_process.finished.connect(self.click_finished)
        self.click_process.errorOccurred.connect(self.click_error)
        self.timer = QTimer(self)
        self.timer.setSingleShot(True)
        self.timer.setInterval(round(timeout * 1000))
        self.timer.timeout.connect(lambda: self.cancel("timeout"))
        app.screenAdded.connect(self.screen_added)
        app.screenRemoved.connect(self.display_removed)
        for screen in app.screens():
            self.watch_screen(screen)

    @property
    def generation(self):
        return self.session.generation

    @property
    def mode_changed(self):
        return self.session.changed

    @property
    def stage(self):
        return self.navigation.view.stage if self.navigation else None

    @property
    def active(self):
        return self.stage is not None

    def watch_screen(self, screen):
        # QObject slots disconnect with the controller. Lambdas retained by a
        # long-lived QScreen could otherwise cancel an already destroyed session.
        screen.geometryChanged.connect(self.display_changed)
        screen.logicalDotsPerInchChanged.connect(self.scale_changed)

    @Slot()
    def display_changed(self):
        self.cancel("display-changed")

    @Slot()
    def scale_changed(self):
        self.cancel("scale-changed")

    @Slot()
    def display_removed(self):
        self.cancel("display-removed")

    def screen_added(self, screen):
        self.cancel("display-added")
        self.watch_screen(screen)

    def log_event(self, state, **details):
        print(json.dumps({"state": state, "generation": self.generation, **details}), flush=True)

    def _open(self):
        if self.click_process.state() != QProcess.ProcessState.NotRunning:
            self.log_event("grid-error", error="Previous click is still pending")
            return
        if self.app.platformName() != "wayland":
            self.log_event("grid-error", error="Mouse grid requires Plasma Wayland with one to nine enabled displays")
            return
        observations = []
        for screen in self.app.screens():
            geometry = screen.geometry()
            observations.append((Monitor(screen.name(), geometry.x(), geometry.y(),
                                         geometry.width(), geometry.height()), screen))
        try:
            navigation = GridNavigation(monitor for monitor, screen in observations)
        except ValueError:
            self.log_event("grid-error", error="Mouse grid requires Plasma Wayland with one to nine enabled displays")
            return
        # Navigation reorders the original observations. Match by identity so
        # distinct screens with equal names/geometry keep their own overlays.
        screen_by_monitor = {id(monitor): screen for monitor, screen in observations}
        screens = [screen_by_monitor[id(monitor)] for monitor in navigation.view.monitors]
        if self.engine is None:
            self.engine = QQmlApplicationEngine(self)
            self.component = QQmlComponent(self.engine, QUrl.fromLocalFile(str(Path(__file__).with_name("grid.qml"))))
        self.screens = screens
        for number, screen in enumerate(screens, 1):
            # Every root owns its screen binding: a shared context property would move
            # all overlays to whichever monitor was assigned last.
            window = self.component.createWithInitialProperties({
                "targetScreen": screen, "monitorNumber": number, "monitorName": screen.name(),
                "dryRun": not self.execute,
            })
            if window is None:
                self.log_event("grid-error", error="; ".join(error.toString() for error in self.component.errors()))
                self.close_windows()
                return
            window.setScreen(screen)
            window.visibleChanged.connect(lambda visible, window=window: self.visibility_changed(window, visible))
            self.windows.append(window)
        self.navigation = navigation
        self.present(stage_changed=True)

    def present(self, stage_changed=False):
        view = self.navigation.view
        if view.stage == "monitors":
            self.screen = self.window = None
            if stage_changed:
                self.session._grid_stage_changed()
            for window in self.windows:
                window.setProperty("choosingMonitor", True)
                window.show()
            self.timer.start()
            self.log_event("grid-monitors", monitors=[
                {"number": number, **monitor.__dict__}
                for number, monitor in enumerate(view.monitors, 1)
            ])
            return
        # Bind the active window before intentionally hiding other overlays, so
        # visibility callbacks cannot interpret those hides as dismissal.
        self.screen, self.window = self.screens[view.monitor - 1], self.windows[view.monitor - 1]
        self.window.setProperty("choosingMonitor", False)
        for window in self.windows:
            if window is not self.window:
                window.hide()
        if stage_changed:
            self.session._grid_stage_changed()
        region = view.region
        self.window.setProperty("region", dict(x=region.x, y=region.y, width=region.width, height=region.height))
        self.window.setProperty("breadcrumb", " → ".join(map(str, view.path)) or "Full screen")
        self.window.setProperty("dryRun", not self.execute)
        self.timer.start()
        self.log_event("grid", monitor=view.monitor, screen=self.screen.name(),
                       path=list(view.path), region=region.__dict__, center=region.center, execute=self.execute)
        if stage_changed:
            self.window.show()

    def visibility_changed(self, window, visible):
        if not visible and (self.stage == "monitors" or (self.stage == "grid" and window is self.window)):
            self.cancel("overlay-hidden")

    def command(self, command, generation):
        self.session.command(command, generation)

    def _command(self, command):
        result = self.navigation.command(command)
        if result.state == "cancel":
            self.cancel("command")
        elif result.state == "click":
            self.session._commit_click(result.position)
        elif result.state in ("monitors", "grid"):
            self.present(stage_changed=result.stage_changed)
        else:
            if result.error:
                details = {"error": result.error}
                if result.state == "rejected":
                    details["command"] = command
                self.log_event("grid-" + result.state, **details)
            if result.error or self.stage == "monitors":
                self.timer.start()

    def _click(self, position):
        if self.execute:
            self.pending_click = position
            self.click_process.setArguments([str(value) for value in position])
            self.click_process.start()
        else:
            self.log_event("grid-would-click", position=position)

    def cancel(self, reason="cancelled"):
        self.session.cancel(reason)

    def _cancel_click(self, reason):
        if self.pending_click is not None:
            self.pending_click = None
            self.click_process.kill()
            self.log_event("grid-click-cancelled", reason=reason)

    def _close(self, reason):
        if not self.active:
            return
        self.navigation.close()
        self.timer.stop()
        self.close_windows()
        self.log_event("grid-closed", reason=reason)

    def close_windows(self):
        for window in self.windows:
            window.hide()
            window.deleteLater()
        self.windows = []
        self.screens = []
        self.screen = self.window = None

    def click_error(self, error):
        if error == QProcess.ProcessError.FailedToStart:
            self.log_event("action-failed", command="click", error=self.click_process.errorString())
            self.pending_click = None

    def click_finished(self, code, status):
        error = bytes(self.click_process.readAllStandardError()).decode("utf-8", errors="replace").strip()
        if self.pending_click is None:
            return
        self.log_event("executed" if code == 0 and status == QProcess.ExitStatus.NormalExit else "action-failed",
                   command="click", position=self.pending_click, error=error)
        self.pending_click = None

    def cleanup(self):
        self.cancel("shutdown")
        if self.click_process.state() != QProcess.ProcessState.NotRunning:
            self.click_process.kill()
            self.click_process.waitForFinished(1000)


class SyntheticController(QObject):
    """One plain command per stdin line, without a microphone or recognizer."""

    def __init__(self, app, grid):
        super().__init__(app)
        self.app = app
        self.grid = grid
        self.buffer = b""
        self.draining = False
        self.drain_timer = QTimer(self)
        self.drain_timer.setSingleShot(True)
        self.drain_timer.setInterval(4000)
        self.drain_timer.timeout.connect(self.quit)
        grid.click_process.finished.connect(self.click_done)
        grid.click_process.errorOccurred.connect(self.click_done)
        self.notifier = QSocketNotifier(0, QSocketNotifier.Type.Read, self)
        self.notifier.activated.connect(self.read)
        app.aboutToQuit.connect(grid.cleanup)
        grid.log_event("grid-stdin", commands=[*GRID_COMMANDS, "quit"])

    def read(self, *unused):
        data = os.read(0, 4096)
        if not data:
            # Finite command streams may end just after committing. Let that one action
            # complete, bounded by the helper's deadline and this shutdown timer.
            self.notifier.setEnabled(False)
            if self.grid.active:
                self.grid.cancel("input-closed")
            if self.grid.click_process.state() != QProcess.ProcessState.NotRunning:
                self.draining = True
                self.drain_timer.start()
            else:
                self.quit()
            return
        self.buffer += data
        while b"\n" in self.buffer:
            raw, self.buffer = self.buffer.split(b"\n", 1)
            command = raw.decode("utf-8", errors="replace").strip().lower()
            if command == "quit":
                self.quit()
                return
            if command in "123456789" and len(command) == 1:
                command = NUMBERS[int(command) - 1]
            if command in GRID_COMMANDS:
                self.grid.command(command, self.grid.generation)
            elif command:
                self.grid.log_event("grid-rejected", command=command)

    def click_done(self, *unused):
        if self.draining:
            self.quit()

    def quit(self):
        self.drain_timer.stop()
        self.notifier.setEnabled(False)
        self.app.quit()

    def cleanup(self):
        self.drain_timer.stop()
        self.notifier.setEnabled(False)
        self.grid.cleanup()
