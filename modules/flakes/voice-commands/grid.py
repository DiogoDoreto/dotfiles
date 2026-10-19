"""Mouse-grid state and Qt presentation; recognition stays in the listener process."""

from dataclasses import dataclass
import json
import os
from pathlib import Path

from PySide6.QtCore import QObject, QProcess, QSocketNotifier, QTimer, QUrl, Slot
from PySide6.QtQml import QQmlApplicationEngine, QQmlComponent
from PySide6.QtQuick import QQuickWindow

from vocabulary import GRID_COMMANDS, NUMBERS
from session import CommandSession


# This overlay is plain 2D geometry/text. Choose its renderer before any Quick
# window exists so opening the grid never depends on desktop EGL/OpenGL support.
QQuickWindow.setSceneGraphBackend("software")


@dataclass(frozen=True)
class Region:
    x: float
    y: float
    width: float
    height: float

    def cell(self, number):
        if number not in range(1, 10):
            raise ValueError("Cell must be between 1 and 9")
        row, column = divmod(number - 1, 3)
        return Region(self.x + column * self.width / 3, self.y + row * self.height / 3,
                      self.width / 3, self.height / 3)

    @property
    def center(self):
        return (self.x + self.width / 2, self.y + self.height / 2)


class GridController(QObject):
    def __init__(self, app, pointer, execute=False, timeout=20, session=None):
        super().__init__(app)
        self.app = app
        self.pointer = pointer
        self.execute = execute
        self.session = session if session is not None else CommandSession(self)
        self.session.grid = self
        self.stage = None
        self.screens = []
        self.windows = []
        self.history = []
        self.path = []
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
        screens = sorted(self.app.screens(), key=lambda screen: (
            screen.geometry().x(), screen.geometry().y(), screen.name()))
        if self.app.platformName() != "wayland" or not 1 <= len(screens) <= len(NUMBERS):
            self.log_event("grid-error", error="Mouse grid requires Plasma Wayland with one to nine enabled displays")
            return
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
        if len(screens) > 1:
            self.show_monitors()
        else:
            self.select_monitor(0)

    def show_monitors(self):
        self.stage = "monitors"
        self.screen = self.window = None
        self.history = []
        self.path = []
        self.session._grid_stage_changed()
        for window in self.windows:
            window.setProperty("choosingMonitor", True)
            window.show()
        self.timer.start()
        self.log_event("grid-monitors", monitors=[
            {"number": number, "name": screen.name(), "x": screen.geometry().x(), "y": screen.geometry().y(),
             "width": screen.geometry().width(), "height": screen.geometry().height()}
            for number, screen in enumerate(self.screens, 1)
        ])

    def select_monitor(self, index):
        self.stage = "grid"
        self.screen, self.window = self.screens[index], self.windows[index]
        self.window.setProperty("choosingMonitor", False)
        for window in self.windows:
            if window is not self.window:
                window.hide()
        geometry = self.screen.geometry()
        self.history = [Region(0, 0, geometry.width(), geometry.height())]
        self.path = []
        self.session._grid_stage_changed()
        self.update()
        self.window.show()

    def visibility_changed(self, window, visible):
        if not visible and (self.stage == "monitors" or (self.stage == "grid" and window is self.window)):
            self.cancel("overlay-hidden")

    def update(self):
        region = self.history[-1]
        self.window.setProperty("region", dict(x=region.x, y=region.y, width=region.width, height=region.height))
        self.window.setProperty("breadcrumb", " → ".join(map(str, self.path)) or "Full screen")
        self.window.setProperty("dryRun", not self.execute)
        self.timer.start()
        self.log_event("grid", monitor=self.window.property("monitorNumber"), screen=self.screen.name(),
                       path=list(self.path), region=region.__dict__, center=region.center, execute=self.execute)

    def command(self, command, generation):
        self.session.command(command, generation)

    def _command(self, command):
        if self.stage == "monitors":
            if command in NUMBERS:
                index = NUMBERS.index(command)
                if index < len(self.screens):
                    self.select_monitor(index)
                else:
                    self.log_event("grid-rejected", command=command, error="No monitor with that number")
            elif command == "cancel":
                self.cancel("command")
            elif command == "click":
                self.log_event("grid-select-monitor", error="Say a monitor number before clicking")
            if self.active:
                self.timer.start()
            return
        if command in NUMBERS:
            region = self.history[-1]
            if min(region.width, region.height) / 3 < 1:
                self.log_event("grid-limit", error="Already at logical-pixel precision; use click or back")
                self.timer.start()
                return
            number = NUMBERS.index(command) + 1
            self.history.append(region.cell(number))
            self.path.append(number)
        elif command == "back":
            if self.path:
                self.path.pop()
                self.history.pop()
            elif len(self.screens) > 1:
                self.show_monitors()
                return
        elif command == "reset":
            self.history = self.history[:1]
            self.path = []
        elif command == "cancel":
            self.cancel("command")
            return
        elif command == "click":
            center = self.history[-1].center
            geometry = self.screen.geometry()
            position = (center[0] + geometry.x(), center[1] + geometry.y())
            self.session._commit_click(position)
            return
        else:
            return
        self.update()

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
        self.stage = None
        self.history = []
        self.path = []
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
