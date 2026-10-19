"""Mouse-grid state and Qt presentation; recognition stays in the listener process."""

from dataclasses import dataclass
import json
import os
from pathlib import Path

from PySide6.QtCore import QObject, QProcess, QSocketNotifier, QTimer, QUrl, Signal
from PySide6.QtQml import QQmlApplicationEngine

from vocabulary import GRID_COMMANDS, NUMBERS


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
    mode_changed = Signal(str, int)

    def __init__(self, app, pointer, execute=False, timeout=20):
        super().__init__(app)
        self.app = app
        self.pointer = pointer
        self.execute = execute
        self.generation = 0
        self.history = []
        self.path = []
        self.screen = None
        self.engine = None
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
        app.screenAdded.connect(lambda unused: self.cancel("display-added"))
        app.screenRemoved.connect(lambda unused: self.cancel("display-removed"))

    @property
    def active(self):
        return bool(self.history)

    def log_event(self, state, **details):
        print(json.dumps({"state": state, "generation": self.generation, **details}), flush=True)

    def open(self):
        if self.click_process.state() != QProcess.ProcessState.NotRunning:
            self.log_event("grid-error", error="Previous click is still pending")
            return
        screens = self.app.screens()
        if self.app.platformName() != "wayland" or len(screens) != 1:
            self.log_event("grid-error", error="Mouse grid requires Plasma Wayland with exactly one enabled display")
            return
        screen = screens[0]
        if self.screen is not screen:
            self.screen = screen
            screen.geometryChanged.connect(lambda unused: self.cancel("display-changed"))
            screen.logicalDotsPerInchChanged.connect(lambda unused: self.cancel("scale-changed"))
        if self.engine is None:
            self.engine = QQmlApplicationEngine(self)
            self.engine.rootContext().setContextProperty("targetScreen", screen)
            self.engine.load(QUrl.fromLocalFile(str(Path(__file__).with_name("grid.qml"))))
            if not self.engine.rootObjects():
                self.log_event("grid-error", error="Could not load mouse-grid overlay")
                self.engine.deleteLater()
                self.engine = None
                return
            self.window = self.engine.rootObjects()[0]
            self.window.visibleChanged.connect(self.visibility_changed)
        self.engine.rootContext().setContextProperty("targetScreen", screen)
        self.window.setScreen(screen)
        geometry = screen.geometry()
        self.history = [Region(0, 0, geometry.width(), geometry.height())]
        self.path = []
        self.generation += 1
        self.update()
        self.window.show()
        self.mode_changed.emit("grid", self.generation)

    def visibility_changed(self, visible):
        if not visible and self.active:
            self.cancel("overlay-hidden")

    def update(self):
        region = self.history[-1]
        self.window.setProperty("region", dict(x=region.x, y=region.y, width=region.width, height=region.height))
        self.window.setProperty("breadcrumb", " → ".join(map(str, self.path)) or "Full screen")
        self.window.setProperty("dryRun", not self.execute)
        self.timer.start()
        self.log_event("grid", path=list(self.path), region=region.__dict__, center=region.center, execute=self.execute)

    def command(self, command, generation):
        # Ignore buffered recognition from an earlier overlay/session, including after timeout.
        if generation != self.generation:
            return
        if not self.active:
            if command == "click":
                self.open()
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
            self.cancel("click", cancel_click=False)
            if self.execute:
                self.pending_click = position
                self.click_process.setArguments([str(value) for value in position])
                self.click_process.start()
            else:
                self.log_event("grid-would-click", position=position)
            return
        else:
            return
        self.update()

    def cancel(self, reason="cancelled", cancel_click=True):
        if cancel_click and self.pending_click is not None:
            self.pending_click = None
            self.click_process.kill()
            self.log_event("grid-click-cancelled", reason=reason)
        if not self.active:
            return
        self.history = []
        self.path = []
        self.timer.stop()
        self.window.hide()
        self.generation += 1
        self.log_event("grid-closed", reason=reason)
        self.mode_changed.emit("normal", self.generation)

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
