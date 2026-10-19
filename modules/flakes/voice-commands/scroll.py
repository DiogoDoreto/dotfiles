"""Repeated desktop scrolling at the current pointer position."""

from collections import deque
import json

from PySide6.QtCore import QObject, QProcess, QTimer

from vocabulary import SCROLL_DIRECTIONS


class ScrollController(QObject):
    def __init__(self, parent, pointer, session, execute=False):
        super().__init__(parent)
        self.session = session
        self.session.scroll = self
        self.execute = execute
        self.queue = deque()
        self.current = None
        self.process = QProcess(self)
        self.process.setProgram(pointer)
        self.process.finished.connect(self.finished)
        self.process.errorOccurred.connect(self.process_error)
        self.timer = QTimer(self)
        self.timer.setSingleShot(True)
        self.timer.setInterval(60_000)
        self.timer.timeout.connect(lambda: self.session.cancel("timeout"))

    @property
    def active(self):
        return self.session.mode == "scroll"

    def log_event(self, state, **details):
        print(json.dumps({"state": state, "generation": self.session.generation, **details}), flush=True)

    def _open(self):
        self.log_event("scroll-opened", timeout_s=60, execute=self.execute)
        self.timer.start()

    def _command(self, command):
        if command == "cancel":
            self.session.cancel("command")
        elif command == "scroll":
            self.timer.start()
        elif command in SCROLL_DIRECTIONS:
            self.timer.start()
            if self.execute:
                self.queue.append(command)
                self.start_next()
            else:
                self.log_event("scroll-would-scroll", direction=command)

    def start_next(self):
        if self.active and self.queue and self.process.state() == QProcess.ProcessState.NotRunning:
            self.current = self.queue.popleft()
            self.process.setArguments(["--scroll", self.current])
            self.process.start()

    def finished(self, code, status):
        error = bytes(self.process.readAllStandardError()).decode("utf-8", errors="replace").strip()
        if self.current is not None:
            self.log_event("executed" if code == 0 and status == QProcess.ExitStatus.NormalExit else "action-failed",
                           command="scroll", direction=self.current, error=error)
            self.current = None
        self.start_next()

    def process_error(self, error):
        if error == QProcess.ProcessError.FailedToStart:
            self.log_event("action-failed", command="scroll", direction=self.current, error=self.process.errorString())
            self.current = None
            self.queue.clear()

    def _close(self, reason):
        self.timer.stop()
        self.queue.clear()
        self.current = None
        if self.process.state() != QProcess.ProcessState.NotRunning:
            # Reap synchronously so a stale finished signal cannot consume a later action.
            self.process.kill()
            self.process.waitForFinished(1000)
        if self.active:
            self.log_event("scroll-closed", reason=reason)

    def cleanup(self):
        self.session.cancel("shutdown")
        if self.process.state() != QProcess.ProcessState.NotRunning:
            self.process.waitForFinished(1000)
