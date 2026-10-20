"""One owned pair of asynchronous Handy toggle dispatches, never retried.

The external interface is enter/finish/activity plus settled/changed. Successful
CLI exit is only dispatch, not Handy acceptance. Each explicit invocation owns
one recording; callers must wait for Handy completion before invoking again.
"""

import json
import math
import time

from PySide6.QtCore import QObject, QProcess, QTimer, Qt, Signal


class TypingController(QObject):
    changed = Signal()
    settled = Signal()

    def __init__(self, parent, session, handy, execute=False, *, first_ms=10000, silence_ms=3000,
                 client_ms=2000):
        super().__init__(parent)
        self.session = session
        session.typing = self
        self.handy = handy
        self.execute = execute
        self.first_ms, self.silence_ms = first_ms, silence_ms
        self.state = "idle"
        self.finish_requested = False
        self.owner = None
        self.deadline = 0
        self.armed_at = 0
        self.process = QProcess(self)
        self.process.setProgram(handy)
        self.process.setArguments(["--toggle-transcription"])
        self.process.finished.connect(self._finished)
        self.process.errorOccurred.connect(self._error)
        self.timer = QTimer(self)
        self.timer.setSingleShot(True)
        self.timer.setTimerType(Qt.TimerType.PreciseTimer)
        self.timer.timeout.connect(self._timeout)
        self.client_timer = QTimer(self)
        self.client_timer.setSingleShot(True)
        self.client_timer.setInterval(client_ms)
        self.client_timer.timeout.connect(self._client_timeout)

    @property
    def busy(self):
        return self.state in ("starting", "waiting", "speech", "stopping")

    def log_event(self, state, **details):
        print(json.dumps({"state": state, **details}), flush=True)

    def _state(self, state):
        self.state = state
        self.changed.emit()

    def enter(self):
        if self.busy:
            self.log_event("typing-unavailable", reason="current interaction or dispatch still settling")
            return
        self.finish_requested = False
        self.session._close_groups("mode-changed")
        self.session._enter("typing")
        self.owner = self.session.generation
        self._state("starting")
        self.log_event("typing-dispatch" if self.execute else "typing-would-type")
        if self.execute:
            self._dispatch()
        else:
            self._started_dispatch()

    def _dispatch(self):
        self.process.start()
        self.client_timer.start()

    def _started_dispatch(self):
        if self.finish_requested:
            self._stop_dispatch()
            return
        # Reset capture again after the client exits: queued entry PCM/activity
        # cannot become dictation simply because dispatch took time.
        self.session._enter("typing")
        self.owner = self.session.generation
        self.armed_at = time.monotonic()
        self._state("waiting")
        self._deadline(self.armed_at + self.first_ms / 1000)

    def _deadline(self, deadline):
        self.deadline = deadline
        self.timer.start(max(1, math.ceil((deadline - time.monotonic()) * 1000)))

    def activity(self, speaking, generation, captured_at):
        if (self.state not in ("waiting", "speech") or type(speaking) is not bool
                or type(generation) is not int or generation != self.owner
                or self.session.mode != "typing" or self.session.generation != self.owner
                or type(captured_at) not in (int, float) or not math.isfinite(captured_at)
                or captured_at < self.armed_at or captured_at > time.monotonic() + .1):
            return
        if time.monotonic() >= self.deadline:
            self._timeout()
        elif speaking:
            self._state("speech")
            self._deadline(captured_at + self.silence_ms / 1000)

    def _timeout(self):
        if self.state not in ("waiting", "speech"):
            return
        if time.monotonic() < self.deadline:
            self._deadline(self.deadline)
            return
        self.session.cancel("typing-initial-timeout" if self.state == "waiting" else "typing-silence")

    def finish(self, reason):
        self.timer.stop()
        if not self.busy or self.finish_requested or self.state == "stopping":
            return
        self.finish_requested = True
        self.log_event("typing-finish", reason=reason)
        if self.state != "starting":
            self._stop_dispatch()

    def _stop_dispatch(self):
        self._state("stopping")
        self.log_event("typing-stop-dispatch" if self.execute else "typing-would-stop")
        if self.execute:
            self._dispatch()
        else:
            self._settle()

    def _finished(self, code, status):
        self.client_timer.stop()
        error = bytes(self.process.readAllStandardError()).decode("utf-8", errors="replace").strip()
        if code != 0 or status != QProcess.ExitStatus.NormalExit:
            self._failed(error or f"client exited {code}; Handy state unknown")
        elif self.state == "starting":
            self.log_event("typing-client-dispatched", phase="start", acknowledged=False)
            self._started_dispatch()
        elif self.state == "stopping":
            self.log_event("typing-client-dispatched", phase="stop", acknowledged=False)
            self._settle()

    def _error(self, error):
        if error == QProcess.ProcessError.FailedToStart:
            self.client_timer.stop()
            self._failed(self.process.errorString())

    def _client_timeout(self):
        self.log_event("typing-error", error="Handy client timeout; state unknown, no retry", phase=self.state)
        self.process.kill()  # finished reports failure; never infer parity or retry.

    def _failed(self, error):
        self.log_event("typing-error", error=error, phase=self.state, handy_state="unknown", retry=False)
        self._settle()

    def _settle(self):
        self.timer.stop()
        self._state("idle")
        if (not self.finish_requested and self.session.mode == "typing"
                and self.session.generation == self.owner):
            self.session._enter("normal")
        self.settled.emit()
