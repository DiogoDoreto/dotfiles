"""Controller-owned, sequential asynchronous MPRIS execution and activity tracking."""

from collections import deque
from dataclasses import dataclass
import json

from PySide6.QtCore import QObject, QProcess, QTimer


@dataclass(eq=False)
class _Request:
    """One action's state; identity distinguishes it from cancelled requests."""

    command: str
    generation: int
    player: str | None = None
    process: QProcess | None = None
    timed_out: bool = False

    def details(self):
        details = {"command": self.command, "generation": self.generation}
        if self.player is not None:
            details["player"] = self.player
        return details


class MediaController(QObject):
    def __init__(self, parent, execute=False, player=None, *, playerctl="playerctl", playerctld="playerctld",
                 timeout_ms=2000):
        super().__init__(parent)
        self.execute = execute
        self.player = player
        self.playerctl = playerctl
        self.playerctld = playerctld
        self.timeout_ms = timeout_ms
        self.queue = deque()
        self.current = None
        self.processes = set()
        self.pending_calls = set()
        self.tracker = None
        self.live = False
        self.settled = False
        self.startup = QTimer(self)
        self.startup.setSingleShot(True)
        self.startup.setInterval(300)
        self.startup.timeout.connect(self._ready)

    def log_event(self, state, **details):
        print(json.dumps({"state": state, **details}), flush=True)

    def _process(self, program, arguments, done):
        process = QProcess(self)
        self.processes.add(process)
        process.setProgram(program)
        process.setArguments(arguments)
        completed = False

        def finish(code, status):
            nonlocal completed
            if completed:
                return
            completed = True
            self.processes.discard(process)
            done(process, code == 0 and status == QProcess.ExitStatus.NormalExit)
            process.deleteLater()

        process.finished.connect(finish)
        process.errorOccurred.connect(lambda error: finish(-1, QProcess.ExitStatus.CrashExit)
                                     if error == QProcess.ProcessError.FailedToStart else None)
        # kill() during QProcess.Starting may precede creation of the OS child.
        process.started.connect(lambda: process.kill() if process.property("cancelled") else None)
        process.start()
        return process

    def _kill(self, process):
        process.setProperty("cancelled", True)
        process.kill()

    def start(self):
        """Begin a ready listener's lifetime; preview never starts a helper."""
        if self.live:
            return
        self.live = True
        if self.execute:
            # A rapid new lifetime must wait for our old helper to release the
            # bus, or the new helper could mistake it for an external daemon.
            if self.tracker is None:
                self._start_tracker()
        else:
            self.settled = True

    def _start_tracker(self):
        # Foreground playerctld exits if an existing daemon already owns the bus.
        self.tracker = self._process(self.playerctld, [], self._tracker_finished)
        self.startup.start()

    def _tracker_finished(self, process, success):
        if self.tracker is process:
            self.tracker = None
            if self.live and process.property("cancelled"):
                self._start_tracker()

    def _ready(self):
        self.settled = True
        self._next()

    def command(self, command, generation):
        if not self.live:
            return
        if not self.execute:
            self.log_event("media-would-execute", command=command, generation=generation)
            return
        self.queue.append((command, generation))
        self._next()

    def _next(self):
        if (not self.live or not self.settled or self.current is not None
                or self.pending_calls or not self.queue):
            return
        command, generation = self.queue.popleft()
        # Each request has a unique token: late completion after cancellation can
        # neither dispatch an action nor consume a newer request.
        request = _Request(command, generation)
        self.current = request
        self._call(request, ["--list-all"], self._selected)

    def _call(self, request, arguments, continuation):
        timer = QTimer(self)
        timer.setSingleShot(True)

        def done(process, success):
            self.pending_calls.discard(process)
            timer.stop()
            timer.deleteLater()
            if self.current is not request:
                self._next()
                return
            error = bytes(process.readAllStandardError()).decode("utf-8", errors="replace").strip()
            if request.timed_out:
                success, error = False, "Media call timed out"
            if not success:
                self.log_event("action-failed", **request.details(), error=error or process.errorString())
                self._complete()
            else:
                continuation(request, bytes(process.readAllStandardOutput()).decode("utf-8", errors="replace"))

        process = self._process(self.playerctl, arguments, done)
        self.pending_calls.add(process)
        request.process = process

        def timeout():
            if self.current is request:
                request.timed_out = True
                self._kill(process)

        timer.timeout.connect(timeout)
        timer.start(self.timeout_ms)

    def _selected(self, request, output):
        players = [p for p in output.splitlines() if p and p != "playerctld"]
        if self.player:
            players = [p for p in players if p == self.player or p.startswith(self.player + ".")]
        if not players:
            self.log_event("no-player", **request.details(), preference=self.player)
            self._complete()
            return
        request.player = players[0]
        self._call(request, ["--player", request.player, request.command], self._executed)

    def _executed(self, request, output):
        self.log_event("executed", **request.details())
        self._complete()

    def _complete(self):
        self.current = None
        self._next()

    def cancel(self):
        """Cancel the old recognition generation without ending activity tracking."""
        self.queue.clear()
        request, self.current = self.current, None
        if request:
            self._kill(request.process)

    def stop(self):
        """Close delivery synchronously; reap children asynchronously."""
        self.live = self.settled = False
        self.startup.stop()
        self.cancel()
        if self.tracker:
            self._kill(self.tracker)

    def cleanup(self):
        # Only used after the event loop exits; runtime stop/cancel never waits.
        self.stop()
        for process in tuple(self.processes):
            self._kill(process)
            process.waitForFinished(1000)
