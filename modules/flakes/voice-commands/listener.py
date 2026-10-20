"""Widget-free listener lifetime and newline-JSON transport over a real QProcess.

The program/arguments seam is shared by the packaged recognizer and deterministic
test children. Recognition-generation policy belongs to CommandSession, not here.
"""

import json
import os
import signal
import time

from PySide6.QtCore import QObject, QProcess, QTimer, Signal


class ListenerLifetime(QObject):
    state_changed = Signal(str)
    ready = Signal()
    command = Signal(object, object)
    typing_activity = Signal(object, object, object)
    shutdown_complete = Signal()

    def __init__(self, program, arguments, parent=None, *, stop_timeout_ms=5000):
        super().__init__(parent)
        self.state = "Paused"
        self.last_error = ""
        self.closing = False
        self._shutdown_notified = False
        self._output = b""
        self._group_id = 0
        self._ending = False
        self._escalated = False
        self._stop_deadline = 0
        self._process = QProcess(self)
        self._process.setProgram(program)
        self._process.setArguments(arguments)
        # The listener and inherited helpers must never share the Qt process group.
        self._process.setUnixProcessParameters(QProcess.UnixProcessFlag.CreateNewSession)
        self._process.setProcessChannelMode(QProcess.ProcessChannelMode.ForwardedErrorChannel)
        self._process.started.connect(self._started)
        self._process.readyReadStandardOutput.connect(self._read_output)
        self._process.finished.connect(self._finished)
        self._process.errorOccurred.connect(self._process_error)
        self._stop_timer = QTimer(self)
        self._stop_timer.setSingleShot(True)
        self._stop_timer.setInterval(stop_timeout_ms)
        self._stop_timer.timeout.connect(self._kill_group)

    def _set_state(self, state):
        self.state = state
        self.state_changed.emit(state)

    def start(self):
        if self.closing or self.state not in ("Paused", "Error"):
            return
        self.last_error = ""
        self._output = b""
        self._escalated = False
        self._set_state("Starting")
        # State observers may cancel before QProcess.start(); no child is needed.
        if self.state != "Starting":
            self._stop_timer.stop()
            self._set_state("Paused")
            self._notify_shutdown()
            return
        self._process.start()

    def _started(self):
        self._group_id = self._process.processId()
        if self.state == "Stopping":
            if self._escalated:
                self._kill_group()
            else:
                self._process.terminate()

    def stop(self):
        if self.state not in ("Starting", "Listening"):
            return
        self._stop_deadline = time.monotonic() + self._stop_timer.interval() / 1000
        # Intent closes command/control delivery synchronously, before any signal.
        self._set_state("Stopping")
        if self.state != "Stopping":
            return
        if self._process.state() == QProcess.ProcessState.Running:
            self._process.terminate()
        self._stop_timer.start()

    def send_control(self, mode, generation):
        """Deliver a session snapshot only to a ready listener; never retain policy.

        Connect ready to CommandSession.synchronize and changed to this method.
        The synchronous ready notification precedes delivery of subsequent commands.
        """
        if (self.state == "Listening" and not self._ending and not self.closing
                and self._process.state() == QProcess.ProcessState.Running):
            self._process.write((json.dumps({"mode": mode, "generation": generation}) + "\n").encode())

    def _read_output(self):
        # Frame bytes before decoding: even a UTF-8 codepoint may span pipe reads.
        self._output += bytes(self._process.readAllStandardOutput())
        while b"\n" in self._output:
            line, self._output = self._output.split(b"\n", 1)
            line = line.decode("utf-8", errors="replace")
            print(line, flush=True)
            try:
                event = json.loads(line)
            except json.JSONDecodeError:
                continue
            if not isinstance(event, dict):
                continue
            if event.get("state") == "error":
                self.last_error = event.get("error") or "Listener error"
            if (self._ending or self.closing or self.state not in ("Starting", "Listening")
                    or self._process.state() != QProcess.ProcessState.Running):
                continue
            if event.get("state") == "listening" and self.state == "Starting":
                self._set_state("Listening")
                if self.state == "Listening" and not self.closing:
                    self.ready.emit()
            elif event.get("state") == "command" and self.state == "Listening":
                self.command.emit(event.get("command"), event.get("generation"))
            elif event.get("state") == "typing-activity" and self.state == "Listening":
                self.typing_activity.emit(event.get("speaking"), event.get("generation"), event.get("monotonic_s"))

    def _kill_group(self):
        self._escalated = True
        if self._group_id:
            try:
                os.killpg(self._group_id, signal.SIGKILL)
            except ProcessLookupError:
                pass
        # Also cover escalation before started() has supplied the group ID.
        if self._process.state() != QProcess.ProcessState.NotRunning:
            self._process.kill()

    def _process_error(self, error):
        if error == QProcess.ProcessError.FailedToStart:
            self._stop_timer.stop()
            self.last_error = self._process.errorString()
            self._set_state("Error")
            self._notify_shutdown()

    def _finished(self, code, status):
        requested_stop = self.state == "Stopping"
        # Drain diagnostics, but never dispatch a command from an exited listener.
        self._ending = True
        self._read_output()
        if self._output:
            print(self._output.decode("utf-8", errors="replace"), flush=True)
            self._output = b""
        self._stop_timer.stop()
        # Even a normal/crashed leader may have left capture/tracker descendants.
        self._kill_group()
        self._group_id = 0
        self._ending = False
        if requested_stop or (code == 0 and status == QProcess.ExitStatus.NormalExit):
            state = "Paused"
        else:
            self.last_error = self.last_error or f"Listener exited with code {code}; see terminal output"
            state = "Error"
        self._set_state(state)
        self._notify_shutdown()

    def _notify_shutdown(self):
        if self.closing and self.state in ("Paused", "Error") and not self._shutdown_notified:
            self._shutdown_notified = True
            self.shutdown_complete.emit()

    def shutdown(self):
        """Permanently close this lifetime; notify when async stop has completed."""
        if self.closing:
            return
        self.closing = True
        self.stop()
        self._notify_shutdown()

    def cleanup(self):
        """Final synchronous fallback when Qt is leaving its event loop.

        At most the remaining five-second grace plus two seconds to reap, fitting
        the host's ten-second TimeoutStopSec. waitForFinished does not run QTimers.
        """
        self.shutdown()
        if self._process.state() != QProcess.ProcessState.NotRunning:
            remaining = max(0, int((self._stop_deadline - time.monotonic()) * 1000))
            if not self._process.waitForFinished(remaining):
                self._kill_group()
                self._process.waitForFinished(2000)
        self._kill_group()
        self._group_id = 0
