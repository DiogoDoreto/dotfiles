"""THROWAWAY: Plasma-compatible tray control for the selected Vosk listener."""

import argparse
import json
import os
import signal
import sys

from PySide6.QtCore import QObject, QProcess, QSocketNotifier, Qt, QTimer
from PySide6.QtGui import QColor, QIcon, QPainter, QPen, QPixmap
from PySide6.QtWidgets import QApplication, QMenu, QSystemTrayIcon


def microphone_icon(color):
    image = QPixmap(48, 48)
    image.fill(Qt.GlobalColor.transparent)
    painter = QPainter(image)
    painter.setRenderHint(QPainter.RenderHint.Antialiasing)
    painter.setPen(Qt.PenStyle.NoPen)
    painter.setBrush(QColor(color))
    painter.drawEllipse(2, 2, 44, 44)
    painter.setBrush(QColor("white"))
    painter.drawRoundedRect(19, 10, 10, 20, 5, 5)
    painter.setPen(QPen(QColor("white"), 3))
    painter.setBrush(Qt.BrushStyle.NoBrush)
    painter.drawArc(14, 17, 20, 19, 180 * 16, 180 * 16)
    painter.drawLine(24, 36, 24, 40)
    painter.drawLine(18, 40, 30, 40)
    painter.end()
    return QIcon(image)


class TrayController(QObject):
    def __init__(self, app, listener, arguments):
        super().__init__(app)
        self.app = app
        self.state = "Paused"
        self.last_error = ""
        self.output = ""
        self.quitting = False
        self.requested_stop = False
        self.group_id = 0
        self.process = QProcess(self)
        self.process.setProgram(listener)
        self.process.setArguments(arguments)
        # Own one process group so even a crashed listener cannot leave its mic helper behind.
        self.process.setUnixProcessParameters(QProcess.UnixProcessFlag.CreateNewSession)
        self.process.setProcessChannelMode(QProcess.ProcessChannelMode.ForwardedErrorChannel)
        self.process.started.connect(self.started)
        self.process.readyReadStandardOutput.connect(self.read_output)
        self.process.finished.connect(self.finished)
        self.process.errorOccurred.connect(self.process_error)

        self.stop_timer = QTimer(self)
        self.stop_timer.setSingleShot(True)
        self.stop_timer.setInterval(5000)
        self.stop_timer.timeout.connect(self.kill_group)

        self.menu = QMenu()
        self.status_action = self.menu.addAction("Voice Commands — Paused")
        self.status_action.setEnabled(False)
        self.toggle_action = self.menu.addAction("Resume listening", self.toggle)
        self.menu.addSeparator()
        self.menu.addAction("Quit", self.quit)
        self.tray = QSystemTrayIcon(self)
        self.tray.setContextMenu(self.menu)
        self.tray.activated.connect(self.activated)
        self.set_state("Paused")
        self.tray.show()
        self.app.aboutToQuit.connect(self.cleanup)

    def set_state(self, state):
        self.state = state
        colors = {"Listening": "#239b56", "Paused": "#7f8c8d", "Starting": "#d4ac0d",
                  "Stopping": "#d4ac0d", "Error": "#c0392b"}
        self.tray.setIcon(microphone_icon(colors[state]))
        detail = f"\n{self.last_error}" if state == "Error" else ""
        self.tray.setToolTip(f"Voice Commands — {state}{detail}\nClick to pause/resume listening")
        self.status_action.setText(f"Voice Commands — {state}")
        self.toggle_action.setText("Pause listening" if state in ("Listening", "Starting", "Stopping") else "Resume listening")
        self.toggle_action.setEnabled(state != "Stopping" and not self.quitting)
        print(json.dumps({"tray": state, "error": self.last_error if state == "Error" else ""}), flush=True)

    def activated(self, reason):
        if reason == QSystemTrayIcon.ActivationReason.Trigger:
            self.toggle()

    def toggle(self):
        if self.quitting or self.state == "Stopping":
            return
        if self.process.state() == QProcess.ProcessState.NotRunning:
            self.start()
        else:
            self.stop()

    def start(self):
        if self.quitting or self.process.state() != QProcess.ProcessState.NotRunning:
            return
        self.requested_stop = False
        self.last_error = self.output = ""
        self.set_state("Starting")
        self.process.start()

    def started(self):
        self.group_id = self.process.processId()
        if self.requested_stop:
            self.process.terminate()

    def stop(self):
        if self.process.state() == QProcess.ProcessState.NotRunning:
            return
        self.requested_stop = True
        self.set_state("Stopping")
        self.process.terminate()
        self.stop_timer.start()

    def kill_group(self):
        if self.group_id:
            try:
                os.killpg(self.group_id, signal.SIGKILL)
            except ProcessLookupError:
                pass

    def read_output(self):
        self.output += bytes(self.process.readAllStandardOutput()).decode("utf-8", errors="replace")
        while "\n" in self.output:
            line, self.output = self.output.split("\n", 1)
            print(line, flush=True)
            try:
                event = json.loads(line)
            except json.JSONDecodeError:
                continue
            if event.get("state") == "listening" and not self.requested_stop:
                self.set_state("Listening")
            elif event.get("state") == "error":
                self.last_error = event.get("error", "Listener error")

    def process_error(self, error):
        if error == QProcess.ProcessError.FailedToStart:
            self.last_error = self.process.errorString()
            self.set_state("Error")
            if self.quitting:
                self.app.quit()

    def finished(self, code, status):
        self.read_output()
        self.stop_timer.stop()
        self.kill_group()
        self.group_id = 0
        if self.requested_stop or (code == 0 and status == QProcess.ExitStatus.NormalExit):
            self.set_state("Paused")
        else:
            self.last_error = self.last_error or f"Listener exited with code {code}; see terminal output"
            self.set_state("Error")
        if self.quitting:
            self.app.quit()

    def quit(self):
        self.quitting = True
        if self.process.state() == QProcess.ProcessState.NotRunning:
            self.app.quit()
        else:
            self.stop()

    def cleanup(self):
        if self.process.state() != QProcess.ProcessState.NotRunning:
            self.stop()
            if not self.process.waitForFinished(6000):
                self.kill_group()
                self.process.waitForFinished(2000)
        self.tray.hide()


def main():
    parser = argparse.ArgumentParser(description=__doc__, allow_abbrev=False,
                                     epilog="Other arguments are forwarded to the Vosk runner, e.g. --execute --wake 'hey computer'.")
    parser.add_argument("--listener", required=True, help=argparse.SUPPRESS)
    parser.add_argument("--start-paused", action="store_true", help="Show the tray without opening the microphone")
    args, listener_arguments = parser.parse_known_args()
    app = QApplication([])
    app.setApplicationName("lapdog-voice-commands")
    app.setQuitOnLastWindowClosed(False)
    if not QSystemTrayIcon.isSystemTrayAvailable():
        print("No system tray is available. Run inside your Plasma desktop session.", file=sys.stderr)
        return 1
    controller = TrayController(app, args.listener, listener_arguments)

    # Deliver Unix shutdown signals through Qt's event loop without an idle polling timer.
    read_fd, write_fd = os.pipe2(os.O_NONBLOCK | os.O_CLOEXEC)
    previous_fd = signal.set_wakeup_fd(write_fd)
    for sig in (signal.SIGINT, signal.SIGTERM, signal.SIGHUP):
        signal.signal(sig, lambda signum, frame: None)
    notifier = QSocketNotifier(read_fd, QSocketNotifier.Type.Read)

    def interrupted(*unused):
        os.read(read_fd, 4096)
        controller.quit()

    notifier.activated.connect(interrupted)
    if not args.start_paused:
        controller.start()
    try:
        return app.exec()
    finally:
        notifier.setEnabled(False)
        signal.set_wakeup_fd(previous_fd)
        os.close(read_fd)
        os.close(write_fd)
        controller.cleanup()


if __name__ == "__main__":
    raise SystemExit(main())
