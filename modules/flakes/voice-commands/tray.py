"""System tray control for the local Vosk voice command runner."""

import argparse
import json
import os
import signal
import sys

from PySide6.QtCore import QObject, QSocketNotifier, Qt
from PySide6.QtGui import QColor, QIcon, QPainter, QPen, QPixmap
from PySide6.QtWidgets import QApplication, QMenu, QSystemTrayIcon

from listener import ListenerLifetime
from features import Features
from vocabulary import normalize


def display_state(state, scrolling):
    return "Scrolling" if state == "Listening" and scrolling else state


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
    def __init__(self, app, listener, arguments, desktop=None):
        super().__init__(app)
        self.app = app
        self.desktop = desktop
        self.listener = ListenerLifetime(listener, arguments, self)
        self.listener.state_changed.connect(self.listener_state_changed)
        self.listener.shutdown_complete.connect(self._maybe_quit)
        if desktop:
            self.listener.ready.connect(desktop.session.synchronize)
            self.listener.command.connect(desktop.command)
            desktop.session.changed.connect(self.recognition_changed)
            if desktop.typing:
                self.listener.typing_activity.connect(desktop.typing.activity)
                desktop.typing.changed.connect(lambda: self.set_state(self.listener.state))
                desktop.typing.settled.connect(self._maybe_quit)

        self.menu = QMenu()
        self.status_action = self.menu.addAction("Voice Commands — Paused")
        self.status_action.setEnabled(False)
        self.toggle_action = self.menu.addAction("Resume listening", self.toggle)
        if desktop:
            self.menu.addAction("Help", desktop.show_help)
        self.menu.addSeparator()
        self.menu.addAction("Quit", self.quit)
        self.tray = QSystemTrayIcon(self)
        self.tray.setContextMenu(self.menu)
        self.tray.activated.connect(self.activated)
        self.set_state("Paused")
        self.tray.show()
        self.app.aboutToQuit.connect(self.cleanup)

    def set_state(self, state):
        colors = {"Listening": "#239b56", "Paused": "#7f8c8d", "Starting": "#d4ac0d",
                  "Stopping": "#d4ac0d", "Error": "#c0392b", "Scrolling": "#2980b9"}
        label = display_state(state, self.desktop and self.desktop.scroll and self.desktop.scroll.active)
        typing = self.desktop.typing if self.desktop else None
        if state == "Listening" and typing and typing.busy:
            label = {"starting": "Typing — dispatching", "waiting": "Typing — waiting for speech (10s)",
                     "speech": "Typing — speech (3s quiet finishes)", "stopping": "Typing — finishing"}[typing.state]
            colors[label] = "#8e44ad"
        self.tray.setIcon(microphone_icon(colors[label]))
        detail = f"\n{self.listener.last_error}" if state == "Error" else ""
        if label == "Scrolling":
            detail = "\nSay up, down, or cancel · exits after one inactive minute"
        if typing and typing.busy:
            detail = "\nHandy must be running and idle; Help/pause finishes recording"
        self.tray.setToolTip(f"Voice Commands — {label}{detail}\nClick to pause/resume listening")
        self.status_action.setText(f"Voice Commands — {label}")
        self.toggle_action.setText("Pause listening" if state in ("Listening", "Starting", "Stopping") else "Resume listening")
        self.toggle_action.setEnabled(state != "Stopping" and not self.listener.closing)
        print(json.dumps({"tray": label, "error": self.listener.last_error if state == "Error" else ""}), flush=True)

    def listener_state_changed(self, state):
        if self.desktop and state in ("Stopping", "Paused", "Error"):
            self.desktop.cancel("listening-stopped" if state == "Stopping" else "listener-exited")
        self.set_state(state)

    def activated(self, reason):
        if reason == QSystemTrayIcon.ActivationReason.Trigger:
            self.toggle()

    def toggle(self):
        if self.listener.closing or self.listener.state == "Stopping":
            return
        if self.listener.state in ("Paused", "Error"):
            self.start()
        else:
            self.stop()

    def start(self):
        self.listener.start()

    def stop(self):
        self.listener.stop()

    def recognition_changed(self, mode, generation):
        self.listener.send_control(mode, generation)
        if self.listener.state == "Listening":
            self.set_state("Listening")

    def quit(self):
        if self.desktop:
            self.desktop.cancel("quit")
        self.listener.shutdown()

    def _maybe_quit(self):
        if (self.listener.closing and self.listener.state in ("Paused", "Error")
                and not (self.desktop and self.desktop.typing and self.desktop.typing.busy)):
            self.app.quit()

    def cleanup(self):
        self.listener.shutdown()
        if self.desktop:
            self.desktop.cleanup()
        self.listener.cleanup()
        self.tray.hide()


def main():
    parser = argparse.ArgumentParser(description=__doc__, allow_abbrev=False,
                                     epilog="Other arguments are forwarded to the Vosk runner, e.g. --execute --wake 'hey computer'.")
    parser.add_argument("--listener", required=True, help=argparse.SUPPRESS)
    parser.add_argument("--pointer", required=True, help=argparse.SUPPRESS)
    parser.add_argument("--start-paused", action="store_true", help="Show the tray without opening the microphone")
    parser.add_argument("--execute", action=argparse.BooleanOptionalAction, default=False, help="Invoke actions instead of previewing")
    Features.add_arguments(parser)
    parser.add_argument("--player", help="Prefer this MPRIS player family/instance, e.g. mpv")
    parser.add_argument("--handy", default="handy", help="Handy executable; must already be running and idle")
    parser.add_argument("--wake", default="hey computer", help="English wake phrase")
    parser.add_argument("--grid-stdin", action="store_true", help="Read synthetic grid commands from stdin instead of listening (no tray)")
    parser.add_argument("--grid-timeout", type=float, default=20, help="Cancel targeting after this many inactive seconds (default: 20)")
    args, listener_arguments = parser.parse_known_args()
    features = Features.from_args(args)
    wake = normalize(args.wake)
    if args.wake.strip() and not wake:
        parser.error("Nonempty wake phrase must contain English words")
    if not 0.1 <= args.grid_timeout <= 3600:
        parser.error("--grid-timeout must be between 0.1 and 3600 seconds")
    if args.grid_stdin and args.start_paused:
        parser.error("--grid-stdin cannot be combined with --start-paused")
    if features.mouse_grid or args.grid_stdin:
        # Load the packaged QML/SSL dependency chain before QApplication can load
        # desktop theme plugins linked against an older libcrypto with the same SONAME.
        from grid import GridController, SyntheticController
    app = QApplication([])
    app.setApplicationName("voice-commands")
    app.setQuitOnLastWindowClosed(False)
    if not args.grid_stdin and not QSystemTrayIcon.isSystemTrayAvailable():
        print("No system tray is available. Run inside your Plasma desktop session.", file=sys.stderr)
        return 1
    if args.grid_stdin:
        if listener_arguments:
            parser.error("--grid-stdin does not accept listener arguments")
        grid = GridController(app, args.pointer, execute=args.execute, timeout=args.grid_timeout)
        controller = SyntheticController(app, grid)
    else:
        from desktop import DesktopCommands
        desktop = DesktopCommands(app, args.pointer, execute=args.execute, features=features,
                                  player=args.player, grid_timeout=args.grid_timeout, wake=wake, handy=args.handy)
        listener_arguments += ["--control-stdin", "--wake", wake, *features.arguments()]
        controller = TrayController(app, args.listener, listener_arguments, desktop)

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
    if not args.grid_stdin and not args.start_paused:
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
