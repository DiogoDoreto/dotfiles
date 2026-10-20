"""Command-group transitions; listener transport belongs to ListenerLifetime.

The concrete grid/scroll controllers attach during construction. Their underscored
operations are internal to this module's implementation: callers send commands or
lifecycle cancellation, never coordinate group teardown and recognition resets.
"""

from PySide6.QtCore import QObject, Signal

from features import Features
from vocabulary import MEDIA_COMMANDS


class CommandSession(QObject):
    changed = Signal(str, int)

    def __init__(self, parent=None, features=None):
        super().__init__(parent)
        self.mode = "normal"
        self.generation = 0
        self.grid = None
        self.scroll = None
        self.media = None
        self.features = features or Features()

    def _enter(self, mode):
        if self.media:
            self.media.cancel()
        self.mode = mode
        self.generation += 1
        self.changed.emit(mode, self.generation)

    def command(self, command, generation):
        if (type(generation) is not int or generation != self.generation
                or not self.features.accepts(command, self.mode)):
            return
        if command in MEDIA_COMMANDS:
            if self.media:
                self.media.command(command, generation)
        elif command == "scroll" and self.scroll:
            # A click can still be in flight after its overlay has closed.
            if self.grid:
                self.grid._cancel_click("mode-changed")
                self.grid._close("mode-changed")
            if self.mode != "scroll":
                self._enter("scroll")
                self.scroll._open()
            else:
                self.scroll._command(command)
        elif command == "click" and self.grid and not self.grid.active:
            if self.scroll:
                self.scroll._close("mode-changed")
            # Successful entry reports its first stage. Do not pass through normal:
            # that would reset recognition/capture twice and discard fresh speech.
            self.grid._open()
            if not self.grid.active and self.mode != "normal":
                self._enter("normal")
        elif self.mode == "scroll" and self.scroll:
            self.scroll._command(command)
        elif self.grid and self.grid.active:
            self.grid._command(command)

    def _grid_stage_changed(self):
        # Monitor choice and refinement use the same grammar but different epochs.
        self._enter("grid")

    def _commit_click(self, position):
        self.grid._close("click")
        self._enter("normal")
        self.grid._click(position)

    def _close_groups(self, reason):
        if self.grid:
            self.grid._cancel_click(reason)
            self.grid._close(reason)
        if self.scroll:
            self.scroll._close(reason)

    def cancel(self, reason):
        if self.media:
            self.media.cancel()
        self._close_groups(reason)
        if self.mode != "normal":
            self._enter("normal")

    def synchronize(self):
        """Publish one normal-mode snapshot to a newly ready listener.

        A fresh listener starts at generation zero. Keep our monotonic generation,
        close any surviving group/input, and publish the snapshot for transport.
        """
        self._close_groups("listener-restarted")
        if self.media:
            self.media.stop()
            self.media.start()
        if self.mode != "normal":
            self._enter("normal")
        else:
            self.changed.emit(self.mode, self.generation)
