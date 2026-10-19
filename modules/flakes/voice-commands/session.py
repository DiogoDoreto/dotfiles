"""One recognition mode/generation shared by the desktop command groups."""

from PySide6.QtCore import QObject, Signal


class CommandSession(QObject):
    changed = Signal(str, int)

    def __init__(self, parent=None):
        super().__init__(parent)
        self.mode = "normal"
        self.generation = 0

    def enter(self, mode):
        self.mode = mode
        self.generation += 1
        self.changed.emit(mode, self.generation)
