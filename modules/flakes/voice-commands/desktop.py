"""Assemble desktop command groups around their transition owner."""

from PySide6.QtCore import QObject

from scroll import ScrollController
from session import CommandSession


class DesktopCommands(QObject):
    def __init__(self, app, pointer, execute=False, mouse_grid=False, grid_timeout=20, scroll=True):
        super().__init__(app)
        self.session = CommandSession(self)
        self.scroll = ScrollController(self, pointer, self.session, execute) if scroll else None
        self.grid = None
        if mouse_grid:
            from grid import GridController
            self.grid = GridController(app, pointer, execute, grid_timeout, session=self.session)
            self.grid.setParent(self)

    def command(self, command, generation):
        self.session.command(command, generation)

    def cancel(self, reason):
        self.session.cancel(reason)

    def cleanup(self):
        if self.grid:
            self.grid.cleanup()
        if self.scroll:
            self.scroll.cleanup()
