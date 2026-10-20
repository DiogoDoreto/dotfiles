"""Assemble desktop command groups around their transition owner."""

from PySide6.QtCore import QObject

from scroll import ScrollController
from session import CommandSession
from features import Features
from media import MediaController
from help import HelpWindow


class DesktopCommands(QObject):
    def __init__(self, app, pointer, execute=False, grid_timeout=20, *, features=Features(), player=None,
                 wake="hey computer"):
        super().__init__(app)
        self.session = CommandSession(self, features)
        self.help = HelpWindow(features.help_sections(wake, execute, grid_timeout))
        self.session.help_requested.connect(self.help.show)
        self.media = MediaController(self, execute, player) if features.media else None
        self.session.media = self.media
        self.scroll = ScrollController(self, pointer, self.session, execute) if features.scroll else None
        self.grid = None
        if features.mouse_grid:
            from grid import GridController
            self.grid = GridController(app, pointer, execute, grid_timeout, session=self.session)
            self.grid.setParent(self)

    def command(self, command, generation):
        self.session.command(command, generation)

    def show_help(self):
        self.session.show_help()

    def cancel(self, reason):
        if self.media:
            self.media.stop()
        self.session.cancel(reason)

    def cleanup(self):
        self.help.cleanup()
        if self.media:
            self.media.cleanup()
        if self.grid:
            self.grid.cleanup()
        if self.scroll:
            self.scroll.cleanup()
