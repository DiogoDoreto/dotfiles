"""Route desktop command groups through one recognition session."""

from PySide6.QtCore import QObject

from scroll import ScrollController
from session import CommandSession


class DesktopCommands(QObject):
    def __init__(self, app, pointer, execute=False, mouse_grid=False, grid_timeout=20):
        super().__init__(app)
        self.session = CommandSession(self)
        self.scroll = ScrollController(self, pointer, self.session, execute)
        self.grid = None
        if mouse_grid:
            from grid import GridController
            self.grid = GridController(app, pointer, execute, grid_timeout, session=self.session)
            self.grid.setParent(self)

    def command(self, command, generation):
        if generation != self.session.generation:
            return
        if command == "scroll":
            if self.grid:
                # Switch directly between groups: an intermediate normal mode would
                # restart capture twice and can discard speech immediately after entry.
                # Always cancel: a committed click may still be pending after the grid closed.
                self.grid.cancel("mode-changed", enter_normal=False)
            self.scroll.open()
        elif command == "click" and self.scroll.active and self.grid:
            self.scroll.cancel("mode-changed", enter_normal=False)
            self.grid.command(command, self.session.generation)
            if not self.grid.active:
                self.session.enter("normal")
        elif self.scroll.active:
            # Media commands were already dispatched inside the listener by wake phrase.
            self.scroll.command(command)
        elif self.grid:
            self.grid.command(command, generation)

    def cancel(self, reason):
        if self.grid:
            self.grid.cancel(reason)
        self.scroll.cancel(reason)

    def cleanup(self):
        if self.grid:
            self.grid.cleanup()
        self.scroll.cleanup()
