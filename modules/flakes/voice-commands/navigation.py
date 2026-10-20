"""Complete mouse-grid navigation, independent of Qt and recognition generations.

Construct from observed logical monitor geometry, read the immutable view, and send
commands. The sorted view retains the original monitor objects, including equal
observations, so callers can associate each with its actual display by identity.
Results describe presentation changes, rejections, or a click/cancel intent;
the caller owns their lifetime and calls close when the interaction ends. A stage
change is descriptive: only CommandSession decides recognition-generation policy.
"""

from dataclasses import dataclass, replace

from vocabulary import NUMBERS


@dataclass(frozen=True)
class Monitor:
    name: str
    x: float
    y: float
    width: float
    height: float


@dataclass(frozen=True)
class Region:
    x: float
    y: float
    width: float
    height: float

    def cell(self, number):
        row, column = divmod(number - 1, 3)
        return Region(self.x + column * self.width / 3, self.y + row * self.height / 3,
                      self.width / 3, self.height / 3)

    @property
    def center(self):
        return (self.x + self.width / 2, self.y + self.height / 2)


@dataclass(frozen=True)
class NavigationView:
    monitors: tuple = ()
    stage: str | None = None
    monitor: int | None = None
    region: Region | None = None
    path: tuple = ()


@dataclass(frozen=True)
class NavigationResult:
    state: str | None = None
    stage_changed: bool = False
    error: str | None = None
    position: tuple | None = None


class GridNavigation:
    def __init__(self, monitors):
        monitors = tuple(sorted(monitors, key=lambda monitor: (monitor.x, monitor.y, monitor.name)))
        if not 1 <= len(monitors) <= len(NUMBERS):
            raise ValueError("Mouse grid requires one to nine enabled displays")
        if any(monitor.width <= 0 or monitor.height <= 0 for monitor in monitors):
            raise ValueError("Monitor dimensions must be positive")
        self._view = NavigationView(monitors=monitors, stage="monitors")
        self._history = []
        if len(monitors) == 1:
            self._select_monitor(1)

    @property
    def view(self):
        return self._view

    def _select_monitor(self, number):
        monitor = self._view.monitors[number - 1]
        region = Region(0, 0, monitor.width, monitor.height)
        self._history = [region]
        self._view = replace(self._view, stage="grid", monitor=number, region=region, path=())

    def command(self, command):
        view = self._view
        if view.stage is None:
            return NavigationResult()
        if command == "cancel":
            return NavigationResult("cancel")
        if view.stage == "monitors":
            if command in NUMBERS:
                number = NUMBERS.index(command) + 1
                if number > len(view.monitors):
                    return NavigationResult("rejected", error="No monitor with that number")
                self._select_monitor(number)
                return NavigationResult("grid", stage_changed=True)
            if command == "click":
                return NavigationResult("select-monitor", error="Say a monitor number before clicking")
            return NavigationResult()
        if command in NUMBERS:
            region = view.region
            if min(region.width, region.height) / 3 < 1:
                return NavigationResult("limit", error="Already at logical-pixel precision; use click or back")
            number = NUMBERS.index(command) + 1
            region = region.cell(number)
            self._history.append(region)
            self._view = replace(view, region=region, path=(*view.path, number))
        elif command == "back":
            if view.path:
                self._history.pop()
                self._view = replace(view, region=self._history[-1], path=view.path[:-1])
            elif len(view.monitors) > 1:
                self._history = []
                self._view = NavigationView(monitors=view.monitors, stage="monitors")
                return NavigationResult("monitors", stage_changed=True)
        elif command == "reset":
            self._history = self._history[:1]
            self._view = replace(view, region=self._history[0], path=())
        elif command == "click":
            monitor = view.monitors[view.monitor - 1]
            center = view.region.center
            return NavigationResult("click", position=(center[0] + monitor.x, center[1] + monitor.y))
        else:
            return NavigationResult()
        return NavigationResult("grid")

    def close(self):
        self._view = NavigationView()
        self._history = []
