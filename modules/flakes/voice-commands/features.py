"""Resolved standalone feature selection, shared without importing Qt or Vosk.

Home Manager supplies explicit flags; these defaults belong to standalone use.
"""

import argparse
from dataclasses import dataclass, fields

from vocabulary import GRID_COMMANDS, MEDIA_COMMANDS, SCROLL_COMMANDS


@dataclass(frozen=True)
class Features:
    media: bool = True
    scroll: bool = True
    mouse_grid: bool = False

    @classmethod
    def add_arguments(cls, parser):
        for field in fields(cls):
            parser.add_argument("--" + field.name.replace("_", "-"),
                                action=argparse.BooleanOptionalAction, default=field.default,
                                help=f"Enable {field.name.replace('_', '-')} commands")

    @classmethod
    def from_args(cls, args):
        return cls(**{field.name: getattr(args, field.name) for field in fields(cls)})

    def arguments(self):
        return ["--" + ("" if getattr(self, field.name) else "no-") + field.name.replace("_", "-")
                for field in fields(self)]

    def supports_mode(self, mode):
        return mode == "normal" or (mode == "scroll" and self.scroll) or (mode == "grid" and self.mouse_grid)

    def phrases(self, wake, mode="normal"):
        phrases = {f"{wake} {command}": command for command in MEDIA_COMMANDS} if self.media else {}
        if self.scroll:
            phrases[f"{wake} scroll"] = "scroll"
        if self.mouse_grid:
            phrases[f"{wake} click"] = "click"
        if mode == "scroll" and self.scroll:
            phrases.update({command: command for command in SCROLL_COMMANDS})
        if mode == "grid" and self.mouse_grid:
            phrases.update({command: command for command in GRID_COMMANDS})
        return phrases

    def accepts(self, command, mode):
        return self.supports_mode(mode) and command in self.phrases("", mode).values()
