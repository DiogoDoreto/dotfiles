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
    typing: bool = False

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
        return (mode == "normal" or (mode == "scroll" and self.scroll)
                or (mode == "grid" and self.mouse_grid) or (mode == "typing" and self.typing))

    def phrases(self, wake, mode="normal"):
        prefix = f"{wake} " if wake else ""
        if mode == "typing" and self.typing:
            return {f"{prefix}show help": "show help", f"{prefix}type": "type"}
        phrases = {f"{prefix}{command}": command for command in MEDIA_COMMANDS} if self.media else {}
        phrases[f"{prefix}show help"] = "show help"
        if self.typing:
            phrases[f"{prefix}type"] = "type"
        if self.scroll:
            phrases[f"{prefix}scroll"] = "scroll"
        if self.mouse_grid:
            phrases[f"{prefix}click"] = "click"
        if mode == "scroll" and self.scroll:
            phrases.update({command: command for command in SCROLL_COMMANDS})
        if mode == "grid" and self.mouse_grid:
            phrases.update({command: command for command in GRID_COMMANDS})
        return phrases

    def accepts(self, command, mode):
        return self.supports_mode(mode) and command in self.phrases("", mode).values()

    def help_sections(self, wake, execute, grid_timeout):
        """Resolved usage alongside vocabulary/eligibility; presentation has no feature policy."""
        prefix = f"{wake} " if wake else ""
        sections = [
            ("Operation", "Execution enabled: commands send real media, typing and desktop input actions."
             if execute else "Preview only: commands log intended actions; Handy is not launched and no media or desktop input is sent."),
            ("Help", f'Say “{prefix}show help” in any command group, or choose Help in the tray menu. '
             "Help cancels the current group and queued/pending actions, then returns to normal listening. "
             "Already delivered actions cannot be undone."),
            ("Tray controls", "Click the microphone icon to pause/resume, or right-click and choose "
             "Pause listening / Resume listening. Pause releases the microphone. Help is available "
             "while paused or after an error and does not resume listening. Close this window to keep the tray running."),
        ]
        if self.media:
            sections.append(("Media", f'Say “{prefix}play”, “{prefix}pause”, “{prefix}next”, or “{prefix}previous”. '
                             + ("Always include the wake phrase, even in a command group. " if wake else "No wake phrase is configured. ")
                             + "Acts on one existing MPRIS player, "
                             "preferring configured player selection and recent activity; no fallback after a failed action. "
                              "Play is not a toggle and does not launch an application."))
        if self.typing:
            sections.append(("Typing (one recording per invocation)", f'Say “{prefix}type”, then dictate. Waits up to 10 seconds for '
                             "speech, then finishes after 3 continuous seconds without speech, with a maximum of 20 seconds "
                             "per invocation even if speech continues. Repeated type is ignored; "
                             "media, scroll and grid commands are blocked during dictation. Help/pause/quit/display changes "
                             "finish recording and may type the captured text. Handy must already be running, idle, onboarded "
                             "and exclusively controlled by this workflow. CLI success is dispatch only, not acknowledgement. "
                             "After dispatches settle, normal commands resume and another explicit type is allowed. "
                             "Wait for Handy to finish and become idle before invoking type again; there is no processing-status protocol "
                             "or automatic restart. After entry, briefly pause for the waiting-for-speech state; "
                             "a fresh 300 ms quiet VAD gate excludes the command tail. Preview logs would-type/would-stop without launching Handy."))
        if self.scroll:
            sections.append(("Scrolling", f'Say “{prefix}scroll”, then “up” or “down” without a wake phrase; '
                             "sequences such as “up up down” work. Scrolls at the current pointer position without "
                             "moving it or focusing a window. Say “cancel” or wait 60 inactive seconds to exit. "
                             "Repeating the entry phrase refreshes the timeout. Real input requires Plasma Wayland."))
        if self.mouse_grid:
            sections.append(("Mouse grid", f'Say “{prefix}click”. Requires Plasma Wayland and one to nine enabled displays. '
                             "With multiple displays, say the monitor number first (ordered left-to-right, then top-to-bottom); "
                             "one display skips selection. Then say “one” through “nine” to refine a row-major 3×3 grid, "
                             "one number per utterance, without a wake phrase. Say “click” to click the region center and close. "
                             "During monitor selection, click waits for a number. “back” undoes refinement or returns to monitor "
                             "selection at the root; “reset” restores the selected display; “cancel” exits without clicking. "
                             f"Exits after {grid_timeout:g} inactive seconds or display changes. Overlays do not take focus; "
                             "the pointer stays still until the final click."))
        if self.scroll and self.mouse_grid:
            sections.append(("Switching groups", f'Say “{prefix}scroll” from the grid or “{prefix}click” while scrolling '
                             "to switch groups, discarding pending input from the previous group."))
        return sections
