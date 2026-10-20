"""Shared spoken commands; importing these does not load Qt or Vosk."""

import re

MEDIA_COMMANDS = ("play", "pause", "next", "previous")
NUMBERS = ("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
GRID_COMMANDS = (*NUMBERS, "click", "back", "reset", "cancel")
SCROLL_DIRECTIONS = ("up", "down")
SCROLL_COMMANDS = (*SCROLL_DIRECTIONS, "cancel")


def normalize(text):
    return " ".join(re.findall(r"[a-z]+", text.lower()))


def phrases_for(wake, mouse_grid=False, mode="normal", *, media=True, scroll=True, typing=False):
    # Compatibility for standalone grid callers; policy lives in Features.
    from features import Features
    return Features(media, scroll, mouse_grid, typing).phrases(wake, mode)
