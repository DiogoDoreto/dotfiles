"""Shared spoken commands; importing these does not load Qt or Vosk."""

MEDIA_COMMANDS = ("play", "pause", "next", "previous")
NUMBERS = ("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
GRID_COMMANDS = (*NUMBERS, "click", "back", "reset", "cancel")
SCROLL_DIRECTIONS = ("up", "down")
SCROLL_COMMANDS = (*SCROLL_DIRECTIONS, "cancel")


def phrases_for(wake, mouse_grid=False, mode="normal", *, media=True, scroll=True):
    # Compatibility for standalone grid callers; policy lives in Features.
    from features import Features
    return Features(media, scroll, mouse_grid).phrases(wake, mode)
