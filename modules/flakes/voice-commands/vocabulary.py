"""Shared spoken commands; importing these does not load Qt or Vosk."""

MEDIA_COMMANDS = ("play", "pause", "next", "previous")
NUMBERS = ("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
GRID_COMMANDS = (*NUMBERS, "click", "back", "reset", "cancel")
SCROLL_DIRECTIONS = ("up", "down")
SCROLL_COMMANDS = (*SCROLL_DIRECTIONS, "cancel")


def phrases_for(wake, mouse_grid=False, mode="normal", *, media=True, scroll=True):
    phrases = {f"{wake} {command}": command for command in MEDIA_COMMANDS} if media else {}
    if scroll:
        phrases[f"{wake} scroll"] = "scroll"
    if mouse_grid:
        phrases[f"{wake} click"] = "click"
        if mode == "grid":
            phrases.update({command: command for command in GRID_COMMANDS})
    if scroll and mode == "scroll":
        phrases.update({command: command for command in SCROLL_COMMANDS})
    return phrases
