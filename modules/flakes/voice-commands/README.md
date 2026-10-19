# Local voice commands

An offline Vosk listener with a Qt system tray controller. Say **“hey computer play”**, **“hey computer pause”**, **“hey computer next”**, or **“hey computer previous”**. Each media command requires the wake phrase; no deliberate pause between wake phrase and command is needed. The optional experimental mouse grid adds **“hey computer click”** followed by numbered targeting commands.

## Home Manager

The shared module is `modules/home-manager/services/voice-commands.nix`. Lapdog enables it in `hosts/lapdog/home.nix`:

```nix
dog.services.voice-commands = {
  enable = true;
  extraArgs = [
    "--execute"
    "--mouse-grid"
    "--wake"
    "hey computer"
  ];
};
```

The module installs the package, a **Voice Commands** desktop launcher, and `voice-commands.service`. The service starts at graphical login, listens by default, and stops on graphical logout. The launcher starts that same service, so it does not launch another listener when the service is already running.

`extraArgs` is the complete argument list; its default is `[ "--execute" ]`. Each flag/value occupies its own list element. Multiword values such as the wake phrase occupy **one** element. These arguments are escaped for systemd, including spaces and literal `$`/`%` characters. The module also accepts a `package` override.

Useful flags:

| Flag | Effect |
| --- | --- |
| `--execute` | Invoke media actions and mouse clicks; omit for media logging and mouse-grid preview only. |
| `--mouse-grid` | Enable the single-display Plasma Wayland mouse experiment. Enabled on lapdog. |
| `--grid-timeout 20` | Cancel the grid after this many inactive seconds; default 20. |
| `--grid-stdin` | Read synthetic grid commands from the terminal, without a tray, microphone, or recognizer. |
| `--wake "hello computer"` | Change the English wake phrase without retraining. Words must exist in the selected Vosk model's vocabulary. |
| `--source <node.name>` | Pin a PipeWire microphone; otherwise use the default input. Find names with `wpctl status -n`. |
| `--player mpv` | Prefer a particular MPRIS player family/instance. |
| `--start-paused` | Show the tray without opening the microphone. |
| `--verbose` | Print partial/final recognition and rejected phrases for debugging. |
| `--stats-every 5` | Emit resource/signal statistics every five audio seconds; disabled by default. |

After the user manually switches lapdog to the updated configuration, use:

```sh
systemctl --user status voice-commands.service
journalctl --user -u voice-commands.service -f
systemctl --user restart voice-commands.service
```

Quit the old manually launched prototype before starting the managed service. Package/model downloads happen during provisioning; runtime recognition loads pinned local files and works offline.

## Tray controls

- **Click the microphone icon:** stop/resume listening.
- **Right-click:** Pause/Resume and Quit.
- **Green:** listening. **Grey:** paused. **Yellow:** starting/stopping. **Red:** listener failed; hover for the error and click to retry.

Pause gracefully exits the listener, closes PipeWire capture, and releases model memory. Resume starts a fresh recognizer, discarding pending audio. The Qt controller stays responsive because recognition runs in a separate process. It reacts to events rather than polling while idle.

Quit stops the service's application normally; `Restart=on-failure` does not undo a deliberate Quit. The desktop launcher or `systemctl --user start voice-commands.service` starts it again. A manual pause lasts until resume or application shutdown; a new login/service restart starts according to `extraArgs`.

## Standalone and diagnostic use

From the repository root:

```sh
nix run path:./modules/flakes/voice-commands -- --execute
nix run path:./modules/flakes/voice-commands -- --start-paused --execute
nix run path:./modules/flakes/voice-commands#listener -- --verbose
```

The tray forwards listener flags and consumes `--start-paused` itself. The headless listener also accepts `--wav /path/sample.wav` (uncompressed mono 16 kHz signed 16-bit PCM) and `--duration <audio-seconds>` for repeatable diagnostics. Files decode as fast as possible and are padded at EOF to finish recognition; this does not recreate live acoustic conditions.

Media dispatch selects one concrete player in playerctld's event-recency order, optionally filtered by `--player`, then sends the requested action to it. Unsupported actions do not fall through to another player. `play` uses MPRIS Play, never a toggle or an application launch; it can resume/start an existing session. Next/previous depend on the player/page's capabilities. A fresh activity tracker has no historical recency, so interact with the intended player after startup if several exist.

## Mouse-grid experiment

Requires **Plasma Wayland with exactly one enabled display**. With `--mouse-grid`:

1. Say **“hey computer click”** to show a full-screen, row-major 3×3 grid:

   ```text
   1  2  3
   4  5  6
   7  8  9
   ```

2. Say **“one”** through **“nine”**, one number per utterance, to subdivide that cell recursively. No wake phrase is needed while targeting.
3. Say **“click”** to left-click the red crosshair at the current region's center and close the grid. Without `--execute`, this logs `grid-would-click` without moving the pointer or clicking.

**“back”** undoes a subdivision, **“reset”** returns to the whole display, and **“cancel”** closes without clicking. The grid also cancels on inactivity, pause/quit, listener exit, overlay dismissal, or display geometry/scale changes. Numbers and bare “click” are ignored outside targeting mode. Media commands still require their wake phrase during targeting.

The overlay is click-through and does not take keyboard focus. It shows the selected region, a breadcrumb, and an enlarged reference keypad when the cells are too small for labels. This keypad is schematic, not a screenshot magnifier. The real pointer stays still until the final click. Subdivision stops before cells become smaller than one logical pixel.

### Try synthetic commands first

From a terminal in the Plasma session:

```sh
nix run path:./modules/flakes/voice-commands -- --grid-stdin
```

Type `click`, then `3`, `7`, `5`, and `click`, each followed by Enter. Spoken number words also work. Use `back`, `reset`, `cancel`, or `quit`; Ctrl+C and stdin EOF exit and clean up. This mode never opens the microphone. Add `--execute` to actually click. EOF allows an already committed click to finish (up to four seconds); explicit quit, pause, or display changes cancel any pending input.

For voice-driven preview, pause the managed listener using its tray icon, then run:

```sh
nix run path:./modules/flakes/voice-commands -- --mouse-grid --verbose
```

Add `--execute` for real clicks. Quit the standalone instance before resuming the managed listener.

### Plasma input integration

`pointer.c` uses KWin's `org_kde_kwin_fake_input` absolute motion and a left-button press/release, in desktop logical coordinates. This avoids ydotool 1.0.4's acceleration-sensitive relative approximation of absolute positioning. The helper runs as the desktop user; no ydotool group membership is required.

The package installs a hidden `voice-commands-pointer.desktop` entry declaring its KWin interface. **Real clicks require this registration to be installed in the desktop session**, normally by the user's manual Home Manager/NixOS deployment. A standalone `nix run` before deployment can preview the grid but does not install that registration. KWin looks up the helper's exact store executable, so use the deployed `voice-commands` command for real-click trials after switching. Errors are logged as `grid-error` or `action-failed`.

The Qt controller owns the region/history and sends mode changes over the listener's stdin. At mode boundaries, Vosk recreates its decoder while sharing the loaded model, and the PipeWire capture subprocess is restarted to discard upstream buffered audio too. Fresh decoders are primed with silent padding so short digits spoken immediately at mode entry are not lost to acoustic startup. Commands carry a generation number so results buffered before cancellation cannot reopen or act on a later grid. The headless `#listener` can emit grid events with `--mouse-grid`, but only the Qt controller injects mouse input.

Recognition constrains Vosk's vocabulary to the full wake-prefixed phrases plus `[unk]`; targeting mode additionally permits the grid commands. Only exact normalized final results are accepted. Partial results never execute actions. Constrained decoding still needs ordinary-speech rejection checks; it is not proof that any permitted transcript was genuinely spoken.

## Packaging and verification

`package.nix` is shared by the standalone flake and Home Manager, using the host's package set for the latter. The standalone flake pins Nixpkgs to lapdog's original validated revision. Vosk 0.3.45 uses a content-pinned upstream x86-64 wheel, patched for Nix; the small US English 0.15 model is a content-pinned archive. The tray uses PySide6/Qt with Wayland plugins. The obsolete sherpa backend, KWS models, comparison flags, NumPy/SentencePiece dependencies, and prototype package names have been removed.

The user validated Vosk recognition and tray behavior on lapdog, selecting Vosk for better quality at similar reported power use. Prior isolated checks covered real PipeWire capture, all four real mpv/MPRIS actions, multi-player targeting, pause/resume, startup cancellation, error retry, crash cleanup, Quit, and signal shutdown. Promotion checks passed for standalone/lapdog flake evaluation, both package builds, generated service/launcher configuration, all four expected commands and three rejection samples, optional diagnostics, and the tray lifecycle using real Vosk with a virtual PipeWire source.

For future changes, verify:

1. Standalone/lapdog flake checks and package builds.
2. Four expected commands, wake-only/command-only/ordinary-speech rejection, and optional WAV/resource diagnostics.
3. Pause/resume/quit and failure cleanup without leftover capture processes.
4. Generated service arguments, graphical-session lifetime, and desktop launcher targeting the same service.
5. On lapdog after manual deployment: login startup, speaker-playback recognition, intended media player, microphone sharing with Handy, and logout cleanup.

`nix flake check path:./modules/flakes/voice-commands` also runs `test_grid.py`: real Qt/QML loading, region/history behavior, preview isolation, timeout/cancellation/stale-result handling, pending-click cancellation, display-change cancellation, pixel-depth limits, upstream capture-buffer discard/cleanup, and mode-specific matching with the real Vosk model. The real-desktop input test is skipped in the offscreen sandbox.

For compositor-level verification, build `#test-runner` and run `check-voice-grid GridTests.test_real_absolute_click_and_focus` with `VOICE_GRID_DESKTOP_TEST=1` in an **isolated** KWin Wayland session whose `XDG_DATA_DIRS` includes this package's `share` directory. It creates a fullscreen target and checks focus retention, click-through behavior, and actual absolute click delivery. Use KWin's virtual backend and fresh config/cache directories; repeat with fractional scaling. This test injects real mouse input and is intended for the isolated session.

The initial experiment passed the offscreen checks and the isolated KWin test at 150% scaling, including actual click delivery after a finite synthetic stdin stream reaches EOF. Additional synthesized-speech checks passed all nine digits, four targeting controls, five wake-prefixed actions, and four rejection samples, plus a streamed-WAV listener/control-stdin round trip through normal → targeting → normal. Physical microphone accuracy, perceived speech latency, and visual comfort still need a trial on lapdog after manual deployment.
