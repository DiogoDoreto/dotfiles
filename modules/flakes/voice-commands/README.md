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
| `--mouse-grid` | Enable the Plasma Wayland mouse experiment with monitor selection. Enabled on lapdog. |
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

Requires **Plasma Wayland with one to nine enabled displays**. With `--mouse-grid`:

1. Say **“hey computer click”**. With multiple displays, each gets a click-through overlay showing its number and output name. Say that number to choose a monitor. Numbers run left-to-right, then top-to-bottom for displays with the same horizontal position. With one display, selection is skipped.
2. The selected monitor shows a full-screen, row-major 3×3 grid:

   ```text
   1  2  3
   4  5  6
   7  8  9
   ```

3. Say **“one”** through **“nine”**, one number per utterance, to subdivide that cell recursively. No wake phrase is needed for monitor selection or grid targeting. The monitor-selection number does not also select a grid cell.
4. Say **“click”** to left-click the red crosshair at the current region's center and close the grid. Without `--execute`, this logs `grid-would-click` without moving the pointer or clicking. During monitor selection, “click” waits for a monitor number and never clicks.

**“back”** undoes a subdivision; at the root of a multi-monitor grid, it returns to monitor selection. **“reset”** returns to the whole selected display, and **“cancel”** closes all overlays without clicking. Monitor selection and targeting both cancel on inactivity, pause/quit, listener exit, overlay dismissal, monitor hotplug, or display geometry/scale changes. Numbers and bare “click” are ignored outside targeting mode. Media commands still require their wake phrase during targeting.

The overlay is click-through and does not take keyboard focus. It shows the selected region, a breadcrumb, and an enlarged reference keypad when the cells are too small for labels. This keypad is schematic, not a screenshot magnifier. The real pointer stays still until the final click. Subdivision stops before cells become smaller than one logical pixel.

The grid explicitly uses Qt Quick's software renderer. Its 2D lines, rectangles, and text do not require an EGL/OpenGL context or GPU driver setup. This also applies to standalone previews; no `QT_QUICK_BACKEND` environment setting is required.

### Try synthetic commands first

From a terminal in the Plasma session:

```sh
nix run path:./modules/flakes/voice-commands -- --grid-stdin
```

Type `click`, choose a monitor number if several are connected, then enter `3`, `7`, `5`, and `click`, each followed by Enter. Spoken number words also work. Use `back`, `reset`, `cancel`, or `quit`; Ctrl+C and stdin EOF exit and clean up. This mode never opens the microphone. Add `--execute` to actually click. EOF allows an already committed click to finish (up to four seconds); explicit quit, pause, or display changes cancel any pending input.

For voice-driven preview, pause the managed listener using its tray icon, then run:

```sh
nix run path:./modules/flakes/voice-commands -- --mouse-grid --verbose
```

Add `--execute` for real clicks. Quit the standalone instance before resuming the managed listener.

### Plasma input integration

`pointer.c` uses KWin's `org_kde_kwin_fake_input` absolute motion and a left-button press/release, in desktop logical coordinates. This avoids ydotool 1.0.4's acceleration-sensitive relative approximation of absolute positioning. The helper runs as the desktop user; no ydotool group membership is required.

The package installs a hidden `voice-commands-pointer.desktop` entry declaring its KWin interface. **Real clicks require this registration to be installed in the desktop session**, normally by the user's manual Home Manager/NixOS deployment. A standalone `nix run` before deployment can preview the grid but does not install that registration. KWin looks up the helper's exact store executable, so use the deployed `voice-commands` command for real-click trials after switching. Errors are logged as `grid-error` or `action-failed`.

The Qt controller owns monitor selection and region/history, and sends mode changes over the listener's stdin. Every overlay has its own screen binding; refinement uses coordinates local to the selected display, and the final click adds its desktop logical origin. Monitor selection uses the existing targeting vocabulary. A selection/refinement transition starts a new generation, discarding buffered monitor-choice results before they can narrow a grid. At mode boundaries, Vosk recreates its decoder while sharing the loaded model, and the PipeWire capture subprocess is restarted to discard upstream buffered audio too. Fresh decoders are primed with silent padding so short digits spoken immediately at mode entry are not lost to acoustic startup. Commands carry a generation number so results buffered before cancellation cannot reopen or act on a later grid. The headless `#listener` can emit grid events with `--mouse-grid`, but only the Qt controller injects mouse input.

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

`nix flake check path:./modules/flakes/voice-commands` also runs `test_grid.py`: real Qt/QML loading, monitor selection/back navigation, region/history behavior, preview isolation, timeout/cancellation/stale-result handling, pending-click cancellation, display-change cancellation, pixel-depth limits, upstream capture-buffer discard/cleanup, and mode-specific matching with the real Vosk model. A fresh-process `test_startup.py` probe simulates a desktop theme loading OpenSSL 3.5 during application initialization: grid startup must still work with the packaged QML/OpenSSL 3.6 dependencies. The runner imports grid/QML before creating `QApplication` to prevent the desktop's older crypto library from winning the load order. A separate `test_rendering.py` probe requests the RHI/OpenGL default in a graphics-less process and checks that the overlay still renders a frame and its red crosshair using software. The real-desktop input tests are skipped in the offscreen sandbox.

For compositor-level verification, build `#test-runner` and run `check-voice-grid GridTests.test_real_absolute_click_and_focus` with `VOICE_GRID_DESKTOP_TEST=1` in an **isolated** KWin Wayland session whose `XDG_DATA_DIRS` includes this package's `share` directory. It creates a fullscreen target and checks focus retention, click-through behavior, and actual absolute click delivery. Use KWin's virtual backend and fresh config/cache directories; repeat with fractional scaling. This test injects real mouse input and is intended for the isolated session.

With two or more virtual outputs, run `GridTests.test_real_monitor_selection_and_global_click` instead. It checks per-monitor overlay rendering, monitor selection, focus retention, and a real click on an offset display. Use `VOICE_GRID_EXPECT_MIXED_SCALE=1` to additionally require distinct output/window scales. Verification passed with monitors at 100% and 150%, including a vertical offset, without `QT_QUICK_BACKEND` in the application's environment. The user confirmed the deployed single-monitor workflow on lapdog; the multi-monitor interaction still needs a hands-on trial there.

The initial experiment passed the offscreen checks and the isolated KWin test at 150% scaling, including actual click delivery after a finite synthetic stdin stream reaches EOF. The compositor test also passed with `QT_QUICK_BACKEND` removed from the application environment after fixing the EGL startup failure. Additional synthesized-speech checks passed all nine digits, four targeting controls, five wake-prefixed actions, and four rejection samples, plus a streamed-WAV listener/control-stdin round trip through normal → targeting → normal. Physical microphone accuracy, perceived speech latency, and visual comfort still need a trial on lapdog after manual deployment.
