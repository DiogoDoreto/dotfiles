# Local voice commands

An offline Vosk listener with a Qt system tray controller. Say **“hey computer play”**, **“hey computer pause”**, **“hey computer next”**, or **“hey computer previous”**. Each media command requires the wake phrase; no deliberate pause between wake phrase and command is needed. **“Hey computer scroll”** enters a repeated up/down scroll group. The optional experimental mouse grid adds **“hey computer click”** followed by numbered targeting commands.

## Home Manager

The shared module is `modules/home-manager/services/voice-commands.nix`. Lapdog enables it in `hosts/lapdog/home.nix`:

```nix
dog.services.voice-commands = {
  enable = true;
  execute = true;
  wakePhrase = "hey computer";
  features = {
    media.enable = true;
    scroll.enable = true;
    mouseGrid.enable = true;
  };
  # extraArgs = [ "--start-paused" "--verbose" ];
};
```

The module installs the package, a **Voice Commands** desktop launcher, and `voice-commands.service`. The service starts at graphical login, listens by default, and stops on graphical logout. The launcher starts that same service, so it does not launch another listener when the service is already running.

Typed options under `dog.services.voice-commands`:

| Option | Default | Effect |
| --- | --- | --- |
| `execute` | `true` | Invoke actions; set `false` for logging/previews. |
| `wakePhrase` | `"hey computer"` | English wake phrase supported by the selected Vosk model. |
| `features.media.enable` | `true` | Recognize media commands and enable media execution/activity tracking. |
| `features.scroll.enable` | `true` | Recognize the scroll group and construct its controller. |
| `features.mouseGrid.enable` | `false` | Recognize mouse-grid commands and construct its controller. |

`extraArgs` defaults to `[]` and supplements these options for diagnostics or unusual settings. Each flag/value occupies its own list element; multiword values occupy **one** element. Arguments are escaped for systemd, including spaces and literal `$`/`%` characters. Execution, wake, and feature flags (including their `--no-*` forms and `--flag=value` spellings) are rejected in `extraArgs`: migrate those to the typed options. `--grid-stdin` and the `--` end-of-options marker are also rejected there. The module accepts a `package` override; overridden packages must support the explicit positive/negative feature and execution flags emitted by the module.

Lapdog retains execution, media, scrolling, mouse grid, and the “hey computer” wake phrase. Home Manager still executes by default; standalone commands remain preview-only unless `--execute` is passed.

Useful flags:

| Flag | Effect |
| --- | --- |
| `--execute` | Invoke media, scroll, and click actions; omit to log actions and preview the mouse grid only. |
| `--mouse-grid` | Enable the Plasma Wayland mouse experiment with monitor selection. Enabled on lapdog. |
| `--no-media` | Disable media vocabulary, dispatch, and the playerctld helper. |
| `--no-scroll` | Disable scroll vocabulary and its controller/input helper. |
| `--no-mouse-grid` | Explicitly disable the mouse grid (the standalone default). |
| `--no-execute` | Explicitly select preview-only operation. |
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
- **Right-click:** Pause/Resume, Help, and Quit.
- **Green:** listening. **Blue:** scroll group active; the tooltip lists its commands. **Grey:** paused. **Yellow:** starting/stopping. **Red:** listener failed; hover for the error and click to retry.

Pause gracefully exits the listener, closes PipeWire capture, and releases model memory. Resume starts a fresh recognizer, discarding pending audio. The Qt controller stays responsive because recognition runs in a separate process. It reacts to events rather than polling while idle.

### Command help

Say **“hey computer show help”** (using your configured wake phrase), or choose **Help** from the tray menu. Help is always enabled, including when all optional features are disabled, and the wake-prefixed phrase works in normal, scroll, and grid modes. It opens one reusable window showing only enabled features, actual wake-phrase examples, group controls/timeouts, tray pause/resume instructions, and whether execution or preview mode is active. Repeated requests restore and raise that window; closing it keeps the tray running.

Both entry points cancel the current group, queued/in-flight media requests, pending scroll input, and any committed-but-pending click, before displaying help. Already delivered input cannot be undone. Help advances the recognition generation and returns to normal recognition without ending the listener lifetime or stopping media activity tracking. Stale voice help is rejected like other stale commands. Tray Help also works while paused or after an error without opening the microphone or resuming listening. The headless listener only emits a `show help` command event; it opens no window.

`ListenerLifetime` in `listener.py` owns that process lifetime and event transport without creating any widgets. Its interface is `start`, `stop`, `send_control(mode, generation)`, asynchronous `shutdown`, and final blocking `cleanup`; callers observe `state_changed`, `ready`, generation-tagged `command`, and `shutdown_complete`. States are Paused → Starting → Listening, Stopping → Paused for deliberate stops, and Error for failed starts/nonzero or crashed exits. A normal zero exit returns to Paused. Error detail is available as `last_error`; `closing` prevents starting again after shutdown. Start requests while starting/listening/stopping are ignored, so resuming requires the prior lifetime to settle.

The module frames newline JSON as bytes before decoding UTF-8, forwards listener stdout lines and stderr unchanged, and gates commands and controls on readiness. The synchronous ready notification lets assembly synchronize `CommandSession` before later commands are delivered; duplicate readiness is ignored. Stop intent immediately closes delivery, including output arriving during graceful exit; final exit output is diagnostic only. Each child starts a new Unix session. Stop allows five seconds for listener-owned cleanup before killing its process group; every exit also kills any surviving helpers in that group, even if the leader crashed. Final cleanup uses the remaining grace and at most two additional seconds to reap, within the managed service's ten-second `TimeoutStopSec`. No timer or process remains active while paused.

Quit stops the service's application normally; `Restart=on-failure` does not undo a deliberate Quit. The desktop launcher or `systemctl --user start voice-commands.service` starts it again. A manual pause lasts until resume or application shutdown; a new login/service restart starts according to `extraArgs`.

## Standalone and diagnostic use

From the repository root:

```sh
nix run path:./modules/flakes/voice-commands -- --execute
nix run path:./modules/flakes/voice-commands -- --start-paused --execute
nix run path:./modules/flakes/voice-commands#listener -- --verbose
```

The tray forwards recognition/diagnostic flags and consumes `--start-paused`, `--execute`/`--no-execute`, and `--player` itself. It resolves `--wake` for both help examples and listener recognition. Both tray and headless listener support `--media`/`--no-media`, `--scroll`/`--no-scroll`, and `--mouse-grid`/`--no-mouse-grid`. Media and scrolling default on, mouse grid and execution default off. Existing `--mouse-grid` invocations continue to work. Flag abbreviations are not supported. Disabled commands are absent from the vocabulary in every mode and rejected at dispatch; disabled controllers and the disabled media activity tracker are not started. All optional features may be disabled together; wake-prefixed help remains available. This does not change the package's dependency set. Synthetic `--grid-stdin` explicitly selects the standalone grid and does not use voice-feature selection.

**Headless compatibility:** `#listener` is a recognition/diagnostic interface, never an action executor. Its old `--execute`/`--no-execute` and `--player` flags remain accepted as deprecated, ignored options; requesting execution or a player emits a `deprecated-options` diagnostic. Use the tray command with `--execute --player mpv` for media execution. The listener emits generation-tagged command events only, regardless of these legacy flags; it never starts playerctl/playerctld. This intentionally replaces the old headless media-execution behavior.

The headless listener also accepts `--wav /path/sample.wav` (uncompressed mono 16 kHz signed 16-bit PCM) and `--duration <audio-seconds>` for repeatable diagnostics. Files decode as fast as possible and are padded at EOF to finish recognition; this does not recreate live acoustic conditions.

Media dispatch selects one concrete player in playerctld's event-recency order, optionally filtered by `--player`, then sends the requested action to it. Unsupported actions do not fall through to another player. `play` uses MPRIS Play, never a toggle or an application launch; it can resume/start an existing session. Next/previous depend on the player/page's capabilities. A fresh activity tracker has no historical recency, so interact with the intended player after startup if several exist.

`media.py` owns execution and the foreground playerctld helper in the controller process. It starts tracking only when an executing, media-enabled listener becomes ready, reusing an existing daemon if that daemon already owns the bus. Preview and disabled media start no media processes. A nonblocking 300 ms startup grace precedes queued requests. Listing and execution run sequentially through asynchronous QProcess calls, each with a two-second timeout; failed listing, no match, failed start, timeout, or failed action never selects a fallback player. Queue entries carry their recognition generation. Recognition transitions cancel queued/in-flight requests while keeping tracking alive. Pause, Quit, or listener exit also stops the owned tracker; resuming starts a fresh lifetime. Cancellation closes delivery before killing children, and late completions cannot consume newer requests. Runtime cancellation never waits for media subprocesses; final cleanup after the Qt event loop exits kills/reaps remaining children. An externally owned playerctld daemon is left running.

## Scroll command group

Scrolling is available by default in the tray application on **Plasma Wayland**:

1. Say **“hey computer scroll”** to enter the group. The tray turns blue and shows **Scrolling**.
2. Say **“up”** or **“down”**, repeatedly, without a wake phrase. Each direction sends a vertical wheel action to the desktop at the **current pointer position**; it does not move the pointer or choose/focus a window. Applications and display scaling determine the scroll distance.
3. Say **“cancel”**, or remain inactive for **60 seconds**, to return to normal wake-phrase listening. Each accepted direction restarts that one-minute inactivity timer.

You can also say a sequence such as **“up up down”** in one utterance. Only final results made entirely of direction commands are dispatched, in order. **“Cancel”** is a standalone command. Bare directions and cancel are ignored outside their command group. A repeated **“hey computer scroll”** keeps the group open and refreshes its timeout. Media actions still require their full wake-prefixed commands.

With `--mouse-grid`, a full **“hey computer click”** switches to mouse targeting, and **“hey computer scroll”** switches back from the grid. The groups share one recognition generation, so results queued before switching or cancelling cannot act in the next group. Switching groups, timeout, cancel, pause/quit, or listener exit clears queued scrolling and stops pending injection.

`--execute` is required to send wheel events. For a voice-driven preview, pause the managed listener using its tray icon, then run:

```sh
nix run path:./modules/flakes/voice-commands -- --verbose
```

Say the scroll entry phrase and directions; this logs `scroll-would-scroll` without input injection. Add `--execute` for real scrolling after the package's desktop registration has been deployed (see Plasma input integration below). Quit the standalone instance before resuming the managed listener.

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

`pointer.c` uses KWin's `org_kde_kwin_fake_input` absolute motion and a left-button press/release, in desktop logical coordinates. It also supports `--scroll up|down`, which sends a vertical axis event without pointer motion or button presses. Each direction uses a 15-unit axis step; client applications/scaling determine the resulting travel. This avoids ydotool 1.0.4's acceleration-sensitive relative approximation of absolute positioning. The helper runs as the desktop user; no ydotool group membership is required.

The package installs a hidden `voice-commands-pointer.desktop` entry declaring its KWin interface. **Real scrolling and clicks require this registration to be installed in the desktop session**, normally by the user's manual Home Manager/NixOS deployment. A standalone `nix run` before deployment can preview actions but does not install that registration. KWin looks up the helper's exact store executable, so use the deployed `voice-commands` command for real-input trials after switching. Errors are logged as `grid-error` or `action-failed`.

The Voice command runner keeps recognition in the listener process and all action execution in the Qt process. The immutable `Features` configuration in `features.py` resolves standalone parsing defaults, feature forwarding, mode validation, vocabulary, controller assembly, and dispatch eligibility. Home Manager's typed options remain authoritative by supplying explicit flags; the configuration adds no second host-default policy. `CommandSession` is the command-group transition module: its interface accepts generation-tagged `command` calls, lifecycle `cancel` calls, and listener-ready `synchronize` calls, and publishes recognition snapshots through `changed(mode, generation)`. `DesktopCommands` assembles the concrete media, grid, and scroll controllers and delegates commands/cancellation to that owner. Media commands are rejected when disabled or stale and routed before group commands, so wake-prefixed media during scrolling/targeting neither changes the group/generation nor refreshes its inactivity timer. There is no adapter hierarchy at this in-process seam.

Transition policy has one home: stale commands are rejected before dispatch; switching grid ↔ scroll publishes one new recognition generation without an intermediate normal mode; unsuccessful grid entry from scrolling returns to normal once. Monitor selection → refinement and returning to monitor selection advance the generation even though the mode stays grid. Cancellation closes the active group and stops pending input, including a committed click whose overlay has already closed. Grid display-change notifications cancel the shared interaction, including scrolling. Listener readiness closes any surviving interaction and publishes a normal snapshot at the current monotonic generation, rather than adopting the restarted listener's generation zero. Repeated scroll entry only refreshes its timer. This depth gives callers leverage without cancellation flags or cross-group ordering obligations, and keeps transition changes/tests local to the owner and its concrete behavior.

The standalone grid's `command`/`cancel` interface uses the same transition owner. Inside the grid, `GridNavigation` in `navigation.py` owns the complete targeting workflow without Qt: construct it with observed logical monitor geometry, read its immutable `view`, and send `command` calls. It sorts monitors by logical x, y, then output name; owns selection versus refinement, path/history, back/reset and the precision floor; and returns global click targets using the selected monitor's actual origin. Command results describe stage changes, rejections, or click/cancel intent. They never advance recognition generations: `CommandSession` retains that policy, and the Qt controller applies terminal intents and calls navigation `close` when the interaction ends.

`GridController` retains real display/scale observation, per-screen QML bindings, overlay visibility ordering, timers, and committed-click process lifetime. It translates navigation views/results into the existing overlay properties and diagnostic events. Every overlay has its own screen binding; refinement uses coordinates local to the selected display. There is no display-provider or GUI adapter framework at this in-process seam. Scroll queuing stays in its existing module.

At recognition transitions, Vosk recreates its decoder while sharing the loaded model, and the PipeWire capture subprocess is restarted to discard upstream buffered audio too. Fresh decoders are primed with silent padding so short commands spoken immediately at mode entry are not lost to acoustic startup. `ListenerLifetime` delivers recognition controls over the listener's stdin; the listener retains capture/decoder reset mechanics. Tray assembly connects readiness to `CommandSession.synchronize`, session snapshots to control delivery, and commands to desktop routing. Lifetime does not store, increment, or reconcile recognition generations. The tray handles icons/menu/labels and translates unavailable lifecycle states into desktop cancellation. The headless `#listener` emits desktop command events but only the Qt controller injects desktop input.

Recognition constrains Vosk's vocabulary to the full wake-prefixed phrases plus `[unk]`; targeting mode additionally permits grid commands, and scroll mode permits up/down/cancel. Only exact normalized final commands (or all-direction scroll sequences) are accepted. Partial results never execute actions. Constrained decoding still needs ordinary-speech rejection checks; it is not proof that any permitted transcript was genuinely spoken.

## Packaging and verification

`test_help.py` runs with the grid/scroll suite. It exercises feature-filtered content and execution/preview status, custom wake examples, voice help from every mode, stale-command rejection, tray Help while paused/error without listener startup, window reuse after close/minimize, active/queued scroll cancellation, and committed-click cancellation after overlay closure. Media checks also exercise help during listing/action with queued requests, preserving the tracker and successfully executing a new request afterward. `features.py` owns resolved help content next to recognition/eligibility policy; `help.py` owns only window presentation and cleanup.

`test_features.py` and `test_media.py` run with the grid/scroll suite. They cover all eight media/scroll/grid combinations with real Vosk matching in each mode, standalone defaults and legacy nonexecuting listener flags, feature-only tray-to-listener forwarding, controller-owned execution/player selection, absent disabled controllers, and rejection of disabled group transitions and injected disabled/out-of-mode actions. Real QProcess fixtures in `media_test_child.py` verify sequential action dispatch, concrete first-player selection and family filtering, no fallback on listing/action failure, failed starts, timeout responsiveness, cancellation during listing/action followed by a new request, helper cleanup, preview isolation, and restart. Tray integration with the real listener child verifies pause/crash/Quit cleanup; real scroll/grid interactions verify that media leaves groups and recognition generations intact. These fixtures never access MPRIS or inject desktop input.

`package.nix` is shared by the standalone flake and Home Manager, using the host's package set for the latter. The standalone flake pins Nixpkgs to lapdog's original validated revision. Vosk 0.3.45 uses a content-pinned upstream x86-64 wheel, patched for Nix; the small US English 0.15 model is a content-pinned archive. The tray uses PySide6/Qt with Wayland plugins. The obsolete sherpa backend, KWS models, comparison flags, NumPy/SentencePiece dependencies, and prototype package names have been removed.

The user validated Vosk recognition and tray behavior on lapdog, selecting Vosk for better quality at similar reported power use. Prior isolated checks covered real PipeWire capture, all four real mpv/MPRIS actions, multi-player targeting, pause/resume, startup cancellation, error retry, crash cleanup, Quit, and signal shutdown. Promotion checks passed for standalone/lapdog flake evaluation, both package builds, generated service/launcher configuration, all four expected commands and three rejection samples, optional diagnostics, and the tray lifecycle using real Vosk with a virtual PipeWire source.

For future changes, verify:

1. Standalone/lapdog flake checks and package builds.
2. Four expected commands, wake-only/command-only/ordinary-speech rejection, and optional WAV/resource diagnostics.
3. Pause/resume/quit and failure cleanup without leftover capture processes.
4. Generated service arguments, graphical-session lifetime, and desktop launcher targeting the same service.
5. On lapdog after manual deployment: login startup, speaker-playback recognition, intended media player, microphone sharing with Handy, and logout cleanup.

`nix flake check path:./modules/flakes/voice-commands` runs `test_navigation.py` with plain Python (no Qt), `test_listener.py` in its own widget-free `QCoreApplication` process, then `test_grid.py` and `test_scroll.py`. Navigation checks cross the production `GridNavigation` interface with complete selection → refinement → back/reset → target traces over unequal monitor sizes, negative and vertical origins, stable x/y/name ordering, all nine cells, rejected numbers, and precision-limit recovery. Build `#test-runner` and run its `check-voice-navigation -v` to focus on targeting rules.

Lifetime checks cross the same program/arguments seam as the packaged recognizer, using actual QProcess children from `listener_test_child.py`: fragmented JSON/UTF-8, readiness and control gating, duplicate readiness, stop before/during startup, commands after stop, failed start/retry, normal/error/crash outcomes, current session-generation delivery after restart, responsive escalation, surviving descendant cleanup after normal/crashed exits, and async/final shutdown. The tests reap their orphaned fixture helpers and never open the microphone or inject desktop input. Run the test runner's `check-voice-listener -v` to focus on lifetime behavior.

The grid/scroll suite covers real Qt/QML loading and region/breadcrumb presentation, monitor-overlay visibility and dismissal, scroll repetition and sequences, preview isolation, timeout/cancellation/stale-result handling, cross-group transitions, pending-input cancellation, display/scale-change cancellation, upstream capture-buffer discard/cleanup, and mode-specific matching with the real Vosk model. Transition traces cross the command interface and check recognition snapshots alongside accepted/rejected actions: monitor entry → refinement → back → direct switch, failed entry with queued input, timeout/display change, committed-click cancellation after overlay closure, and restarted-listener synchronization. Pending-action checks use controlled sleeping Python children, not real desktop input. A fresh-process `test_startup.py` probe simulates a desktop theme loading OpenSSL 3.5 during application initialization: grid startup must still work with the packaged QML/OpenSSL 3.6 dependencies. The runner imports grid/QML before creating `QApplication` to prevent the desktop's older crypto library from winning the load order. A separate `test_rendering.py` probe requests the RHI/OpenGL default in a graphics-less process and checks that the overlay still renders a frame and its red crosshair using software. The real-desktop input tests are skipped in the offscreen sandbox.

For compositor-level verification, build `#test-runner` and run `check-voice-grid GridTests.test_real_absolute_click_and_focus` with `VOICE_GRID_DESKTOP_TEST=1` in an **isolated** KWin Wayland session whose `XDG_DATA_DIRS` includes this package's `share` directory. It creates a fullscreen target and checks focus retention, click-through behavior, and actual absolute click delivery. Use KWin's virtual backend and fresh config/cache directories; repeat with fractional scaling. This test injects real mouse input and is intended for the isolated session.

With two or more virtual outputs, run `GridTests.test_real_monitor_selection_and_global_click` instead. It checks per-monitor overlay rendering, monitor selection, focus retention, and a real click on an offset display. Use `VOICE_GRID_EXPECT_MIXED_SCALE=1` to additionally require distinct output/window scales. Verification passed with monitors at 100% and 150%, including a vertical offset, without `QT_QUICK_BACKEND` in the application's environment. The user confirmed the deployed single-monitor workflow on lapdog; the multi-monitor interaction still needs a hands-on trial there.

`ScrollDesktopTests.test_real_repeated_scroll_at_current_pointer` checks actual repeated wheel-event delivery, direction/order, stable pointer coordinates, focus retention, and cancellation in the same isolated KWin setup.

The initial experiment passed the offscreen checks and the isolated KWin test at 150% scaling, including actual click delivery after a finite synthetic stdin stream reaches EOF. The compositor test also passed with `QT_QUICK_BACKEND` removed from the application environment after fixing the EGL startup failure. Additional synthesized-speech checks passed all nine digits, four targeting controls, five wake-prefixed actions, and four rejection samples, plus a streamed-WAV listener/control-stdin round trip through normal → targeting → normal. Physical microphone accuracy, perceived speech latency, and visual comfort still need a trial on lapdog after manual deployment.
