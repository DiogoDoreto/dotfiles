# Local voice commands

An offline Vosk listener with a Qt system tray controller. Say **“hey computer play”**, **“hey computer pause”**, **“hey computer next”**, or **“hey computer previous”**. Each command requires the wake phrase; no deliberate pause between wake phrase and command is needed.

## Home Manager

The shared module is `modules/home-manager/services/voice-commands.nix`. Lapdog enables it in `hosts/lapdog/home.nix`:

```nix
dog.services.voice-commands = {
  enable = true;
  extraArgs = [
    "--execute"
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
| `--execute` | Invoke media actions; omit to print recognized commands only. |
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

Recognition constrains Vosk's vocabulary to the four full phrases plus `[unk]`, then accepts only exact normalized final results. Partial results never execute actions. Constrained decoding still needs ordinary-speech rejection checks; it is not proof that any permitted transcript was genuinely spoken.

## Packaging and verification

`package.nix` is shared by the standalone flake and Home Manager, using the host's package set for the latter. The standalone flake pins Nixpkgs to lapdog's original validated revision. Vosk 0.3.45 uses a content-pinned upstream x86-64 wheel, patched for Nix; the small US English 0.15 model is a content-pinned archive. The tray uses PySide6/Qt with Wayland plugins. The obsolete sherpa backend, KWS models, comparison flags, NumPy/SentencePiece dependencies, and prototype package names have been removed.

The user validated Vosk recognition and tray behavior on lapdog, selecting Vosk for better quality at similar reported power use. Prior isolated checks covered real PipeWire capture, all four real mpv/MPRIS actions, multi-player targeting, pause/resume, startup cancellation, error retry, crash cleanup, Quit, and signal shutdown. Promotion checks passed for standalone/lapdog flake evaluation, both package builds, generated service/launcher configuration, all four expected commands and three rejection samples, optional diagnostics, and the tray lifecycle using real Vosk with a virtual PipeWire source.

For future changes, verify:

1. Standalone/lapdog flake checks and package builds.
2. Four expected commands, wake-only/command-only/ordinary-speech rejection, and optional WAV/resource diagnostics.
3. Pause/resume/quit and failure cleanup without leftover capture processes.
4. Generated service arguments, graphical-session lifetime, and desktop launcher targeting the same service.
5. On lapdog after manual deployment: login startup, speaker-playback recognition, intended media player, microphone sharing with Handy, and logout cleanup.
