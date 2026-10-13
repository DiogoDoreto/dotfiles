# THROWAWAY: lapdog voice-command comparison

**Question:** how do complete-phrase sherpa-onnx keyword spotting and continuous constrained Vosk feel on lapdog's built-in microphone, including while laptop speakers play music?

Both runners accept the same four English phrases, share a PipeWire PCM capture path, print timestamped recognition/action/resource events, and optionally control one MPRIS media player. The default wake phrase is **“hey computer”**: say **“hey computer pause”**, etc., in one utterance. This is a provisional phrase, not an engine selection or a production service.

## Run on lapdog

From the dotfiles repository root, run one engine at a time:

```sh
nix run path:./modules/flakes/voice-command-prototype#sherpa
nix run path:./modules/flakes/voice-command-prototype#vosk
```

Initially these print recognized commands. To control your actual media player:

```sh
nix run path:./modules/flakes/voice-command-prototype#sherpa -- --execute
nix run path:./modules/flakes/voice-command-prototype#vosk -- --execute
```

**Ctrl+C stops listening.** First startup fetches/builds the pinned engine dependencies and model; subsequent recognition uses local files and requires no network. Both run as your normal desktop user against your existing PipeWire and session bus. No system switch is needed.

Commands are `play`, `pause`, `next`, `previous`. Each needs its own wake phrase. Play invokes MPRIS Play (resumes/starts an existing session; does not launch a player), never a toggle. Next/previous depend on the app's playlist/page capabilities.

## Vosk system tray controller

Vosk was selected after the user's practical comparison: similar reported power usage, better recognition quality. Run its Qt tray layer inside lapdog's Plasma session:

```sh
nix run path:./modules/flakes/voice-command-prototype#vosk-tray -- --execute
```

- **Click the microphone icon** to stop/resume listening. Right-click for **Pause listening / Resume listening** and **Quit**.
- **Green:** listening. **Grey:** paused. **Yellow:** starting/stopping. **Red:** listener failed; hover for the error and click to retry.
- Pause gracefully exits the listener, closes its PipeWire capture, and releases model memory. Resume loads a fresh listener, so pending audio/commands are discarded. This is not process suspension with the microphone held open.
- Quit (or Ctrl+C in the launching terminal) stops the listener and removes the icon. Errors remain visible in the tray rather than silently leaving a green icon.
- Use `--start-paused` to show the icon without starting capture. Runner options such as `--wake`, `--source`, `--player`, and `--execute` are forwarded unchanged.

```sh
nix run path:./modules/flakes/voice-command-prototype#vosk-tray -- \
  --start-paused --execute --wake "hey computer" --player mpv
```

The tray controller supervises one Vosk process with event-driven Qt signals; it does not poll recognition or run it on the UI thread. Qt's tray protocol integrates with Plasma on Wayland. This is still a manually launched prototype; desktop autostart and lock/suspend policy belong to the eventual runner configuration.

## Common controls

Use identical options for both runs:

```sh
# Different wake phrase, fixed player family, five-minute run:
nix run path:./modules/flakes/voice-command-prototype#vosk -- \
  --wake "hello computer" --execute --player mpv --duration 300

# Pin the microphone by its PipeWire node.name:
nix run path:./modules/flakes/voice-command-prototype#sherpa -- \
  --source alsa_input.REPLACE_WITH_YOUR_NODE_NAME
```

Find microphone names with `wpctl status -n` on lapdog. Use the built-in mic node rather than a playback monitor. Without `--source`, both use the default input. The prototype has no explicit AEC processing; raw/default routing and speaker playback are part of the comparison.

`--execute` runs a foreground playerctld while the prototype is active, or reuses an existing daemon. The first concrete player in its event-recency order is selected per command, then pinned for dispatch: an unsupported action does not fall through to a different player. `--player mpv` filters that order to an instance/family. The temporary daemon is stopped on exit if this process started it; an already running daemon is retained. Starting a fresh tracker cannot recover earlier activity, so interact with your intended player after startup. Duplicate native/browser-extension endpoints and player disappearance remain visible experimental limitations.

## What to compare

For each engine, at the same desk position and volume:

1. Say each of the four complete phrases several times with no deliberate wake/command pause; repeat at a natural slower pace.
2. Try repeated commands with a fresh wake each time. Check missing, wrong, and duplicate `command` events.
3. Say the wake alone, a command without the wake, and ordinary conversation. Leave music playing without speaking and observe unintended events.
4. Repeat with music at your usual speaker volume. Try pause, then play after silence; try next/previous with an actual playlist.
5. With `--execute`, open two players, interact with one, and check the reported target. Try no player and unsupported next/previous.
6. Observe the five-second `stats` events during silence and playback. Compare process CPU and current RSS, and note perceived response delay. Run alongside Handy if you normally use it.

Record wake phrase, source name, speaker volume, successful/missed/wrong/duplicate commands, false activations and listening time. `audio_s` is position in input, **not end-to-end response latency**. `decode_rtf` is decoding wall time / input duration; lower is cheaper, not necessarily faster feedback. `cpu_s` starts at runner initialization, including model loading; compare deltas between successive stats for idle cost. `rss_mib` samples current resident memory, avoiding inherited launcher high-water marks. CPU/RSS cover the Python recognizer process (not PipeWire/playerctld or total laptop power). Stats are provisional diagnostics, not agreed acceptance thresholds. Partial Vosk output never executes commands.

## Replay the same recording

Both accept `--wav /absolute/path/sample.wav`: uncompressed mono **16 kHz signed 16-bit PCM**. Files decode as fast as possible, with endpoint padding at EOF. This isolates the recognizer comparison from microphone differences; it does not reproduce live playback/acoustic conditions.

```sh
nix run path:./modules/flakes/voice-command-prototype#sherpa -- --wav /path/sample.wav
nix run path:./modules/flakes/voice-command-prototype#vosk -- --wav /path/sample.wav
```

## Engine differences worth noticing

- **sherpa:** generates BPE keyword tokens from the configured complete phrases, spots them in the stream, and resets after a hit. Default CPU FP32 model, one thread, score 1.0, threshold 0.25, one trailing blank. Compare `--int8`, `--threshold 0.4`, or `--trailing-blanks 8` if useful; change one setting at a time. KWS can spot a phrase inside a longer utterance; it is not an exact whole-utterance rejection policy.
- **Vosk:** constrains vocabulary to the four phrases plus `[unk]`, waits for final endpoint output, then accepts only an exact normalized whole phrase. It rejects unsupported wake words at startup. Its grammar is a bigram language model, so unknown speech can still be misrecognized as a permitted phrase. Final-result gating trades latency for avoiding unstable partial executions.

## Reproducibility and provenance

- Nixpkgs pinned to lapdog's `e158d9ed9b51c98974c5e66e1ba1c9e0255fecaa`; standalone flake with its own lock.
- sherpa-onnx 1.13.3 from Nixpkgs, English GigaSpeech KWS 3.3M model dated 2024-01-01, with matching BPE/tokens and a content-pinned archive.
- Vosk 0.3.45 upstream PyPI x86-64 wheel (the available Python release, distinct from GitHub's release numbering), patched for Nix; small US English model 0.15, content-pinned archive.
- Recognition references: [sherpa keyword spotting](https://k2-fsa.github.io/sherpa/onnx/kws/index.html), [English model and tokenization](https://k2-fsa.github.io/sherpa/onnx/kws/pretrained_models/index.html#sherpa-onnx-kws-zipformer-gigaspeech-3-3m-2024-01-01-english), [Vosk model list and licenses](https://alphacephei.com/vosk/models), and [Vosk streaming API](https://github.com/alphacep/vosk-api/blob/v0.3.45/python/vosk/__init__.py).
- The selected engines and Vosk model use Apache-2.0; the [English sherpa model author's card](https://www.modelscope.cn/api/v1/models/pkufool/sherpa-onnx-kws-zipformer-gigaspeech-3.3M-2024-01-01/repo?Revision=master&FilePath=README.md) also declares Apache-2.0. SentencePiece and NumPy have their own package licenses.
- Integration references: [MPRIS player methods](https://specifications.freedesktop.org/mpris-spec/latest/Player_Interface.html), [playerctl/playerctld](https://github.com/altdesktop/playerctl/tree/v2.4.1), and [PipeWire capture](https://docs.pipewire.org/page_man_pw-cat_1.html).

Sources live here for easy experimental iteration. The local wayfinding map and cited research are under `.private/lapdog-voice-commands/`, which is git-ignored. Record the human's real-lapdog evaluation in its **Validate wake-and-command listening on lapdog** ticket when those local notes are available; the procedure above is sufficient to evaluate this standalone prototype.

## Verification already performed

On the agent machine, 2026-10-13:

- Standalone and lapdog flake checks passed; both executable packages built.
- Each engine emitted exactly the expected command for synthesized `hey computer play/pause/next/previous` WAVs, and no commands for command-only, wake-only, and ordinary-speech samples.
- Custom `hello computer pause` worked for both engines; sherpa's INT8 option loaded and recognized it. Cached runs worked with Nix's offline flag.
- Both live capture paths recognized the synthesized play phrase through a real, isolated PipeWire/WirePlumber virtual microphone and `pw-record`.
- Both engines invoked all four actions against real mpv/MPRIS in an isolated D-Bus session, with playback/playlist state checked. Recent-player selection with two mpv instances and no-player handling also passed.
- The Qt tray controller passed headless checks with real Vosk and a virtual PipeWire source: click/menu pause-resume, fresh-listener restart, startup cancellation, error/retry, crash cleanup, Quit, and Unix-signal shutdown. Pausing/quitting left no listener or microphone helper running. The packaged Qt launcher and missing-tray failure path were also checked; icon placement and click behavior in Plasma remain for the on-lapdog evaluation.

These are plumbing and smoke checks, not accuracy estimates. Your physical microphone, accent, speaker leakage, Firefox endpoints, Handy coexistence, perceived latency, and laptop power remain for the on-lapdog comparison.
