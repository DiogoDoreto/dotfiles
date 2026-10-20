"""Local Vosk recognition/diagnostics; emits commands but never executes actions."""

import argparse
from contextlib import contextmanager
import json
import math
import os
from pathlib import Path
import re
import select
import signal
import subprocess
import struct
import sys
import time
import wave

from features import Features
from vocabulary import SCROLL_DIRECTIONS

RATE = 16000
CHUNK = 1600  # 100 ms, mono signed 16-bit PCM.


def normalize(text):
    return " ".join(re.findall(r"[a-z]+", text.lower()))


def event(state, **details):
    print(json.dumps({"time": time.strftime("%H:%M:%S"), "monotonic_s": round(time.monotonic(), 3),
                      "state": state, **details}), flush=True)


class VoskRecognizer:
    def __init__(self, args, phrases):
        import vosk

        vosk.SetLogLevel(-1)
        self.model = vosk.Model(str(args.model))
        self.verbose = args.verbose
        self.set_phrases(phrases)

    def set_phrases(self, phrases):
        import vosk

        missing = [word for word in set(" ".join(phrases).split()) if self.model.vosk_model_find_word(word) < 0]
        if missing:
            raise ValueError(f"Words absent from Vosk's vocabulary: {', '.join(sorted(missing))}")
        self.scroll_sequences = all(direction in phrases for direction in SCROLL_DIRECTIONS)
        grammar = list(phrases)
        if self.scroll_sequences:
            # Vosk's dynamic language model needs the transitions, including up->up
            # and down->down, to preserve repeated directions in one final result.
            grammar += [f"{first} {second}" for first in SCROLL_DIRECTIONS for second in SCROLL_DIRECTIONS]
        # A fresh decoder discards pending utterances at a mode boundary, but shares the model.
        self.recognizer = vosk.KaldiRecognizer(self.model, RATE, json.dumps([*grammar, "[unk]"]))
        self.recognizer.SetWords(True)
        # Prime a fresh decoder so a short digit spoken immediately at mode entry is
        # not lost to acoustic startup. This padding contains no captured/pending speech.
        self.recognizer.AcceptWaveform(bytes(RATE))  # 500 ms of mono s16 silence.
        self.phrases = phrases
        self.partial = ""

    def feed(self, pcm):
        if self.recognizer.AcceptWaveform(pcm):
            self.partial = ""
            return self.result(self.recognizer.Result())
        if self.verbose:
            partial = json.loads(self.recognizer.PartialResult()).get("partial", "")
            if partial != self.partial:
                event("partial", text=partial)
                self.partial = partial
        return []

    def result(self, raw):
        result = json.loads(raw)
        text = result.get("text", "")
        if not text:
            return []
        normalized = normalize(text)
        command = self.phrases.get(normalized)
        commands = [command] if command else []
        words = normalized.split()
        if not commands and self.scroll_sequences and words and all(word in SCROLL_DIRECTIONS for word in words):
            commands = words
        if self.verbose:
            event("final" if commands else "rejected", text=text, words=result.get("result", []))
        return commands

    def finish(self):
        return self.result(self.recognizer.FinalResult())


@contextmanager
def audio(args):
    if args.wav:
        with wave.open(str(args.wav), "rb") as source:
            if (source.getnchannels(), source.getsampwidth(), source.getframerate(), source.getcomptype()) != (1, 2, RATE, "NONE"):
                raise ValueError("--wav requires uncompressed mono 16 kHz signed 16-bit PCM")
            yield (lambda: source.readframes(CHUNK)), (lambda: None)
        return
    command = ["pw-record", "--raw", "--rate=16000", "--channels=1", "--format=s16",
               "--properties={ node.name=voice-commands-listener }"]
    if args.source:
        command += ["--target", args.source]
    capture = None

    def close():
        if capture:
            capture.terminate()
            try:
                capture.wait(timeout=2)
            except subprocess.TimeoutExpired:
                capture.kill()
                capture.wait()
            capture.stdout.close()

    def restart():
        nonlocal capture
        # Reset the producer as well as the decoder: old PCM queued in either process must
        # not be relabelled as a command from the new targeting generation.
        close()
        capture = subprocess.Popen([*command, "-"], stdout=subprocess.PIPE)

    def read():
        pcm = capture.stdout.read(CHUNK * 2)
        if not pcm:
            raise RuntimeError(f"PipeWire capture ended (exit {capture.wait()})")
        return pcm

    try:
        restart()
        yield read, restart
    finally:
        close()


def main():
    parser = argparse.ArgumentParser(description=__doc__, allow_abbrev=False,
                                     formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument("--model", type=Path, required=True)
    parser.add_argument("--wake", default="hey computer", help="English wake phrase, no training required")
    parser.add_argument("--source", help="PipeWire microphone node.name; otherwise follow the default input")
    parser.add_argument("--execute", action=argparse.BooleanOptionalAction, default=False,
                        help="Deprecated, ignored: the listener never executes actions")
    Features.add_arguments(parser)
    parser.add_argument("--control-stdin", action="store_true", help="Accept controller mode/generation JSON lines on stdin")
    parser.add_argument("--player", help="Deprecated, ignored: configure --player on the tray controller")
    parser.add_argument("--wav", type=Path, help="Replay a mono 16 kHz PCM WAV instead of opening the microphone")
    parser.add_argument("--duration", type=float, default=0, help="Stop after this many audio seconds; 0 means unlimited")
    parser.add_argument("--stats-every", type=float, default=0, help="Resource/signal report interval in audio seconds; 0 disables")
    parser.add_argument("--verbose", action="store_true", help="Print partial/final recognition and rejected phrases")
    args = parser.parse_args()
    features = Features.from_args(args)
    if args.execute or args.player:
        event("deprecated-options", message="Listener --execute/--player are nonexecuting; use the tray controller")

    def stop(signum, frame):
        # Tray stop and terminal Ctrl+C both unwind capture/model cleanup.
        raise KeyboardInterrupt

    signal.signal(signal.SIGTERM, stop)
    wake = normalize(args.wake)
    if not wake or args.duration < 0 or args.stats_every < 0:
        parser.error("Wake phrase must contain English words; duration and stats interval must be nonnegative")
    def vocabulary(mode="normal"):
        return features.phrases(wake, mode)

    phrases = vocabulary()
    mode = "normal"
    generation = 0
    control_buffer = b""
    started = time.monotonic()
    started_cpu = time.process_time()
    total_audio = decode_seconds = 0.0
    actions = 0

    def report(state):
        # nix run may exec us after evaluation; historical rusage includes that launcher.
        resident_pages = int(Path("/proc/self/statm").read_text().split()[1])
        event(state, engine="vosk", pid=os.getpid(), audio_s=round(total_audio, 2),
              wall_s=round(time.monotonic() - started, 2), cpu_s=round(time.process_time() - started_cpu, 3),
              rss_mib=round(resident_pages * os.sysconf("SC_PAGE_SIZE") / 1024 ** 2, 1),
              decode_rtf=round(decode_seconds / total_audio, 4) if total_audio else 0,
              actions=actions)

    def dispatch(commands):
        nonlocal actions
        for command in commands:
            if command not in phrases.values():
                event("rejected", command=command, mode=mode, reason="disabled-or-out-of-mode")
                continue
            actions += 1
            event("command", command=command, mode=mode, generation=generation,
                  audio_s=round(total_audio, 2))

    try:
        recognizer = VoskRecognizer(args, phrases)
        if features.scroll:
            recognizer.set_phrases(vocabulary("scroll"))
        if features.mouse_grid:
            # Check all targeting words before announcing readiness, not during the first click.
            recognizer.set_phrases(vocabulary("grid"))
        recognizer.set_phrases(phrases)
        model_load_seconds = time.monotonic() - started
        next_report = args.stats_every
        with audio(args) as (read, reset_capture):
            announced = False
            while not args.duration or total_audio < args.duration:
                if args.control_stdin and select.select([sys.stdin], [], [], 0)[0]:
                    data = os.read(sys.stdin.fileno(), 4096)
                    if not data:
                        raise RuntimeError("Controller input closed")
                    control_buffer += data
                    while b"\n" in control_buffer:
                        line, control_buffer = control_buffer.split(b"\n", 1)
                        control = json.loads(line)
                        new_mode, new_generation = control.get("mode"), control.get("generation")
                        if (not features.supports_mode(new_mode) or type(new_generation) is not int
                                or new_generation < generation):
                            raise ValueError("Invalid controller mode/generation")
                        if (new_mode, new_generation) != (mode, generation):
                            mode, generation = new_mode, new_generation
                            reset_capture()
                            phrases = vocabulary(mode)
                            recognizer.set_phrases(phrases)
                            event("mode-changed", mode=mode, generation=generation)
                pcm = read()
                if not pcm:
                    break
                if not announced:
                    event("listening", engine="vosk", phrases=list(phrases), source=args.source or "default",
                          input=str(args.wav) if args.wav else "microphone",
                          model_load_s=round(model_load_seconds, 3))
                    announced = True
                total_audio += len(pcm) / (RATE * 2)
                begin = time.monotonic()
                commands = recognizer.feed(pcm)
                decode_seconds += time.monotonic() - begin
                dispatch(commands)
                if args.stats_every and total_audio >= next_report:
                    energy = sum(value * value for (value,) in struct.iter_unpack("<h", pcm))
                    event("signal", rms=round(math.sqrt(energy / (len(pcm) // 2)) / 32768, 5))
                    report("stats")
                    next_report += args.stats_every
        if args.wav:
            # Flush endpoint delay at EOF; padding is not counted as source audio.
            dispatch(recognizer.feed(bytes(RATE * 2)))
            dispatch(recognizer.finish())
    except KeyboardInterrupt:
        event("stopping")
    except (ValueError, RuntimeError, OSError) as error:
        event("error", error=str(error))
        return 1
    finally:
        if args.stats_every:
            report("summary")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
