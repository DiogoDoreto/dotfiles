"""THROWAWAY: compare two real recognizers with the same audio/action plumbing."""

import argparse
from contextlib import contextmanager
import json
import os
from pathlib import Path
import re
import signal
import subprocess
import tempfile
import time
import wave

import numpy as np

RATE = 16000
CHUNK = 1600  # 100 ms, mono signed 16-bit PCM, shared by both engines.
COMMANDS = ("play", "pause", "next", "previous")


def normalize(text):
    return " ".join(re.findall(r"[a-z]+", text.lower()))


def event(state, **details):
    print(json.dumps({"time": time.strftime("%H:%M:%S"), "monotonic_s": round(time.monotonic(), 3),
                      "state": state, **details}), flush=True)


class VoskRecognizer:
    def __init__(self, args, phrases, directory):
        import vosk

        vosk.SetLogLevel(-1)
        model = vosk.Model(str(args.model))
        missing = [word for word in set(" ".join(phrases).split()) if model.vosk_model_find_word(word) < 0]
        if missing:
            raise ValueError(f"Words absent from Vosk's vocabulary: {', '.join(sorted(missing))}")
        self.recognizer = vosk.KaldiRecognizer(model, RATE, json.dumps([*phrases, "[unk]"]))
        self.recognizer.SetWords(True)
        self.phrases = phrases
        self.partial = ""

    def feed(self, pcm):
        if self.recognizer.AcceptWaveform(pcm):
            self.partial = ""
            return self.result(self.recognizer.Result())
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
        command = self.phrases.get(normalize(text))
        event("final" if command else "rejected", text=text, words=result.get("result", []))
        return [command] if command else []

    def finish(self):
        return self.result(self.recognizer.FinalResult())


class SherpaRecognizer:
    def __init__(self, args, phrases, directory):
        import sentencepiece
        import sherpa_onnx

        tokenizer = sentencepiece.SentencePieceProcessor(model_file=str(args.model / "bpe.model"))
        keywords = Path(directory) / "keywords.txt"
        keywords.write_text("\n".join(
            " ".join(tokenizer.encode(phrase.upper(), out_type=str)) + f" @{command}"
            for phrase, command in phrases.items()
        ) + "\n")
        suffix = "epoch-12-avg-2-chunk-16-left-64"
        precision = ".int8" if args.int8 else ""
        self.spotter = sherpa_onnx.KeywordSpotter(
            tokens=str(args.model / "tokens.txt"),
            encoder=str(args.model / f"encoder-{suffix}{precision}.onnx"),
            decoder=str(args.model / f"decoder-{suffix}.onnx"),
            joiner=str(args.model / f"joiner-{suffix}{precision}.onnx"),
            keywords_file=str(keywords),
            num_threads=args.threads,
            keywords_threshold=args.threshold,
            keywords_score=args.score,
            num_trailing_blanks=args.trailing_blanks,
            provider="cpu",
        )
        self.stream = self.spotter.create_stream()

    def feed(self, pcm):
        samples = np.frombuffer(pcm, dtype="<i2").astype(np.float32) / 32768.0
        self.stream.accept_waveform(RATE, samples)
        commands = []
        while self.spotter.is_ready(self.stream):
            self.spotter.decode_stream(self.stream)
            command = self.spotter.get_result(self.stream)
            if command:
                event("keyword", command=command)
                self.spotter.reset_stream(self.stream)
                if command in COMMANDS:
                    commands.append(command)
        return commands

    def finish(self):
        self.stream.input_finished()
        return self.feed(b"")


@contextmanager
def audio(args):
    if args.wav:
        with wave.open(str(args.wav), "rb") as source:
            if (source.getnchannels(), source.getsampwidth(), source.getframerate(), source.getcomptype()) != (1, 2, RATE, "NONE"):
                raise ValueError("--wav requires uncompressed mono 16 kHz signed 16-bit PCM")
            yield lambda: source.readframes(CHUNK)
        return
    command = ["pw-record", "--raw", "--rate=16000", "--channels=1", "--format=s16",
               "--properties={ node.name=voice-command-prototype }"]
    if args.source:
        command += ["--target", args.source]
    with subprocess.Popen([*command, "-"], stdout=subprocess.PIPE) as capture:
        try:
            def read():
                pcm = capture.stdout.read(CHUNK * 2)
                if not pcm:
                    raise RuntimeError(f"PipeWire capture ended (exit {capture.wait()})")
                return pcm
            yield read
        finally:
            capture.terminate()
            try:
                capture.wait(timeout=2)
            except subprocess.TimeoutExpired:
                capture.kill()
                capture.wait()


def media(command, preference):
    # Freeze a concrete instance before dispatch: no per-action fallback to another player.
    listed = subprocess.run(["playerctl", "--list-all"], capture_output=True, text=True, timeout=2)
    if listed.returncode:
        event("action-failed", command=command, error=listed.stderr.strip())
        return
    players = [p for p in listed.stdout.splitlines() if p != "playerctld"]
    if preference:
        players = [p for p in players if p == preference or p.startswith(preference + ".")]
    if not players:
        event("no-player", command=command, preference=preference)
        return
    target = players[0]
    result = subprocess.run(["playerctl", "--player", target, command], capture_output=True, text=True, timeout=2)
    event("executed" if result.returncode == 0 else "action-failed",
          command=command, player=target, error=result.stderr.strip())


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument("--engine", choices=("sherpa", "vosk"), required=True)
    parser.add_argument("--model", type=Path, required=True)
    parser.add_argument("--wake", default="hey computer", help="English wake phrase, no training required")
    parser.add_argument("--source", help="PipeWire microphone node.name; otherwise follow the default input")
    parser.add_argument("--execute", action="store_true", help="Actually invoke media actions; otherwise print events")
    parser.add_argument("--player", help="Prefer this MPRIS player family/instance, e.g. mpv")
    parser.add_argument("--wav", type=Path, help="Replay a mono 16 kHz PCM WAV instead of opening the microphone")
    parser.add_argument("--duration", type=float, default=0, help="Stop after this many audio seconds; 0 means unlimited")
    parser.add_argument("--stats-every", type=float, default=5, help="Resource and signal report interval in audio seconds")
    parser.add_argument("--threads", type=int, default=1, help="sherpa CPU inference threads")
    parser.add_argument("--threshold", type=float, default=0.25, help="sherpa trigger threshold")
    parser.add_argument("--score", type=float, default=1.0, help="sherpa keyword boosting")
    parser.add_argument("--trailing-blanks", type=int, default=1, help="sherpa trailing blanks before triggering")
    parser.add_argument("--int8", action="store_true", help="Use quantized sherpa encoder/joiner")
    args = parser.parse_args()

    def stop(signum, frame):
        # Tray stop and terminal Ctrl+C both unwind capture/model/tracker cleanup.
        raise KeyboardInterrupt

    signal.signal(signal.SIGTERM, stop)
    wake = normalize(args.wake)
    if not wake or args.threads < 1 or args.duration < 0 or args.stats_every <= 0:
        parser.error("Wake phrase must contain English words; threads/stats interval must be positive and duration nonnegative")
    phrases = {f"{wake} {command}": command for command in COMMANDS}
    started = time.monotonic()
    started_cpu = time.process_time()
    total_audio = decode_seconds = 0.0
    actions = 0
    tracker = None

    def report(state):
        # nix run may exec us after evaluation; historical rusage includes that launcher.
        resident_pages = int(Path("/proc/self/statm").read_text().split()[1])
        event(state, engine=args.engine, pid=os.getpid(), audio_s=round(total_audio, 2),
              wall_s=round(time.monotonic() - started, 2), cpu_s=round(time.process_time() - started_cpu, 3),
              rss_mib=round(resident_pages * os.sysconf("SC_PAGE_SIZE") / 1024 ** 2, 1),
              decode_rtf=round(decode_seconds / total_audio, 4) if total_audio else 0,
              actions=actions)

    def dispatch(commands):
        nonlocal actions
        for command in commands:
            actions += 1
            event("command", command=command, phrase=f"{wake} {command}", audio_s=round(total_audio, 2), execute=args.execute)
            if args.execute:
                try:
                    media(command, args.player)
                except subprocess.TimeoutExpired:
                    event("action-failed", command=command, error="Media call timed out")

    try:
        with tempfile.TemporaryDirectory(prefix="voice-command-prototype-") as directory:
            recognizer = (SherpaRecognizer if args.engine == "sherpa" else VoskRecognizer)(args, phrases, directory)
            model_load_seconds = time.monotonic() - started
            # No argument runs playerctld in the foreground. Reuse an existing daemon if it owns the bus.
            if args.execute:
                tracker = subprocess.Popen(["playerctld"], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
                time.sleep(0.3)
            next_report = args.stats_every
            with audio(args) as read:
                announced = False
                while not args.duration or total_audio < args.duration:
                    pcm = read()
                    if not pcm:
                        break
                    if not announced:
                        event("listening", engine=args.engine, phrases=list(phrases), source=args.source or "default",
                              input=str(args.wav) if args.wav else "microphone", execute=args.execute,
                              model_load_s=round(model_load_seconds, 3))
                        announced = True
                    total_audio += len(pcm) / (RATE * 2)
                    begin = time.monotonic()
                    commands = recognizer.feed(pcm)
                    decode_seconds += time.monotonic() - begin
                    dispatch(commands)
                    if total_audio >= next_report:
                        samples = np.frombuffer(pcm, dtype="<i2").astype(np.float32) / 32768
                        event("signal", rms=round(float(np.sqrt(np.mean(samples ** 2))), 5))
                        report("stats")
                        next_report += args.stats_every
            if args.wav:
                # Flush endpoint/KWS delay at EOF; padding is not counted as source audio.
                dispatch(recognizer.feed(bytes(RATE * 2)))
                dispatch(recognizer.finish())
    except KeyboardInterrupt:
        event("stopping")
    except (ValueError, RuntimeError, OSError) as error:
        event("error", error=str(error))
        return 1
    finally:
        if tracker and tracker.poll() is None:
            tracker.terminate()
            tracker.wait(timeout=2)
        report("summary")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
