"""Typing ownership with real CLI/listener children and offline VAD/ASR audio."""

import io
import json
import os
from pathlib import Path
import signal
import struct
import subprocess
import sys
import tempfile
import threading
import time
from types import SimpleNamespace
import unittest
from unittest.mock import Mock, patch
import wave

from PySide6.QtCore import QCoreApplication, QProcess, QTimer
from PySide6.QtTest import QTest

from dictation import TypingController
from features import Features
from listener import ListenerLifetime
from runner import VoskRecognizer
import runner
from session import CommandSession
from speech import SpeechDetector

app = QCoreApplication.instance() or QCoreApplication([])


class TypingTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.path = Path(self.temp.name) / "calls"
        self.config = Path(self.temp.name) / "config.json"
        self.environment = patch.dict(os.environ, {"VOICE_HANDY_TEST_CONFIG": str(self.config)})
        self.environment.start()
        self.session = CommandSession(features=Features(typing=True))
        self.typing = TypingController(None, self.session, os.environ["VOICE_TEST_HANDY"], True,
                                       first_ms=180, silence_ms=100, client_ms=500)
        self.mode("success")
        self.events = []
        self.typing.log_event = lambda state, **kw: self.events.append((state, kw))

    def mode(self, mode):
        self.config.write_text(json.dumps({"log": str(self.path), "mode": mode}))

    def tearDown(self):
        self.session.cancel("test-end")
        self.wait(lambda: not self.typing.busy)
        self.typing.deleteLater()
        self.session.deleteLater()
        QTest.qWait(1)
        self.environment.stop()
        self.temp.cleanup()

    def wait(self, predicate, timeout=2000):
        deadline = time.monotonic() + timeout / 1000
        while not predicate() and time.monotonic() < deadline:
            QTest.qWait(5)
        self.assertTrue(predicate(), self.events)

    def calls(self):
        return [json.loads(line) for line in self.path.read_text().splitlines()] if self.path.exists() else []

    def say(self, command, generation=None):
        self.session.command(command, self.session.generation if generation is None else generation)

    def activity(self, speaking=True, generation=None, captured=None):
        self.typing.activity(speaking, self.session.generation if generation is None else generation,
                             time.monotonic() if captured is None else captured)

    def start(self):
        self.say("type")
        self.wait(lambda: self.typing.state == "waiting")

    def test_paired_dispatch_speech_resets_quiet_and_commands_are_blocked(self):
        self.start()
        owner = self.session.generation
        media = self.session.media = Mock()
        for command in ["type", "play", "scroll", "click", "down", "cancel"]:
            self.say(command)
        media.command.assert_not_called()
        self.assertEqual(self.session.generation, owner)
        self.activity()
        QTest.qWait(60)
        self.activity(False)
        self.activity()
        QTest.qWait(60)
        self.assertEqual(self.typing.state, "speech")
        self.wait(lambda: not self.typing.busy)
        self.assertEqual(self.calls(), [["--toggle-transcription"]] * 2)
        self.assertEqual(self.session.mode, "normal")
        self.say("type", owner)
        self.activity(generation=owner)
        QTest.qWait(120)
        self.assertEqual(len(self.calls()), 2, "Stale commands/activity cannot start another invocation")

    def test_two_explicit_invocations_each_own_one_pair_and_ignore_old_events(self):
        self.start()
        old_owner = self.session.generation
        self.activity()
        self.session.cancel("help")
        self.wait(lambda: not self.typing.busy)
        QTest.qWait(30)
        self.assertEqual(len(self.calls()), 2, "No automatic restart")
        self.start()  # Operational assumption: the user has waited for Handy to become idle.
        owner = self.session.generation
        self.assertGreater(owner, old_owner)
        self.say("type", old_owner)
        self.activity(generation=old_owner)
        self.typing._timeout()  # A queued old timer must respect the new deadline.
        self.assertEqual((self.typing.state, self.session.generation), ("waiting", owner))
        self.activity()
        self.wait(lambda: not self.typing.busy)
        self.assertEqual(self.calls(), [["--toggle-transcription"]] * 4)
        self.assertEqual(self.session.mode, "normal")

    def test_reentry_blocked_during_start_and_stop_even_after_normal_recognition(self):
        self.mode("slow")
        self.say("type")
        owner = self.session.generation
        self.say("type")
        self.assertEqual(self.session.generation, owner)
        self.wait(lambda: self.typing.state == "waiting")
        self.session.cancel("help")
        self.assertEqual(self.session.mode, "normal")
        self.assertEqual(self.typing.state, "stopping")
        for unused in range(5):
            self.say("type")
        self.wait(lambda: not self.typing.busy)
        self.assertEqual(len(self.calls()), 2)
        self.mode("success")
        self.start()
        self.session.cancel("done")
        self.wait(lambda: not self.typing.busy)
        self.assertEqual(len(self.calls()), 4)

    def test_initial_timeout_without_audio_is_wall_clock_based(self):
        self.start()
        self.wait(lambda: not self.typing.busy)
        self.assertEqual(len(self.calls()), 2)
        self.assertIn(("typing-finish", {"reason": "typing-initial-timeout"}), self.events)

    def test_continuous_speech_cannot_extend_maximum_and_next_invocation_gets_fresh_limit(self):
        self.typing.max_ms = 300
        for invocation in range(2):
            self.events.clear()
            started = time.monotonic()
            self.start()
            while self.typing.state in ("waiting", "speech") and time.monotonic() - started < 1:
                self.activity()
                QTest.qWait(20)
            self.wait(lambda: not self.typing.busy)
            self.assertIn(("typing-finish", {"reason": "typing-max-duration"}), self.events)
            self.assertGreaterEqual(time.monotonic() - started, .3)
            self.assertLess(time.monotonic() - started, 1)
            self.assertEqual(self.calls(), [["--toggle-transcription"]] * (2 * (invocation + 1)))
            self.assertEqual(self.session.mode, "normal")

    def test_continuously_voiced_entry_tail_cannot_hold_quiet_gate_past_initial_deadline(self):
        detector = SpeechDetector()
        # Controlled voiced frames: no 300 ms quiet interval is ever available.
        with patch.object(detector.vad, "is_speech", return_value=True):
            self.start()
            started = time.monotonic()
            while self.typing.busy and time.monotonic() - started < 1:
                self.activity(detector.feed(bytes(3200)))
                QTest.qWait(10)
            self.assertFalse(detector.armed)
            self.assertFalse(self.typing.busy, "The initial wall-clock deadline is independent of VAD arming")
        self.assertLess(time.monotonic() - started, 1)
        self.assertEqual(len(self.calls()), 2)
        self.assertIn(("typing-finish", {"reason": "typing-initial-timeout"}), self.events)

    def test_old_activity_and_timer_cannot_stop_a_later_group(self):
        self.start()
        owner = self.session.generation
        self.session.cancel("help")
        self.wait(lambda: not self.typing.busy)
        self.session._enter("scroll")
        generation = self.session.generation
        self.activity(generation=owner)
        self.typing._timeout()  # Already-queued timeout callback from the old typing interaction.
        QTest.qWait(200)
        self.assertEqual((self.session.mode, self.session.generation), ("scroll", generation))
        self.assertEqual(len(self.calls()), 2)

    def test_stale_activity_and_entry_pcm_do_not_start_speech(self):
        self.start()
        owner = self.session.generation
        for gen, timestamp in [(owner - 1, time.monotonic()), (owner, self.typing.armed_at - 1),
                               (owner, float("nan")), (owner, time.monotonic() + 10)]:
            self.activity(generation=gen, captured=timestamp)
        self.assertEqual(self.typing.state, "waiting")
        self.session.cancel("help")
        self.activity(generation=owner)
        self.wait(lambda: not self.typing.busy)
        self.assertEqual(len(self.calls()), 2)

    def test_teardown_during_start_waits_for_success_then_sends_stop_once(self):
        self.mode("slow")
        self.say("type")
        self.say("show help")
        self.assertEqual(self.session.mode, "normal")
        self.assertEqual(self.typing.state, "starting")
        self.wait(lambda: self.typing.state == "stopping")
        self.session.cancel("quit")
        self.session.synchronize()
        self.wait(lambda: not self.typing.busy)
        self.assertEqual(len(self.calls()), 2)

    def test_recording_teardown_causes_are_idempotent(self):
        self.start()
        self.activity()
        for reason in ("help", "listening-stopped", "listener-exited", "display-change", "mode-changed", "quit"):
            self.session.cancel(reason)
        self.wait(lambda: not self.typing.busy)
        self.assertEqual(len(self.calls()), 2)

    def test_failed_start_does_not_guess_stop_or_retry(self):
        self.mode("fail")
        self.say("type")
        self.wait(lambda: not self.typing.busy)
        self.assertEqual(len(self.calls()), 1)
        self.assertEqual(self.session.mode, "normal")
        self.assertTrue(any(state == "typing-error" and "controlled dispatch failure" in kw["error"]
                            for state, kw in self.events))
        QTest.qWait(30)
        self.assertEqual(len(self.calls()), 1, "No automatic retry after failure")
        self.mode("success")
        self.start()  # Explicit retry only after the user has restored Handy to idle.
        self.session.cancel("done")
        self.wait(lambda: not self.typing.busy)
        self.assertEqual(len(self.calls()), 3)

    def test_failed_to_launch(self):
        self.typing.deleteLater()
        self.typing = TypingController(None, self.session, "/does/not/exist", True)
        self.typing.log_event = lambda state, **kw: self.events.append((state, kw))
        self.say("type")
        self.wait(lambda: not self.typing.busy)
        self.assertEqual(self.calls(), [])
        self.assertEqual(self.session.mode, "normal")

    def test_stop_error_is_reported_once(self):
        self.start()
        self.mode("fail")
        self.session.cancel("quit")
        self.wait(lambda: not self.typing.busy)
        self.assertEqual(len(self.calls()), 2)
        self.assertTrue(any(state == "typing-error" and kw["phase"] == "stopping" for state, kw in self.events))

    def test_client_timeout_keeps_event_loop_responsive_and_never_retries(self):
        self.mode("hang")
        self.typing.client_timer.setInterval(80)
        tick = []
        QTimer.singleShot(20, lambda: tick.append(True))
        self.say("type")
        self.session.cancel("quit")
        self.wait(lambda: not self.typing.busy)
        self.assertTrue(tick)
        self.assertEqual(len(self.calls()), 1)
        self.assertTrue(any(state == "typing-error" for state, kw in self.events))

    def test_preview_uses_same_timers_without_launch(self):
        self.typing.execute = False
        self.typing.first_ms = 40
        self.start()
        self.wait(lambda: not self.typing.busy)
        self.assertEqual(self.calls(), [])
        self.assertEqual(self.typing.process.processId(), 0)
        self.assertIn(("typing-would-type", {}), self.events)
        self.assertIn(("typing-would-stop", {}), self.events)

    def test_disabled_and_custom_wake_mode_vocabulary(self):
        self.assertFalse(Features().supports_mode("typing"))
        self.assertNotIn("type", Features().phrases("hello computer").values())
        phrases = Features(typing=True).phrases("hello computer", "typing")
        self.assertEqual(phrases, {"hello computer type": "type", "hello computer show help": "show help"})
        session = CommandSession(features=Features())
        session.typing = Mock()
        session.command("type", 0)
        session.typing.enter.assert_not_called()

    def test_listener_transports_activity_separately_and_closes_delivery(self):
        script = ('import json,sys,time; print(json.dumps({"state":"listening"}),flush=True); '
                  'sys.stdin.readline(); '
                  'print(json.dumps({"state":"typing-activity","speaking":True,"generation":7,"monotonic_s":12.5}),flush=True); '
                  'time.sleep(60)')
        listener = ListenerLifetime(sys.executable, ["-u", "-c", script])
        activity, commands = [], []
        listener.typing_activity.connect(lambda *args: activity.append(args))
        listener.command.connect(lambda *args: commands.append(args))
        listener.ready.connect(lambda: listener.send_control("typing", 7))
        try:
            listener.start()
            self.wait(lambda: bool(activity))
            self.assertEqual(activity, [(True, 7, 12.5)])
            self.assertEqual(commands, [])
            listener.stop()
            self.wait(lambda: listener.state == "Paused")
            self.assertEqual(len(activity), 1)
        finally:
            listener.cleanup()

    def test_capture_and_detector_reset_on_recognition_generation(self):
        events, processes = [], []
        popen = subprocess.Popen
        def capture(*args, **kwargs):
            marker = len(processes) + 1
            process = popen([sys.executable, "-u", "-c",
                             f"import sys,time; sys.stdout.buffer.write(bytes([{marker}])*3200); "
                             "sys.stdout.buffer.flush(); time.sleep(60)"], **kwargs)
            processes.append(process)
            return process
        detectors = [Mock(), Mock()]
        for detector in detectors:
            detector.feed.return_value = True
        with patch.object(runner.subprocess, "Popen", side_effect=capture), patch.object(
                runner, "event", side_effect=lambda state, **kw: events.append((state, kw))), patch(
                "speech.SpeechDetector", side_effect=detectors):
            with runner.audio(SimpleNamespace(wav=None, source=None)) as (read, reset):
                reset("typing", 1)
                self.assertEqual(read(), bytes([2]) * 3200)
                reset("typing", 2)
                self.assertIsNotNone(processes[1].poll())
                self.assertEqual(read(), bytes([3]) * 3200)
        for marker, detector in enumerate(detectors, start=2):
            detector.feed.assert_called_once_with(bytes([marker]) * 3200)
        self.assertEqual([kw["generation"] for state, kw in events if state == "typing-activity"], [1, 2])
        self.assertTrue(all(process.poll() is not None for process in processes))

    def test_capture_reader_stall_discards_buffered_speech_instead_of_retimestamping(self):
        popen = subprocess.Popen
        script = ('import sys,time; '
                  '[(sys.stdout.buffer.write(bytes([1])*3200),sys.stdout.buffer.flush(),time.sleep(.02)) '
                  'for _ in range(12)]; time.sleep(60)')
        detector = Mock()
        detector.feed.side_effect = lambda pcm: (time.sleep(.35), True)[1]
        events = []
        with patch.object(runner.subprocess, "Popen", side_effect=lambda *args, **kw: popen(
                [sys.executable, "-u", "-c", script], **kw)), patch("speech.SpeechDetector", return_value=detector), patch.object(
                runner, "event", side_effect=lambda state, **kw: events.append((state, kw))):
            with runner.audio(SimpleNamespace(wav=None, source=None)) as (read, reset):
                started = time.monotonic()
                reset("typing", 1)
                with self.assertRaisesRegex(RuntimeError, "reader stalled"):
                    while True:
                        read()
        self.assertEqual(detector.feed.call_count, 1, "Buffered speech after the stall must not reach VAD")
        activity = [kw for state, kw in events if state == "typing-activity"]
        self.assertEqual(len(activity), 1)
        self.assertLess(activity[0]["monotonic_s"] - started, .2, "Timestamp precedes VAD processing, not pipe replay")

    def test_missing_or_partial_pcm_unblocks_listener_with_control_pending(self):
        # Run the actual runner/control loop separately so the regression itself
        # remains bounded even if its capture reader gets stuck indefinitely.
        harness = '''
import sys
from unittest.mock import Mock, patch
import runner
scripts = {
    "stops": "import os,time; os.write(1,bytes(3200)); time.sleep(60)",
    "partial": "import os,time; os.write(1,bytes(100)); time.sleep(60)",
    "trickle": "import os,time; [(os.write(1,b'x'),time.sleep(.1)) for _ in range(600)]",
    "eof-alive": "import os,time; os.close(1); time.sleep(60)",
}
popen = runner.subprocess.Popen
script = scripts[sys.argv[1]]
decoder = Mock()
decoder.feed.return_value = []
sys.argv = ["runner", "--model", "unused", "--typing", "--control-stdin"]
def capture(*args, **kwargs):
    return popen([sys.executable, "-u", "-c", script], **kwargs)
with patch.object(runner.subprocess, "Popen", side_effect=capture), patch.object(runner, "VoskRecognizer", return_value=decoder):
    raise SystemExit(runner.main())
'''
        for scenario in ("stops", "partial", "trickle", "eof-alive"):
            with self.subTest(scenario=scenario):
                child = subprocess.Popen([sys.executable, "-u", "-c", harness, scenario],
                                         stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                         start_new_session=True, bufsize=0)
                try:
                    # Allow the first full/partial write, then queue a control
                    # while the runner can already be blocked waiting for PCM.
                    time.sleep(.1)
                    try:
                        child.stdin.write(b'{"mode":"typing","generation":1}\n')
                        child.stdin.flush()
                    except BrokenPipeError:
                        self.assertEqual(scenario, "eof-alive", "Only EOF may terminate before the control write")
                    started = time.monotonic()
                    try:
                        child.wait(timeout=1)
                    except subprocess.TimeoutExpired:
                        self.fail("Capture starvation blocked the listener and its pending control for over one second")
                    output = child.stdout.read().decode()
                    self.assertEqual(child.returncode, 1, output + child.stderr.read().decode())
                    errors = [json.loads(line) for line in output.splitlines() if json.loads(line).get("state") == "error"]
                    self.assertEqual(len(errors), 1, output)
                    self.assertIn("capture", errors[0]["error"].lower())
                    if scenario != "eof-alive":
                        self.assertIn("stalled", errors[0]["error"])
                    self.assertLess(time.monotonic() - started, 1)
                finally:
                    try:
                        os.killpg(child.pid, signal.SIGKILL)
                    except ProcessLookupError:
                        pass
                    child.wait(timeout=2)
                    child.stdin.close()
                    child.stdout.close()
                    child.stderr.close()

    def test_partial_chunks_complete_within_the_same_absolute_deadline(self):
        popen = subprocess.Popen
        script = ('import os,time; os.write(1,bytes([1])*1000); time.sleep(.04); '
                  'os.write(1,bytes([2])*2200); time.sleep(60)')
        with patch.object(runner.subprocess, "Popen", side_effect=lambda *args, **kw: popen(
                [sys.executable, "-u", "-c", script], **kw)):
            with runner.audio(SimpleNamespace(wav=None, source=None)) as (read, reset):
                self.assertEqual(read(), bytes([1]) * 1000 + bytes([2]) * 2200)

    def test_decoder_backlog_is_bounded_and_fails_closed(self):
        popen = subprocess.Popen
        script = ('import sys,time; '
                  '[(sys.stdout.buffer.write(bytes(3200)),sys.stdout.buffer.flush(),time.sleep(.01)) '
                  'for _ in range(12)]; time.sleep(60)')
        with patch.object(runner.subprocess, "Popen", side_effect=lambda *args, **kw: popen(
                [sys.executable, "-u", "-c", script], **kw)):
            with runner.audio(SimpleNamespace(wav=None, source=None)) as (read, reset):
                time.sleep(.2)  # Decoder does not consume; the producer must not backpressure capture.
                with self.assertRaisesRegex(RuntimeError, "backlog exceeded 500 ms"):
                    read()

    def test_live_vad_observes_speech_end_while_decoder_is_blocked(self):
        quiet = threading.Event()
        observed_during_decode, voice_times = [], []
        popen = subprocess.Popen
        select_ready, read_bytes = runner.select.select, os.read
        control_sent = False
        def readable(readers, writers, errors, timeout):
            nonlocal control_sent
            if readers == [sys.stdin]:
                if not control_sent:
                    control_sent = True
                    return ([sys.stdin], [], [])
                return ([], [], [])
            return select_ready(readers, writers, errors, timeout)
        def read(fd, size):
            if fd == sys.stdin.fileno():
                return b'{"mode":"typing","generation":1}\n'
            return read_bytes(fd, size)
        # A real pipe buffers speech and subsequent silence independently of Vosk.
        script = ('import sys,time; '
                  '[(sys.stdout.buffer.write(bytes([value])*3200),sys.stdout.buffer.flush(),time.sleep(.1)) '
                  'for value in [1,1,0,0,0,0,0,0]]; time.sleep(60)')
        recognizer = Mock()
        def decode(pcm):
            if not observed_during_decode:
                observed_during_decode.append(quiet.wait(.7))
            return []
        recognizer.feed.side_effect = decode
        def output(state, **details):
            if state == "typing-activity":
                if details["speaking"]:
                    voice_times.append(time.monotonic())
                else:
                    quiet.set()
        detector = Mock()
        detector.feed.side_effect = lambda pcm: bool(pcm[0])
        with patch.object(sys, "argv", ["runner", "--model", "unused", "--typing", "--control-stdin", "--duration", ".5"]), patch.object(
                runner, "VoskRecognizer", return_value=recognizer), patch.object(
                runner.subprocess, "Popen", side_effect=lambda *args, **kw: popen([sys.executable, "-u", "-c", script], **kw)), patch.object(
                runner.select, "select", side_effect=readable), patch.object(
                runner.os, "read", side_effect=read), patch.object(
                runner, "event", side_effect=output), patch("speech.SpeechDetector", return_value=detector):
            started = time.monotonic()
            self.assertEqual(runner.main(), 0)
        self.assertEqual(observed_during_decode, [True], "VAD silence must arrive without waiting for Vosk")
        self.assertTrue(voice_times)
        self.assertLess(max(voice_times) - started, .5, "Buffered old speech must not be retimestamped after decoder delay")

    def test_real_vad_noncommand_speech_vs_silence_and_tail_gate(self):
        wav = subprocess.check_output([os.environ["VOICE_TEST_SPEECH"], "--stdout", "-v", "en-us",
                                       "a curious elephant remembers the purple umbrella"], timeout=5)
        with wave.open(io.BytesIO(wav)) as source:
            rate = source.getframerate()
            samples = [x for (x,) in struct.iter_unpack("<h", source.readframes(1_000_000))]
        samples = [samples[min(len(samples) - 1, int(i * rate / 16000))]
                   for i in range(int(len(samples) * 16000 / rate))]
        pcm = struct.pack(f"<{len(samples)}h", *samples)
        import webrtcvad
        vad = webrtcvad.Vad(2)
        speech_frame = next(pcm[i:i + 640] for i in range(0, len(pcm) - 640, 640)
                            if vad.is_speech(pcm[i:i + 640], 16000))
        tail = SpeechDetector()
        self.assertFalse(tail.feed(speech_frame * 20), "Entry speech cannot open the quiet gate")
        detector = SpeechDetector()
        self.assertFalse(detector.feed(bytes(32000)))
        self.assertTrue(any([detector.feed(pcm[i:i + 3200]) for i in range(0, len(pcm), 3200)]))
        for unused in range(20):
            quiet = detector.feed(bytes(3200))
        self.assertFalse(quiet)
        recognizer = VoskRecognizer(SimpleNamespace(model=os.environ["VOICE_TEST_MODEL"], verbose=False),
                                    Features(typing=True).phrases("hey computer", "typing"))
        commands = []
        for i in range(0, len(pcm), 3200):
            commands.extend(recognizer.feed(pcm[i:i + 3200]))
        commands.extend(recognizer.feed(bytes(32000)))
        commands.extend(recognizer.finish())
        self.assertEqual(commands, [], "Speech outside the command grammar still drives VAD")


if __name__ == "__main__":
    unittest.main()
