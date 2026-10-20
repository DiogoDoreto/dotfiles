"""WebRTC VAD over the listener's existing 16 kHz mono s16 capture.

A fresh detector must see 300 ms of quiet before accepting speech. Together
with capture reset this keeps the entry command/tail out of dictation activity.
No recognized-word or energy-threshold policy lives here.
"""


class SpeechDetector:
    frame_bytes = 640  # WebRTC accepts 20 ms frames at 16 kHz.

    def __init__(self):
        import webrtcvad
        self.vad = webrtcvad.Vad(2)
        self.pending = b""
        self.quiet_frames = 0
        self.armed = False

    def feed(self, pcm):
        self.pending += pcm
        speaking = False
        while len(self.pending) >= self.frame_bytes:
            frame, self.pending = self.pending[:self.frame_bytes], self.pending[self.frame_bytes:]
            speech = self.vad.is_speech(frame, 16000)
            if not self.armed:
                self.quiet_frames = 0 if speech else self.quiet_frames + 1
                self.armed = self.quiet_frames >= 15
            else:
                speaking |= speech
        return speaking
