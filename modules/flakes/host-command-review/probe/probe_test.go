package probe

import (
	"bytes"
	"context"
	"crypto/sha256"
	"encoding/binary"
	"encoding/hex"
	"io"
	"os"
	"os/exec"
	"strings"
	"sync"
	"testing"
	"time"
)

func digest(b []byte) string { s := sha256.Sum256(b); return hex.EncodeToString(s[:]) }

type chunkReader struct{ io.Reader }

func (c chunkReader) Read(p []byte) (int, error) {
	if len(p) > 1 {
		p = p[:1]
	}
	return c.Reader.Read(p)
}
func TestFraming(t *testing.T) {
	var b bytes.Buffer
	for _, f := range []Frame{{Manifest, []byte("secret\x00\n")}, {Start, []byte("nonce")}, {Cancel, nil}} {
		if err := WriteFrame(&b, f); err != nil {
			t.Fatal(err)
		}
	}
	for _, typ := range []byte{Manifest, Start, Cancel} {
		f, err := ReadFrame(chunkReader{&b})
		if err != nil || f.Type != typ {
			t.Fatalf("%v %v", f, err)
		}
	}
	if _, err := ReadFrame(&b); err != io.EOF {
		t.Fatal(err)
	}
	for _, n := range []uint32{0, MaxFrame + 1, ^uint32(0)} {
		var v bytes.Buffer
		binary.Write(&v, binary.BigEndian, n)
		if _, err := ReadFrame(&v); err == nil {
			t.Fatal("accepted size", n)
		}
	}
	for _, raw := range [][]byte{{0}, {0, 0, 0, 2, Manifest}} {
		if _, err := ReadFrame(bytes.NewReader(raw)); err == nil {
			t.Fatal("accepted truncated frame")
		}
	}
	if err := WriteFrame(io.Discard, Frame{Manifest, make([]byte, MaxFrame)}); err == nil {
		t.Fatal("oversize write")
	}
}
func TestApprovalExactBytes(t *testing.T) {
	raw := []byte(`{"script":"echo hi"}`)
	if err := VerifyApproved(raw, digest(raw)); err != nil {
		t.Fatal(err)
	}
	for _, bad := range []string{strings.ToUpper(digest(raw)), "bad", digest([]byte("other"))} {
		if VerifyApproved(raw, bad) == nil {
			t.Fatal("accepted", bad)
		}
	}
	if VerifyApproved(append(raw, '\n'), digest(raw)) == nil {
		t.Fatal("approved different bytes")
	}
}

type fakeBackend struct {
	mu     sync.Mutex
	starts int
	ex     *fakeExec
	err    error
}
type fakeExec struct {
	done   chan struct{}
	once   sync.Once
	stops  int
	result Result
}

func newFake() *fakeBackend {
	return &fakeBackend{ex: &fakeExec{done: make(chan struct{}), result: Result{127, "exited"}}}
}
func (b *fakeBackend) Start(context.Context, []byte) (Execution, error) {
	b.mu.Lock()
	defer b.mu.Unlock()
	b.starts++
	return b.ex, b.err
}
func (e *fakeExec) Done() <-chan struct{}      { return e.done }
func (e *fakeExec) Result() Result             { return e.result }
func (e *fakeExec) Stop(context.Context) error { e.stops++; e.finish(); return nil }
func (e *fakeExec) finish()                    { e.once.Do(func() { close(e.done) }) }
func opts() Options {
	return Options{Runtime: 2 * time.Second, Handshake: 200 * time.Millisecond, Lease: 100 * time.Millisecond, Grace: 100 * time.Millisecond}
}

type session struct {
	w     *io.PipeWriter
	ready chan Frame
	done  chan Result
	b     *fakeBackend
}

func begin(t *testing.T, o Options) *session {
	t.Helper()
	r, w := io.Pipe()
	s := &session{w: w, ready: make(chan Frame, 1), done: make(chan Result, 1), b: newFake()}
	go func() {
		s.done <- Supervise(context.Background(), r, func(f Frame) error { s.ready <- f; return nil }, digest([]byte("approved")), o, s.b)
	}()
	t.Cleanup(func() {
		w.Close()
		select {
		case <-s.done:
		default:
		}
	})
	return s
}
func send(t *testing.T, w io.Writer, f Frame) {
	t.Helper()
	if err := WriteFrame(w, f); err != nil {
		t.Fatal(err)
	}
}
func (s *session) authorize(t *testing.T) {
	send(t, s.w, Frame{Manifest, []byte("approved")})
	f := <-s.ready
	if f.Type != Ready {
		t.Fatal(f)
	}
	send(t, s.w, Frame{Start, f.Body})
}
func result(t *testing.T, s *session) Result {
	t.Helper()
	select {
	case r := <-s.done:
		return r
	case <-time.After(3 * time.Second):
		t.Fatal("unbounded lifecycle")
		return Result{}
	}
}
func TestPostAuthHandshake(t *testing.T) {
	for _, kind := range []string{"eof", "cancel", "wrong-nonce", "partial-frame", "timeout"} {
		t.Run(kind, func(t *testing.T) {
			s := begin(t, opts())
			send(t, s.w, Frame{Manifest, []byte("approved")})
			<-s.ready
			switch kind {
			case "eof":
				s.w.Close()
			case "cancel":
				send(t, s.w, Frame{Cancel, nil})
			case "wrong-nonce":
				send(t, s.w, Frame{Start, []byte("pre-auth-replay")})
			case "partial-frame":
				s.w.Write([]byte{0, 0})
			case "timeout":
			}
			if got := result(t, s); got.Reason != "start-rejected" {
				t.Fatal(got)
			}
			if s.b.starts != 0 {
				t.Fatal("started without live post-auth response")
			}
		})
	}
}
func TestRunningStops(t *testing.T) {
	for _, kind := range []string{"eof", "cancel", "timeout", "lease-expired", "protocol-error", "partial-frame"} {
		t.Run(kind, func(t *testing.T) {
			o := opts()
			if kind == "timeout" {
				o.Runtime = 40 * time.Millisecond
			}
			s := begin(t, o)
			s.authorize(t)
			switch kind {
			case "eof":
				s.w.Close()
			case "cancel":
				send(t, s.w, Frame{Cancel, nil})
			case "protocol-error":
				send(t, s.w, Frame{Start, nil})
			case "partial-frame":
				s.w.Write([]byte{0, 0})
			}
			got := result(t, s)
			expected := kind
			if kind == "cancel" {
				expected = "cancelled"
			}
			if kind == "eof" {
				expected = "reviewer-eof"
			}
			if kind == "partial-frame" {
				expected = "lease-expired"
			}
			// EOF/cancel may already be queued before backend submission; fail closed.
			if got.Reason != expected && got.Reason != "pre-start-disconnect" {
				t.Fatalf("got %+v expected %s", got, expected)
			}
			if s.b.starts > 0 && s.b.ex.stops != 1 {
				t.Fatal("stop not called exactly once")
			}
		})
	}
}
func TestLeaseAndExit127(t *testing.T) {
	s := begin(t, opts())
	s.authorize(t)
	for i := 0; i < 4; i++ {
		time.Sleep(30 * time.Millisecond)
		send(t, s.w, Frame{Lease, nil})
	}
	s.b.ex.finish()
	got := result(t, s)
	if got.ExitCode != 127 || got.Reason != "exited" {
		t.Fatal(got)
	}
	if ClassifyPkexec(127, true, true) != "runner-result" || ClassifyPkexec(126, true, false) != "runner-lost" || ClassifyPkexec(126, false, false) != "authentication-cancelled" {
		t.Fatal("ambiguous status classification")
	}
}
func TestInvalidManifestAndOptions(t *testing.T) {
	s := begin(t, opts())
	send(t, s.w, Frame{Manifest, []byte("unapproved")})
	if got := result(t, s); got.Reason != "manifest-rejected" {
		t.Fatal(got)
	}
	o := opts()
	o.Runtime = MaxRuntime + time.Second
	r := Supervise(context.Background(), io.NopCloser(bytes.NewReader(nil)), func(Frame) error { return nil }, "", o, newFake())
	if r.Reason != "invalid-options" {
		t.Fatal(r)
	}
}

func shell(t *testing.T) string {
	t.Helper()
	s, err := exec.LookPath("sh")
	if err != nil {
		t.Fatal(err)
	}
	return s
}
func waitLocal(t *testing.T, p *LocalProcess) {
	t.Helper()
	select {
	case <-p.Done():
	case <-time.After(3 * time.Second):
		ctx, cancel := context.WithTimeout(context.Background(), time.Millisecond)
		defer cancel()
		p.Stop(ctx)
		t.Fatal("process did not finish")
	}
}
func TestScriptFDNullStdinCleanEnvAndCapture(t *testing.T) {
	// Probe the transport technique, NOT the parent's manifest validator.
	f, err := ScriptFile(t.TempDir(), "read x && exit 90\nprintf '%s:%s' \"${PROBE_LEAK-unset}\" \"$PWD\"\nprintf err >&2\nexit 126\n")
	if err != nil {
		t.Fatal(err)
	}
	defer f.Close()
	t.Setenv("PROBE_LEAK", "must-not-leak")
	cwd := t.TempDir()
	p, err := StartLocal([]string{shell(t), "/proc/self/fd/3"}, []string{"LANG=C"}, cwd, []*os.File{f}, 50*time.Millisecond)
	if err != nil {
		t.Fatal(err)
	}
	waitLocal(t, p)
	if p.Result().ExitCode != 126 || string(p.Capture.Out) != "unset:"+cwd || string(p.Capture.Err) != "err" {
		t.Fatalf("result=%+v stdout=%q stderr=%q", p.Result(), p.Capture.Out, p.Capture.Err)
	}
}
func TestCaptureDrainsBeyondLimit(t *testing.T) {
	p, err := StartLocal([]string{os.Args[0], "-test.run=^TestChildProcess$"}, []string{"PROBE_CHILD=flood", "GOCOVERDIR=" + t.TempDir()}, "", nil, 100*time.Millisecond)
	if err != nil {
		t.Fatal(err)
	}
	waitLocal(t, p)
	c := p.Capture
	if len(c.Out)+len(c.Err) != CaptureLimit || c.SeenOut != 5<<20 || c.SeenErr != 5<<20 || p.Result().ExitCode != 0 {
		t.Fatalf("capture out=%d err=%d retained=%d", c.SeenOut, c.SeenErr, len(c.Out)+len(c.Err))
	}
}
func TestChildProcess(t *testing.T) {
	switch os.Getenv("PROBE_CHILD") {
	case "flood":
		b := bytes.Repeat([]byte("x"), 5<<20)
		os.Stdout.Write(b)
		os.Stderr.Write(b)
		os.Exit(0)
	case "sleep":
		for {
			time.Sleep(time.Hour)
		}
	}
}
func TestRealProcessCancel(t *testing.T) {
	p, err := StartLocal([]string{os.Args[0], "-test.run=^TestChildProcess$"}, []string{"PROBE_CHILD=sleep"}, "", nil, 30*time.Millisecond)
	if err != nil {
		t.Fatal(err)
	}
	ctx, cancel := context.WithTimeout(context.Background(), 200*time.Millisecond)
	defer cancel()
	if err = p.Stop(ctx); err != nil {
		t.Fatal(err)
	}
	waitLocal(t, p)
	if p.Result().ExitCode >= 0 {
		t.Fatal(p.Result())
	}
}
func TestInheritedOutputIsBoundedAfterMainExit(t *testing.T) {
	// Ordinary background descendants retaining pipes are killed on main exit.
	p, err := StartLocal([]string{shell(t), "-c", "sleep 60 & exit 0"}, []string{"PATH=" + os.Getenv("PATH")}, "", nil, 30*time.Millisecond)
	if err != nil {
		t.Fatal(err)
	}
	waitLocal(t, p)
}
func TestPlansContainNoCommandSecrets(t *testing.T) {
	unit := "host-review-" + strings.Repeat("a", 32) + ".service"
	argv, err := UnitPlan(unit, "/nix/store/"+strings.Repeat("a", 32)+"-runner/bin/runner", digest([]byte("secret command")), DefaultRuntime)
	if err != nil {
		t.Fatal(err)
	}
	joined := strings.Join(argv, " ")
	for _, flag := range []string{"--pipe", "--wait", "KillMode=control-group", "RuntimeMaxSec=120.000", "TimeoutStopSec=5s"} {
		if !strings.Contains(joined, flag) {
			t.Fatal("missing", flag)
		}
	}
	if strings.Contains(joined, "secret command") {
		t.Fatal("secret in argv")
	}
	if _, err := StopPlan(unit); err != nil {
		t.Fatal(err)
	}
	if _, err := StopPlan("--all"); err == nil {
		t.Fatal("option injection")
	}
}
