package execution

import (
	"bytes"
	"context"
	"encoding/binary"
	"encoding/json"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"sync/atomic"
	"testing"
	"time"

	"host-command-review/contracts"
)

const testShell = "/nix/store/0123456789abcdfghijklmnpqrsvwxyz-bash-5.3/bin/bash"

func config() Config {
	base := "/nix/store/0123456789abcdfghijklmnpqrsvwxyz-review/bin/"
	return Config{Shell: testShell, SystemdRun: base + "systemd-run", Systemctl: base + "systemctl", Helper: base + "host-command-unit-helper", Runner: base + "host-command-root-runner", RuntimeDir: "/run/host-review", ExpectedUID: 1000}
}
func spec() contracts.ExecutionSpec {
	s, e := contracts.Freeze(contracts.Proposal{Reason: "Inspect host state", Script: "echo secret", Cwd: "/secret/cwd", User: contracts.Dog, TimeoutSeconds: 120}, testShell)
	if e != nil {
		panic(e)
	}
	return s
}
func TestFrames(t *testing.T) {
	var b bytes.Buffer
	if e := WriteFrame(&b, "lease", []byte{}); e != nil {
		t.Fatal(e)
	}
	good := append([]byte{}, b.Bytes()...)
	f, e := ReadFrame(&b)
	if e != nil || f.Type != "lease" {
		t.Fatal(e)
	}
	for _, bad := range [][]byte{good[:2], good[:len(good)-1], {0, 0, 0, 0}, {255, 255, 255, 255}} {
		if _, e = ReadFrame(bytes.NewReader(bad)); e == nil {
			t.Fatal("invalid frame accepted")
		}
	}
	for _, body := range []string{`{"type":"lease","type":"lease","data":""}`, `{"type":"lease","Data":""}`, `{"type":"lease","data":null}`, `{"type":"lease","data":""} {}`} {
		var h [4]byte
		binary.BigEndian.PutUint32(h[:], uint32(len(body)))
		if _, e = ReadFrame(bytes.NewReader(append(h[:], []byte(body)...))); e == nil {
			t.Fatal("malformed accepted")
		}
	}
}
func TestManifestPlanAndIdentity(t *testing.T) {
	s := spec()
	b, d, e := Manifest(s)
	if e != nil {
		t.Fatal(e)
	}
	if _, e = parseManifest(b, d, config()); e != nil {
		t.Fatal(e)
	}
	if _, e = parseManifest(append(b, ' '), d, config()); e == nil {
		t.Fatal("changed bytes accepted")
	}
	other := config()
	other.Shell = strings.Replace(testShell, "5.3", "5.4", 1)
	if _, e = parseManifest(b, d, other); e == nil {
		t.Fatal("wrong shell")
	}
	args, e := config().UnitArgs(s, "host-review-"+strings.Repeat("a", 32)+".service", d)
	if e != nil {
		t.Fatal(e)
	}
	for _, a := range args {
		if strings.Contains(a, s.Script) || strings.Contains(a, s.Cwd) || strings.HasPrefix(a, "--property=Environment") || strings.HasPrefix(a, "--property=WorkingDirectory") {
			t.Fatal("metadata leak", a)
		}
	}
	for _, required := range []string{"--user", "--pipe", "--wait", "--property=RuntimeMaxSec=120s", "--property=KillMode=control-group", "--property=TimeoutStopSec=5s"} {
		if !contains(args, required) {
			t.Fatal("missing", required)
		}
	}
	if _, e = config().UnitArgs(s, "../../other", d); e == nil {
		t.Fatal("unsafe unit")
	}
	if config().CheckRunner(false) == nil {
		t.Fatal("development executable accepted")
	}
}
func contains(xs []string, s string) bool {
	for _, x := range xs {
		if x == s {
			return true
		}
	}
	return false
}

type fakeBackend struct {
	calls     atomic.Int32
	wait      bool
	cancelled chan struct{}
}

func (b *fakeBackend) Run(ctx context.Context, _ Config, _ contracts.ExecutionSpec, _ []byte, _ string) Result {
	b.calls.Add(1)
	if b.wait {
		<-ctx.Done()
		if b.cancelled != nil {
			close(b.cancelled)
		}
		return unknown(true)
	}
	return Result{Started: true, Stdout: []byte("private"), Stderr: []byte{}, Exit: contracts.ExitInfo{Kind: "exited", Code: 127}}
}
func supervisorPipes(t *testing.T, b Backend) (*os.File, *os.File, <-chan error) {
	t.Helper()
	in, iw, _ := os.Pipe()
	out, ow, _ := os.Pipe()
	done := make(chan error, 1)
	s := spec()
	_, d, _ := Manifest(s)
	go func() { defer in.Close(); defer ow.Close(); done <- Supervisor(config(), d, false, in, ow, b) }()
	t.Cleanup(func() { iw.Close(); out.Close() })
	return iw, out, done
}
func TestFirstGateAndEOF(t *testing.T) {
	for _, mode := range []string{"eof", "wrong", "correct", "cancel", "partial"} {
		t.Run(mode, func(t *testing.T) {
			b := &fakeBackend{}
			if mode == "cancel" || mode == "partial" {
				b.wait = true
			}
			in, out, done := supervisorPipes(t, b)
			manifest, _, _ := Manifest(spec())
			if e := send(in, "manifest", manifest); e != nil {
				t.Fatal(e)
			}
			f, e := receive(out, time.Second)
			if e != nil || f.Type != "runner_ready" || len(f.Data) != 32 {
				t.Fatal(f, e)
			}
			switch mode {
			case "eof":
				in.Close()
			case "wrong":
				send(in, "start", nonce())
			case "correct", "cancel", "partial":
				send(in, "start", f.Data)
				if mode == "cancel" {
					in.Close()
				}
				if mode == "partial" {
					in.Write([]byte{0, 0})
				}
			}
			if mode == "correct" || mode == "cancel" || mode == "partial" {
				f, e = receive(out, 4*time.Second)
				if e != nil || f.Type != "result" {
					t.Fatal(f, e)
				}
				r, e := parseResult(f.Data)
				if e != nil {
					t.Fatal(e)
				}
				if mode == "correct" && (r.Exit.Code != 127 || r.Uncertain) {
					t.Fatal("raw exit confused", r)
				}
				if mode != "correct" && !r.Uncertain {
					t.Fatal("claimed cancellation")
				}
			}
			select {
			case <-done:
			case <-time.After(4 * time.Second):
				t.Fatal("unbounded gate")
			}
			want := int32(0)
			if mode == "correct" || mode == "cancel" || mode == "partial" {
				want = 1
			}
			if b.calls.Load() != want {
				t.Fatal("gate bypass")
			}
		})
	}
}
func TestBlockedWritesBounded(t *testing.T) {
	r, w, _ := os.Pipe()
	defer r.Close()
	defer w.Close()
	w.SetWriteDeadline(time.Now().Add(20 * time.Millisecond))
	e := WriteFrame(w, "result", make([]byte, 1<<20))
	if e == nil || !os.IsTimeout(e) {
		t.Fatal(e)
	}
}
func TestCaptureBudgetDrainAndPayload126127(t *testing.T) {
	bash, e := exec.LookPath("bash")
	if e != nil {
		t.Skip("bash not available")
	}
	dir, e := os.MkdirTemp(".", ".payload-test-")
	if e != nil {
		t.Fatal(e)
	}
	defer os.RemoveAll(dir)
	dir, _ = filepath.Abs(dir)
	c := config()
	c.RuntimeDir = dir
	for _, script := range []string{"read x; test $? -eq 1; printf '%s' \"$HOME:$LANG:${BASH_ENV-unset}\"; exit 126", "exit 127", "printf -v pad '%1000s' ''; for ((i=0;i<5000;i++)); do printf '%s' \"$pad\"; printf '%s' \"$pad\" >&2; done"} {
		s := spec()
		s.Shell = bash
		s.Cwd = dir
		s.Script = script
		ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
		r := runPayload(ctx, c, s)
		cancel()
		if r.Uncertain || !r.Started || r.Validate() != nil {
			t.Fatal(r)
		}
		switch {
		case strings.Contains(script, "exit 126"):
			if r.Exit.Code != 126 || string(r.Stdout) != "/home/dog:C.UTF-8:unset" {
				t.Fatal(r)
			}
		case script == "exit 127":
			if r.Exit.Code != 127 {
				t.Fatal(r)
			}
		default:
			if !r.Truncated || len(r.Stdout)+len(r.Stderr) != contracts.CaptureBytes {
				t.Fatal("did not bound/drain capture")
			}
		}
	}
	// Real unprivileged main-process cancellation only; cgroup cleanup remains a
	// live systemd gate, never inferred from this test.
	s := spec()
	s.Shell = bash
	s.Cwd = dir
	s.Script = "while :; do :; done"
	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Millisecond)
	defer cancel()
	r := runPayload(ctx, c, s)
	if r.Exit.Kind != "timed_out" {
		t.Fatal(r)
	}
	entries, _ := os.ReadDir(dir)
	if len(entries) != 0 {
		t.Fatal("script file not unlinked")
	}
}
func TestTerminalValidation(t *testing.T) {
	for _, raw := range []string{`{}`, `{"started":true}`, `null`} {
		if _, e := parseResult([]byte(raw)); e == nil {
			t.Fatal("invalid terminal accepted")
		}
	}
	r := unknown(true)
	r.Exit.Kind = "exited"
	r.Exit.Code = 0
	if r.Validate() == nil {
		t.Fatal("uncertain success")
	}
	r = Result{Started: true, Stdout: make([]byte, contracts.CaptureBytes), Stderr: []byte{}, Exit: contracts.ExitInfo{Kind: "exited", Code: 0}}
	var wire bytes.Buffer
	if e := WriteFrame(&wire, "result", resultBytes(r)); e != nil {
		t.Fatal(e)
	}
	f, e := ReadFrame(&wire)
	if e != nil {
		t.Fatal(e)
	}
	if _, e = parseResult(f.Data); e != nil {
		t.Fatal(e)
	}
}

// A subprocess proves inherited stdin/stdout get actual deadlines, not ignored
// ErrNoDeadline. No privileged executable or systemd instance is used.
func TestStdioChild(t *testing.T) {
	if os.Getenv("HOST_REVIEW_TEST_CHILD") != "1" {
		return
	}
	in, out, e := ControlPipes()
	if e != nil {
		os.Exit(71)
	}
	defer in.Close()
	defer out.Close()
	if e = in.SetReadDeadline(time.Now().Add(time.Second)); e != nil {
		os.Exit(72)
	}
	f, e := ReadFrame(in)
	if e != nil {
		os.Exit(73)
	}
	if e = send(out, "echo", f.Data); e != nil {
		os.Exit(74)
	}
	os.Exit(0)
}
func TestInheritedStdio(t *testing.T) {
	cmd := exec.Command(os.Args[0], "-test.run=^TestStdioChild$")
	cmd.Env = []string{"HOST_REVIEW_TEST_CHILD=1"}
	in, e := cmd.StdinPipe()
	if e != nil {
		t.Fatal(e)
	}
	out, e := cmd.StdoutPipe()
	if e != nil {
		t.Fatal(e)
	}
	if e = cmd.Start(); e != nil {
		t.Fatal(e)
	}
	defer cmd.Process.Kill()
	if e = WriteFrame(in, "hello", []byte("exact")); e != nil {
		t.Fatal(e)
	}
	f, e := ReadFrame(out)
	if e != nil || f.Type != "echo" || string(f.Data) != "exact" {
		t.Fatal(f, e)
	}
	in.Close()
	if e = cmd.Wait(); e != nil {
		t.Fatal(e)
	}
}

func TestNoSystemdFallback(t *testing.T) {
	s := spec()
	manifest, d, _ := Manifest(s)
	r := (Systemd{}).Run(context.Background(), config(), s, manifest, d)
	if !r.Uncertain || r.Started {
		t.Fatal("missing systemd silently downgraded", r)
	}
}

// Check all framed helper bytes can cross the nested framing without exceeding
// limits at the full combined capture budget.
func TestMaxNestedResult(t *testing.T) {
	r := Result{Started: true, Stdout: make([]byte, contracts.CaptureBytes), Stderr: []byte{}, Exit: contracts.ExitInfo{Kind: "exited", Code: 0}}
	b := resultBytes(r)
	var buf bytes.Buffer
	if e := WriteFrame(&buf, "result", b); e != nil {
		t.Fatal(e)
	}
	f, e := ReadFrame(&buf)
	if e != nil {
		t.Fatal(e)
	}
	var v Result
	if e = json.Unmarshal(f.Data, &v); e != nil || v.Validate() != nil {
		t.Fatal(e)
	}
}
