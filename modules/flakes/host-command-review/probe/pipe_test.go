package probe

import (
	"context"
	"encoding/json"
	"os"
	"os/exec"
	"testing"
	"time"
)

type localBackend struct{}

func (localBackend) Start(context.Context, []byte) (Execution, error) {
	return StartLocal([]string{os.Args[0], "-test.run=^TestChildProcess$"}, []string{"PROBE_CHILD=sleep"}, "", nil, 20*time.Millisecond)
}
func TestRunnerChild(t *testing.T) {
	if os.Getenv("PROBE_RUNNER") != "1" {
		return
	}
	emit := func(f Frame) error {
		// Pipes on Linux support Go's file deadlines. A blocked output reader must
		// fail the handshake, never prevent cancellation of an already started unit.
		os.Stdout.SetWriteDeadline(time.Now().Add(100 * time.Millisecond))
		return WriteFrame(os.Stdout, f)
	}
	r := Supervise(context.Background(), os.Stdin, emit, digest([]byte("approved")), opts(), localBackend{})
	b, _ := json.Marshal(r)
	emit(Frame{99, b})
	os.Exit(0)
}
func TestRealStdioControlTransport(t *testing.T) {
	for _, mode := range []string{"eof-before-start", "eof-running", "cancel", "partial-frame"} {
		t.Run(mode, func(t *testing.T) {
			cmd := exec.Command(os.Args[0], "-test.run=^TestRunnerChild$")
			cmd.Env = []string{"PROBE_RUNNER=1"}
			input, err := cmd.StdinPipe()
			if err != nil {
				t.Fatal(err)
			}
			output, err := cmd.StdoutPipe()
			if err != nil {
				t.Fatal(err)
			}
			cmd.WaitDelay = time.Second
			if err = cmd.Start(); err != nil {
				t.Fatal(err)
			}
			t.Cleanup(func() { input.Close(); cmd.Process.Kill() })
			send(t, input, Frame{Manifest, []byte("approved")})
			ready, err := ReadFrame(output)
			if err != nil || ready.Type != Ready {
				t.Fatal(ready, err)
			}
			if mode != "eof-before-start" {
				send(t, input, Frame{Start, ready.Body})
			}
			switch mode {
			case "eof-before-start", "eof-running":
				input.Close()
			case "cancel":
				send(t, input, Frame{Cancel, nil})
			case "partial-frame":
				input.Write([]byte{0, 0})
			}
			// A blocked/misbehaving child cannot hang the test indefinitely.
			timer := time.AfterFunc(2*time.Second, func() { cmd.Process.Kill() })
			defer timer.Stop()
			terminal, err := ReadFrame(output)
			if err != nil || terminal.Type != 99 {
				t.Fatal(terminal, err)
			}
			var got Result
			if err = json.Unmarshal(terminal.Body, &got); err != nil {
				t.Fatal(err)
			}
			wants := map[string]string{"eof-before-start": "start-rejected", "eof-running": "reviewer-eof", "cancel": "cancelled", "partial-frame": "lease-expired"}
			if got.Reason != wants[mode] && got.Reason != "pre-start-disconnect" {
				t.Fatal(got)
			}
			if err = cmd.Wait(); err != nil {
				t.Fatal(err)
			}
		})
	}
}
func TestBlockedPrivatePipeHasBoundedWrite(t *testing.T) {
	r, w, err := os.Pipe()
	if err != nil {
		t.Fatal(err)
	}
	defer r.Close()
	defer w.Close()
	if err = w.SetWriteDeadline(time.Now().Add(25 * time.Millisecond)); err != nil {
		t.Fatal(err)
	}
	_, err = w.Write(make([]byte, 1<<20)) // more than pipe capacity, no reader
	if err == nil || !os.IsTimeout(err) {
		t.Fatal("expected deadline", err)
	}
}
