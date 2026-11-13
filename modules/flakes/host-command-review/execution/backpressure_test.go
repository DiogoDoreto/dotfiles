package execution

import (
	"context"
	"os"
	"os/exec"
	"path/filepath"
	"testing"
	"time"
)

func TestProgressBackpressureDoesNotBlockDraining(t *testing.T) {
	bash, e := exec.LookPath("bash")
	if e != nil {
		t.Skip(e)
	}
	dir, e := os.MkdirTemp(".", ".progress-pressure-")
	if e != nil {
		t.Fatal(e)
	}
	defer os.RemoveAll(dir)
	dir, _ = filepath.Abs(dir)
	cfg := config()
	cfg.RuntimeDir = dir
	s := spec()
	s.Shell = bash
	s.Cwd = dir
	s.Script = "printf -v pad '%1000s' ''; for ((i=0;i<6000;i++)); do printf '%s' \"$pad\"; done; : > drained"
	ctx, cancel := context.WithTimeout(context.Background(), 3*time.Second)
	defer cancel()
	blocked := make(chan struct{})
	release := make(chan struct{})
	done := make(chan Result, 1)
	go func() {
		first := true
		done <- runPayloadObserved(ctx, cfg, s, func(p Progress) {
			if first {
				first = false
				close(blocked)
				<-release
			}
		})
	}()
	<-blocked
	deadline := time.Now().Add(2 * time.Second)
	drained := false
	for time.Now().Before(deadline) {
		if _, e := os.Stat(filepath.Join(dir, "drained")); e == nil {
			drained = true
			break
		}
		time.Sleep(time.Millisecond)
	}
	close(release)
	r := <-done
	if !drained || !r.Truncated || r.Uncertain {
		t.Fatal("transport backpressure blocked cap/drain")
	}
}

func TestFailedProgressWriteCancelsPayload(t *testing.T) {
	bash, e := exec.LookPath("bash")
	if e != nil {
		t.Skip(e)
	}
	dir, e := os.MkdirTemp(".", ".progress-cancel-")
	if e != nil {
		t.Fatal(e)
	}
	defer os.RemoveAll(dir)
	dir, _ = filepath.Abs(dir)
	cfg := config()
	cfg.RuntimeDir = dir
	s := spec()
	s.Shell = bash
	s.Cwd = dir
	s.Script = "printf -v pad '%1000s' ''; for ((i=0;i<200;i++)); do printf '%s' \"$pad\"; done; while :; do :; done"
	in, out, e := os.Pipe()
	if e != nil {
		t.Fatal(e)
	}
	defer in.Close()
	defer out.Close()
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Second)
	defer cancel()
	failed := false
	start := time.Now()
	r := runPayloadObserved(ctx, cfg, s, func(p Progress) {
		if failed {
			return
		}
		out.SetWriteDeadline(time.Now().Add(30 * time.Millisecond))
		if WriteFrame(out, "progress", progressBytes(p)) != nil {
			failed = true
			cancel()
		}
	})
	if !failed || r.Uncertain || r.Exit.Kind != "cancelled" || time.Since(start) > time.Second {
		t.Fatalf("failed=%v outcome=%+v", failed, r.Exit)
	}
}
