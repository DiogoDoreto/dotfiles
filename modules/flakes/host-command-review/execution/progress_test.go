package execution

import (
	"bytes"
	"context"
	"os"
	"os/exec"
	"path/filepath"
	"testing"
	"time"

	"host-command-review/contracts"
)

func TestProgressValidation(t *testing.T) {
	good := Progress{Stdout: []byte("secret"), Stderr: []byte{}}
	for _, mutate := range []func(*Progress){
		func(p *Progress) { p.StdoutOffset = 1 },
		func(p *Progress) { p.StderrOffset = 1 },
		func(p *Progress) { p.Stdout = nil },
		func(p *Progress) { p.Stdout = make([]byte, ProgressBytes+1) },
	} {
		p := good
		mutate(&p)
		var state progressState
		if state.accept(p) == nil {
			t.Fatal("invalid progress accepted")
		}
	}
	var state progressState
	if e := state.accept(good); e != nil {
		t.Fatal(e)
	}
	if state.accept(good) == nil {
		t.Fatal("replayed offset")
	}
	r := Result{Started: true, Stdout: []byte("secreX"), Stderr: []byte{}, Exit: contracts.ExitInfo{Kind: "exited", Code: 0}}
	if state.final(r) == nil {
		t.Fatal("mismatched final")
	}
	r.Stdout = []byte("secret tail")
	if e := state.final(r); e != nil {
		t.Fatal(e)
	}
	p := Progress{StdoutOffset: 6, Stdout: []byte{}, Stderr: []byte{}, Truncated: true}
	if e := state.accept(p); e != nil {
		t.Fatal(e)
	}
	p.Truncated = false
	if state.accept(p) == nil {
		t.Fatal("truncation regressed")
	}
	if state.final(r) == nil {
		t.Fatal("terminal truncation regressed")
	}
	b := progressBytes(good)
	if _, e := parseProgress(append(b, ' ')); e == nil {
		t.Fatal("noncanonical JSON")
	}
	if _, e := parseProgress(bytes.Replace(b, []byte("\"stdout_offset\""), []byte("\"unknown\""), 1)); e == nil {
		t.Fatal("unknown field")
	}
}

// Actual payload pipes and framed transport, with a child blocked until the
// receiver observes its prefix. No systemd, pkexec, or privileged operations.
func TestPayloadProgressBeforeCompletion(t *testing.T) {
	bash, e := exec.LookPath("bash")
	if e != nil {
		t.Skip(e)
	}
	dir, e := os.MkdirTemp(".", ".progress-test-")
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
	s.Script = "printf secret; printf error >&2; while [[ ! -e release ]]; do :; done"
	ctx, cancel := context.WithTimeout(context.Background(), 3*time.Second)
	defer cancel()
	in, out, e := os.Pipe()
	if e != nil {
		t.Fatal(e)
	}
	defer in.Close()
	defer out.Close()
	done := make(chan struct{})
	go func() {
		defer close(done)
		r := runPayloadObserved(ctx, cfg, s, func(p Progress) {
			if send(out, "progress", progressBytes(p)) != nil {
				cancel()
			}
		})
		send(out, "result", resultBytes(r))
	}()
	observed := false
	r, e := receiveResult(in, time.Now().Add(4*time.Second), func(p Progress) {
		if len(p.Stdout) > 0 {
			select {
			case <-done:
				t.Error("child completed before progress")
			default:
			}
			observed = true
			if e := os.WriteFile(filepath.Join(dir, "release"), nil, 0600); e != nil {
				t.Error(e)
			}
		}
	})
	if e != nil || !observed || string(r.Stdout) != "secret" || string(r.Stderr) != "error" || r.Exit.Code != 0 {
		t.Fatalf("observed=%v result=%+v err=%v", observed, r, e)
	}
	<-done
}

func TestFramedProgressRejectsInvalidTerminal(t *testing.T) {
	for _, mode := range []string{"offset", "oversize", "unknown", "mismatch", "truncation"} {
		t.Run(mode, func(t *testing.T) {
			in, out, e := os.Pipe()
			if e != nil {
				t.Fatal(e)
			}
			defer in.Close()
			defer out.Close()
			done := make(chan struct{})
			go func() {
				defer close(done)
				p := Progress{Stdout: []byte("prefix"), Stderr: []byte{}}
				if mode == "offset" {
					p.StdoutOffset = 9
				}
				if mode == "oversize" {
					p.Stdout = make([]byte, ProgressBytes+1)
				}
				if mode == "truncation" {
					p.Truncated = true
				}
				b := progressBytes(p)
				if mode == "unknown" {
					b = bytes.Replace(b, []byte("stdout_offset"), []byte("unknown"), 1)
				}
				if send(out, "progress", b) != nil {
					return
				}
				r := Result{Started: true, Stdout: []byte("prefix"), Stderr: []byte{}, Exit: contracts.ExitInfo{Kind: "exited", Code: 0}}
				if mode == "mismatch" {
					r.Stdout = []byte("wrong")
				}
				send(out, "result", resultBytes(r))
			}()
			if _, e := receiveResult(in, time.Now().Add(time.Second), nil); e == nil {
				t.Fatal("invalid framed stream accepted")
			}
			in.Close()
			<-done
		})
	}
}

func TestProgressDeadlineCannotBeRenewed(t *testing.T) {
	in, out, e := os.Pipe()
	if e != nil {
		t.Fatal(e)
	}
	defer in.Close()
	defer out.Close()
	done := make(chan struct{})
	go func() {
		defer close(done)
		for {
			if send(out, "progress", progressBytes(Progress{Stdout: []byte{}, Stderr: []byte{}})) != nil {
				return
			}
			time.Sleep(time.Millisecond)
		}
	}()
	start := time.Now()
	if _, e := receiveResult(in, start.Add(50*time.Millisecond), nil); e == nil {
		t.Fatal("missing absolute deadline")
	}
	if time.Since(start) > time.Second {
		t.Fatal("deadline reset")
	}
	in.Close()
	<-done
}

func TestObservedCapDrain(t *testing.T) {
	bash, e := exec.LookPath("bash")
	if e != nil {
		t.Skip(e)
	}
	dir, e := os.MkdirTemp(".", ".progress-cap-")
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
	s.Script = "printf -v pad '%1000s' ''; for ((i=0;i<6000;i++)); do printf '%s' \"$pad\"; done"
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	var state progressState
	r := runPayloadObserved(ctx, cfg, s, func(p Progress) {
		if e := state.accept(p); e != nil {
			t.Error(e)
		}
	})
	if r.Uncertain || !r.Truncated || len(r.Stdout) != contracts.CaptureBytes || !state.truncated || len(state.stdout) != contracts.CaptureBytes {
		t.Fatal("capture not bounded/drained")
	}
	if e := state.final(r); e != nil {
		t.Fatal(e)
	}
}
