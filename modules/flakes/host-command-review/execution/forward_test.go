package execution

import (
	"context"
	"os"
	"os/exec"
	"path/filepath"
	"testing"
	"time"
)

func TestNestedProgressForwardedBeforeChildFinishes(t *testing.T) {
	marker := filepath.Join(t.TempDir(), "started")
	cfg := config()
	backend := Systemd{command: func(ctx context.Context, path string, args ...string) *exec.Cmd {
		mode := "progress"
		if path == cfg.Systemctl {
			mode = "stop"
		}
		return exec.CommandContext(ctx, os.Args[0], "-test.run=^TestFakeSystemdChild$", "--", "--fake-systemd", mode, marker)
	}}
	runnerIn, clientOut, e := os.Pipe()
	if e != nil {
		t.Fatal(e)
	}
	defer runnerIn.Close()
	defer clientOut.Close()
	clientIn, runnerOut, e := os.Pipe()
	if e != nil {
		t.Fatal(e)
	}
	defer clientIn.Close()
	defer runnerOut.Close()
	s := spec()
	manifest, digest, e := Manifest(s)
	if e != nil {
		t.Fatal(e)
	}
	done := make(chan error, 1)
	go func() { done <- Supervisor(cfg, digest, false, runnerIn, runnerOut, backend) }()
	if e := send(clientOut, "manifest", manifest); e != nil {
		t.Fatal(e)
	}
	f, e := receive(clientIn, time.Second)
	if e != nil || f.Type != "runner_ready" {
		t.Fatal(f, e)
	}
	if e := send(clientOut, "start", f.Data); e != nil {
		t.Fatal(e)
	}
	observed := false
	r, e := receiveResult(clientIn, time.Now().Add(4*time.Second), func(p Progress) {
		if string(p.Stdout) != "captured" {
			t.Error("incorrect forwarded bytes")
		}
		select {
		case e := <-done:
			t.Errorf("finished before progress: %v", e)
		default:
		}
		observed = true
		if e := os.WriteFile(marker+".release", nil, 0600); e != nil {
			t.Error(e)
		}
	})
	if e != nil || !observed || r.Uncertain || string(r.Stdout) != "captured" {
		t.Fatalf("observed=%v result=%+v err=%v", observed, r, e)
	}
	if e := <-done; e != nil {
		t.Fatal(e)
	}
}
