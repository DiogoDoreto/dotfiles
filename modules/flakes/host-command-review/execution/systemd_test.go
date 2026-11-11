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

// This child emulates only the CLI/FD boundary, not systemd or cgroup semantics.
// No test invokes systemd-run, systemctl, pkexec, or a privileged executable.
func TestFakeSystemdChild(t *testing.T) {
	args := os.Args
	start := -1
	for i, a := range args {
		if a == "--fake-systemd" {
			start = i + 1
			break
		}
	}
	if start < 0 {
		return
	}
	args = args[start:]
	if len(args) != 2 {
		os.Exit(90)
	}
	mode, marker := args[0], args[1]
	if mode == "stop" {
		os.Exit(0)
	}
	in, out, e := ControlPipes()
	if e != nil {
		os.Exit(91)
	}
	defer in.Close()
	defer out.Close()
	f, e := receive(in, time.Second)
	if e != nil || f.Type != "manifest" {
		os.Exit(92)
	}
	if mode == "no-result" {
		os.Exit(0)
	}
	if mode == "late" {
		time.Sleep(200 * time.Millisecond)
	}
	challenge := nonce()
	if e = send(out, "unit_ready", challenge); e != nil {
		os.Exit(0)
	}
	f, e = receive(in, time.Second)
	if e != nil {
		os.Exit(0)
	}
	if f.Type != "unit_start" || !bytes.Equal(f.Data, challenge) {
		os.Exit(93)
	}
	if e = os.WriteFile(marker, []byte("gate crossed"), 0600); e != nil {
		os.Exit(94)
	}
	r := Result{Started: true, Stdout: []byte("captured"), Stderr: []byte{}, Exit: contracts.ExitInfo{Kind: "exited", Code: 126}}
	if e = send(out, "result", resultBytes(r)); e != nil {
		os.Exit(95)
	}
	if mode == "cli-failure" {
		os.Exit(1)
	}
	os.Exit(0)
}
func TestSystemdPrivateBoundary(t *testing.T) {
	for _, mode := range []string{"success", "no-result", "cli-failure", "late"} {
		t.Run(mode, func(t *testing.T) {
			marker := filepath.Join(t.TempDir(), "started")
			cfg := config()
			factory := func(ctx context.Context, path string, args ...string) *exec.Cmd {
				childMode := mode
				if path == cfg.Systemctl {
					childMode = "stop"
				} else {
					if path != cfg.SystemdRun {
						t.Error("unexpected executable")
					}
					for _, arg := range args {
						if arg == spec().Script || arg == spec().Cwd {
							t.Error("payload in command argv")
						}
					}
				}
				return exec.CommandContext(ctx, os.Args[0], "-test.run=^TestFakeSystemdChild$", "--", "--fake-systemd", childMode, marker)
			}
			backend := Systemd{command: factory}
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			if mode == "late" {
				time.AfterFunc(30*time.Millisecond, cancel)
			}
			s := spec()
			manifest, d, _ := Manifest(s)
			r := backend.Run(ctx, cfg, s, manifest, d)
			switch mode {
			case "success":
				if r.Uncertain || r.Exit.Code != 126 || !r.Started || string(r.Stdout) != "captured" {
					t.Fatal(r)
				}
			case "no-result", "cli-failure", "late":
				if !r.Uncertain {
					t.Fatal("CLI status guessed into payload outcome", r)
				}
			}
			_, e := os.Stat(marker)
			if mode == "late" || mode == "no-result" {
				if !os.IsNotExist(e) {
					t.Fatal("late second gate launched", e)
				}
			} else if e != nil {
				t.Fatal("second gate never crossed", e)
			}
		})
	}
}
