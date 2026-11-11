package main

import (
	"context"
	"crypto/sha256"
	"encoding/base64"
	"host-command-review/contracts"
	"host-command-review/host"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

func private(t *testing.T) string {
	t.Helper()
	p, err := os.MkdirTemp(".", ".launcher-test-")
	if err != nil {
		t.Fatal(err)
	}
	p, err = filepath.Abs(p)
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { os.RemoveAll(p) })
	return p
}
func executable(t *testing.T, body string) string {
	t.Helper()
	p := filepath.Join(private(t), "tool")
	if err := os.WriteFile(p, []byte("#!/bin/sh\n"+body+"\n"), 0700); err != nil {
		t.Fatal(err)
	}
	return p
}
func TestCleanupSkipsLiveReviewerAndDeletesCrashRaw(t *testing.T) {
	path := private(t)
	trustedShell := "/nix/store/0123456789abcdfghijklmnpqrsvwxyz-bash/bin/bash"
	s, err := host.OpenStore(path, nil)
	if err != nil {
		t.Fatal(err)
	}
	defer s.Close()
	c, err := host.NewController(s, host.Config{Shell: trustedShell})
	if err != nil {
		t.Fatal(err)
	}
	defer c.CloseUnclean()
	q := contracts.Request{Version: 1, Operation: contracts.Submit, SessionID: c.SessionID(), RequestID: strings.Repeat("a", 64), Proposal: &contracts.Proposal{Script: "echo PRIVATE", Cwd: "/", User: contracts.Dog, TimeoutSeconds: 120}}
	r, err := c.Import(q)
	if err != nil {
		t.Fatal(err)
	}
	before, err := c.Reviews()
	if err != nil {
		t.Fatal(err)
	}
	if err = cleanupStore(path, trustedShell); err != nil {
		t.Fatal(err)
	}
	after, err := c.Reviews()
	if err != nil || before[0].Binding != after[0].Binding {
		t.Fatalf("cleanup interfered with live reviewer: %v", err)
	}
	if err = c.Heartbeat(); err != nil {
		t.Fatal(err)
	}
	binding, err := c.Approve(r.Snapshot.SnapshotID, r.Revision, r.Digest)
	if err != nil {
		t.Fatal(err)
	}
	spec, err := c.Consume(binding)
	if err != nil {
		t.Fatal(err)
	}
	if err = c.Reconcile(binding.SnapshotID, contracts.Started, nil); err != nil {
		t.Fatal(err)
	}
	artifact := contracts.Artifact{Version: 1, ArtifactID: strings.Repeat("b", 64), SnapshotID: binding.SnapshotID, Revision: binding.Revision, ExecutionDigest: binding.Digest, Executed: spec, Stdout: []byte("PRIVATE"), Stderr: []byte{}, Exit: contracts.ExitInfo{Kind: "exited", Code: 0}, RedactedFields: []string{}}
	if err = c.Reconcile(binding.SnapshotID, contracts.Finish, &artifact); err != nil {
		t.Fatal(err)
	}
	if err = c.CloseUnclean(); err != nil {
		t.Fatal(err)
	}
	if err = cleanupStore(path, trustedShell); err != nil {
		t.Fatal(err)
	}
	files, err := os.ReadDir(filepath.Join(path, "raw"))
	if err != nil || len(files) != 0 {
		t.Fatalf("cleanup retained crash raw: %v %v", files, err)
	}
}

func TestEnrollmentRequiresIndependentFingerprint(t *testing.T) {
	wire := []byte("\x00\x00\x00\x0bssh-ed25519\x00\x00\x00\x20" + strings.Repeat("k", 32))
	key := base64.StdEncoding.EncodeToString(wire)
	sum := sha256.Sum256(wire)
	fingerprint := "SHA256:" + base64.RawStdEncoding.EncodeToString(sum[:])
	if got, err := enrollmentKey(strings.NewReader("ssh-ed25519 "+key+" guest-host-key\n"), fingerprint); err != nil || got != key {
		t.Fatalf("valid enrollment: %s %v", got, err)
	}
	for _, public := range []string{"ssh-rsa " + key, "ssh-ed25519 invalid", "ssh-ed25519 " + base64.StdEncoding.EncodeToString(wire[:50]), strings.Repeat("x", 4097)} {
		if _, err := enrollmentKey(strings.NewReader(public), fingerprint); err == nil {
			t.Fatal("invalid public key accepted")
		}
	}
	if _, err := enrollmentKey(strings.NewReader("ssh-ed25519 "+key), "SHA256:untrusted"); err == nil {
		t.Fatal("enrollment trusted supplied key without independent fingerprint")
	}
}

func TestDesktopAdmission(t *testing.T) {
	old := loginctl
	t.Cleanup(func() { loginctl = old })
	t.Setenv("XDG_SESSION_ID", "test-session")
	for _, tt := range []struct {
		name, active, remote, desktop string
		allowed                       bool
	}{
		{"dog", "yes", "no", "KDE", true}, {"dog", "yes", "no", "plasma", true},
		{"agent", "yes", "no", "KDE", false}, {"dog", "no", "no", "KDE", false},
		{"dog", "yes", "yes", "KDE", false}, {"dog", "yes", "no", "GNOME", false},
	} {
		// Match loginctl's successful-but-empty result when no requested property
		// name matches (including a comma-separated list passed as one name).
		loginctl = executable(t, `[ "$*" = "show-session test-session --property=Name --property=Active --property=Remote --property=Type --property=Class --property=Desktop" ] || exit 0
`+"printf 'Name="+tt.name+"\\nActive="+tt.active+"\\nRemote="+tt.remote+"\\nType=wayland\\nClass=user\\nDesktop="+tt.desktop+"\\n'")
		if err := desktop(context.Background()); (err == nil) != tt.allowed {
			t.Fatalf("%+v: %v", tt, err)
		}
	}
	t.Setenv("XDG_SESSION_ID", "")
	if err := desktop(context.Background()); err == nil {
		t.Fatal("missing desktop admitted")
	}
}
func TestSleepMonitorInvalidatesAndJoinsChild(t *testing.T) {
	old := dbusMonitor
	t.Cleanup(func() { dbusMonitor = old })
	// Test subprocess only: no actual D-Bus connection or system suspend.
	dbusMonitor = executable(t, "printf 'signal time=0 sender=test -> destination=test\\n'\nexec sleep 60")
	s, err := host.OpenStore(private(t), nil)
	if err != nil {
		t.Fatal(err)
	}
	defer s.Close()
	c, err := host.NewController(s, host.Config{Shell: "/nix/store/0123456789abcdfghijklmnpqrsvwxyz-bash/bin/bash"})
	if err != nil {
		t.Fatal(err)
	}
	defer c.CloseUnclean()
	q := contracts.Request{Version: 1, Operation: contracts.Submit, SessionID: c.SessionID(), RequestID: "1111111111111111111111111111111111111111111111111111111111111111", Proposal: &contracts.Proposal{Script: "true", Cwd: "/", User: contracts.Dog, TimeoutSeconds: 120}}
	if _, err = c.Import(q); err != nil {
		t.Fatal(err)
	}
	before, err := c.Reviews()
	if err != nil {
		t.Fatal(err)
	}
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	errs, ready, done := monitor(ctx, c)
	select {
	case <-ready:
	case err := <-errs:
		t.Fatal(err)
	case <-time.After(2 * time.Second):
		t.Fatal("monitor not ready")
	}
	cancel()
	select {
	case <-done:
	case <-time.After(2 * time.Second):
		t.Fatal("monitor child was not reaped")
	}
	if err := <-errs; err == nil {
		t.Fatal("monitor loss must fail closed")
	}
	after, err := c.Reviews()
	if err != nil {
		t.Fatal(err)
	}
	if after[0].Binding.Epoch <= before[0].Binding.Epoch {
		t.Fatal("sleep signal did not invalidate approval epoch")
	}
}
