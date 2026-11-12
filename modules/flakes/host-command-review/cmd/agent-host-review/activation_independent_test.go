package main

import (
	"errors"
	"net"
	"os"
	"path/filepath"
	"strings"
	"sync/atomic"
	"testing"
	"time"
)

func TestIndependentControlRecoversAfterMalformedAndFailedActivation(t *testing.T) {
	// Keep Unix socket paths below sun_path's limit without using /tmp.
	dir, err := os.MkdirTemp(".", ".independent-control-")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(dir)
	path := filepath.Join(dir, "control.sock")
	l, err := net.ListenUnix("unix", &net.UnixAddr{Name: path, Net: "unix"})
	if err != nil {
		t.Fatal(err)
	}
	var calls atomic.Int32
	done := make(chan struct{})
	go func() {
		defer close(done)
		controlServe(l, func() (string, error) {
			if calls.Add(1) == 1 {
				return "", errors.New("bootstrap temporarily unavailable")
			}
			return "http://127.0.0.1:1234/#fresh", nil
		})
	}()
	defer func() { l.Close(); <-done }()
	for _, input := range []string{"", "open", "open\nextra", strings.Repeat("x", 1000), "OPEN\n"} {
		c, err := net.DialUnix("unix", nil, &net.UnixAddr{Name: path, Net: "unix"})
		if err != nil {
			t.Fatal(err)
		}
		_ = c.SetDeadline(time.Now().Add(time.Second))
		_, _ = c.Write([]byte(input))
		_ = c.CloseWrite()
		var b [1]byte
		n, _ := c.Read(b[:])
		c.Close()
		if n != 0 {
			t.Fatalf("malformed control request produced credential: %q", input)
		}
	}
	if calls.Load() != 0 {
		t.Fatal("malformed request invoked bootstrap")
	}
	if url, err := controlOpen(path); err == nil || url != "" {
		t.Fatalf("failed activation returned URL: %q %v", url, err)
	}
	if url, err := controlOpen(path); err != nil || url != "http://127.0.0.1:1234/#fresh" {
		t.Fatalf("next activation did not recover: %q %v", url, err)
	}
	if calls.Load() != 2 {
		t.Fatalf("unexpected bootstrap count: %d", calls.Load())
	}
}

func TestIndependentControlLockRejectsUnsafeExistingInodes(t *testing.T) {
	for _, kind := range []string{"symlink", "hardlink", "world-readable"} {
		t.Run(kind, func(t *testing.T) {
			dir, err := os.MkdirTemp(".", ".independent-lock-")
			if err != nil {
				t.Fatal(err)
			}
			defer os.RemoveAll(dir)
			dir, err = filepath.Abs(dir)
			if err != nil {
				t.Fatal(err)
			}
			target := filepath.Join(dir, "other")
			lock := filepath.Join(dir, "application.lock")
			if err := os.WriteFile(target, []byte{}, 0600); err != nil {
				t.Fatal(err)
			}
			switch kind {
			case "symlink":
				err = os.Symlink(target, lock)
			case "hardlink":
				err = os.Link(target, lock)
			case "world-readable":
				err = os.WriteFile(lock, []byte{}, 0600)
				if err == nil {
					err = os.Chmod(lock, 0644)
				}
			}
			if err != nil {
				t.Fatal(err)
			}
			s, f, owner, err := controlLock(dir)
			if s != nil {
				s.Close()
			}
			if f != nil {
				f.Close()
			}
			if err == nil || owner {
				t.Fatalf("unsafe %s accepted: owner=%v err=%v", kind, owner, err)
			}
		})
	}
}
