package main

import (
	"net"
	"os"
	"path/filepath"
	"sync/atomic"
	"testing"
)

func TestRepeatedActivationUsesExistingControlOwner(t *testing.T) {
	dir, err := os.MkdirTemp(".", ".control-")
	if err != nil {
		t.Fatal(err)
	}
	dir, _ = filepath.Abs(dir)
	defer os.RemoveAll(dir)
	s, lock, owner, err := controlLock(dir)
	if err != nil || !owner {
		t.Fatal(owner, err)
	}
	defer s.Close()
	defer lock.Close()
	s2, lock2, owner2, err := controlLock(dir)
	if err != nil || owner2 {
		t.Fatal("second owner", owner2, err)
	}
	defer s2.Close()
	defer lock2.Close()
	cwd, _ := os.Getwd()
	relative, _ := filepath.Rel(cwd, dir)
	path := filepath.Join(relative, "control.sock")
	l, err := net.ListenUnix("unix", &net.UnixAddr{Name: path, Net: "unix"})
	if err != nil {
		t.Fatal(err)
	}
	var opens atomic.Int32
	done := make(chan struct{})
	go func() {
		defer close(done)
		controlServe(l, func() (string, error) { opens.Add(1); return "http://127.0.0.1:1234/#secret", nil })
	}()
	defer func() { l.Close(); <-done }()
	for i := 0; i < 2; i++ {
		url, err := controlOpen(path)
		if err != nil || url != "http://127.0.0.1:1234/#secret" {
			t.Fatal(url, err)
		}
	}
	if opens.Load() != 2 {
		t.Fatal(opens.Load())
	}
	c, err := net.DialUnix("unix", nil, &net.UnixAddr{Name: path, Net: "unix"})
	if err != nil {
		t.Fatal(err)
	}
	_, _ = c.Write([]byte("open extra\n"))
	_ = c.CloseWrite()
	var b [10]byte
	n, _ := c.Read(b[:])
	c.Close()
	if n != 0 || opens.Load() != 2 {
		t.Fatal("unbounded command accepted")
	}
}
