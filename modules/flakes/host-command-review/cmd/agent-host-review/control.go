package main

import (
	"errors"
	"io"
	"net"
	"os"
	"syscall"
	"time"

	"host-command-review/host"
)

func sameUser(c *net.UnixConn) bool {
	raw, err := c.SyscallConn()
	if err != nil {
		return false
	}
	var cred *syscall.Ucred
	err = raw.Control(func(fd uintptr) { cred, _ = syscall.GetsockoptUcred(int(fd), syscall.SOL_SOCKET, syscall.SO_PEERCRED) })
	return err == nil && cred != nil && cred.Uid == uint32(os.Getuid())
}

// controlLock validates all runtime ancestors using the same private-directory
// walker as the ledger. Stable nofollow lock serializes simultaneous launches.
func controlLock(dir string) (*host.Store, *os.File, bool, error) {
	s, err := host.OpenStore(dir, nil)
	if err != nil {
		return nil, nil, false, err
	}
	fd, err := syscall.Open(dir+"/application.lock", syscall.O_CREAT|syscall.O_RDWR|syscall.O_NOFOLLOW|syscall.O_CLOEXEC, 0600)
	if err != nil {
		s.Close()
		return nil, nil, false, err
	}
	f := os.NewFile(uintptr(fd), "application.lock")
	st, err := f.Stat()
	if err != nil || !st.Mode().IsRegular() || st.Mode().Perm() != 0600 || st.Sys().(*syscall.Stat_t).Uid != uint32(os.Getuid()) || st.Sys().(*syscall.Stat_t).Nlink != 1 {
		f.Close()
		s.Close()
		return nil, nil, false, errors.New("unsafe application lock")
	}
	err = syscall.Flock(fd, syscall.LOCK_EX|syscall.LOCK_NB)
	if err == syscall.EWOULDBLOCK {
		return s, f, false, nil
	}
	if err != nil {
		f.Close()
		s.Close()
		return nil, nil, false, err
	}
	return s, f, true, nil
}
func controlOpen(path string) (string, error) {
	// Owner may still be binding during a simultaneous activation.
	var c *net.UnixConn
	var err error
	for i := 0; i < 30; i++ {
		c, err = net.DialUnix("unix", nil, &net.UnixAddr{Name: path, Net: "unix"})
		if err == nil {
			break
		}
		time.Sleep(100 * time.Millisecond)
	}
	if err != nil {
		return "", err
	}
	defer c.Close()
	_ = c.SetDeadline(time.Now().Add(6 * time.Second))
	if !sameUser(c) {
		return "", errors.New("unsafe control peer")
	}
	if _, err = c.Write([]byte("open\n")); err != nil {
		return "", err
	}
	_ = c.CloseWrite()
	b, err := io.ReadAll(io.LimitReader(c, 513))
	if err != nil || len(b) > 512 || len(b) == 0 {
		return "", errors.New("control activation failed")
	}
	return string(b), nil
}
func controlServe(l *net.UnixListener, bootstrap func() (string, error)) {
	for {
		c, err := l.AcceptUnix()
		if err != nil {
			return
		}
		// Serialized, bounded local requests; never spawn unbounded goroutines.
		_ = c.SetDeadline(time.Now().Add(6 * time.Second))
		if sameUser(c) {
			b, err := io.ReadAll(io.LimitReader(c, 17))
			if err == nil && string(b) == "open\n" {
				if url, err := bootstrap(); err == nil {
					_, _ = io.WriteString(c, url)
				}
			}
		}
		_ = c.Close()
	}
}
