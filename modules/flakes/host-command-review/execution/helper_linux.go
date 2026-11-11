package execution

import (
	"bytes"
	"context"
	"errors"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"sync"
	"syscall"
	"time"

	"host-command-review/contracts"
)

// UnitHelper repeats manifest and identity validation, then demands a fresh
// second challenge response. It must only be installed at Config.Helper.
func UnitHelper(c Config, digest string, in, out *os.File) error {
	f, e := receive(in, Handshake)
	if e != nil || f.Type != "manifest" {
		return errors.New("missing unit manifest")
	}
	s, e := parseManifest(f.Data, digest, c)
	if e != nil {
		return e
	}
	if e = c.checkHelper(s); e != nil {
		return e
	}
	challenge := nonce()
	if e = send(out, "unit_ready", challenge); e != nil {
		return e
	}
	f, e = receive(in, Handshake)
	if e != nil || f.Type != "unit_start" || !bytes.Equal(f.Data, challenge) {
		return errors.New("unit start gate denied")
	}
	ctx, cancel := context.WithTimeout(context.Background(), time.Duration(s.TimeoutSeconds)*time.Second)
	defer cancel()
	// EOF and partial frames do not renew the lease. A broken control channel
	// stops the helper, so systemd deactivation cleans the entire cgroup.
	go func() {
		for {
			f, e := receive(in, Lease)
			if e != nil || f.Type != "lease" || len(f.Data) != 0 {
				cancel()
				return
			}
		}
	}()
	r := runPayload(ctx, c, s)
	if e = send(out, "result", resultBytes(r)); e != nil {
		return e
	}
	return nil
}

type capture struct {
	mu             sync.Mutex
	stdout, stderr []byte
	truncated      bool
}
type captureWriter struct {
	c         *capture
	errStream bool
}

func (w captureWriter) Write(p []byte) (int, error) {
	w.c.mu.Lock()
	defer w.c.mu.Unlock()
	n := len(p)
	room := contracts.CaptureBytes - len(w.c.stdout) - len(w.c.stderr)
	keep := min(room, n)
	if w.errStream {
		w.c.stderr = append(w.c.stderr, p[:keep]...)
	} else {
		w.c.stdout = append(w.c.stdout, p[:keep]...)
	}
	if keep < n {
		w.c.truncated = true
	}
	return n, nil
}
func privateScript(dir, script string) (*os.File, error) {
	// Traverse via FDs, refusing symlinks and writable-by-others ancestors.
	fd, e := syscall.Open("/", syscall.O_RDONLY|syscall.O_DIRECTORY|syscall.O_CLOEXEC, 0)
	if e != nil {
		return nil, e
	}
	for _, part := range strings.Split(strings.TrimPrefix(filepath.Clean(dir), "/"), "/") {
		next, err := syscall.Openat(fd, part, syscall.O_RDONLY|syscall.O_DIRECTORY|syscall.O_NOFOLLOW|syscall.O_CLOEXEC, 0)
		syscall.Close(fd)
		if err != nil {
			return nil, err
		}
		fd = next
		var st syscall.Stat_t
		if err = syscall.Fstat(fd, &st); err != nil || st.Mode&0022 != 0 || (st.Uid != 0 && st.Uid != uint32(os.Geteuid())) {
			syscall.Close(fd)
			return nil, errors.New("unsafe runtime ancestor")
		}
	}
	defer syscall.Close(fd)
	var st syscall.Stat_t
	if e = syscall.Fstat(fd, &st); e != nil || st.Uid != uint32(os.Geteuid()) || st.Mode&0077 != 0 {
		return nil, errors.New("unsafe runtime directory")
	}
	name := ".script-" + stringHex(nonce())
	n, e := syscall.Openat(fd, name, syscall.O_RDWR|syscall.O_CREAT|syscall.O_EXCL|syscall.O_NOFOLLOW|syscall.O_CLOEXEC, 0600)
	if e != nil {
		return nil, e
	}
	f := os.NewFile(uintptr(n), "anonymous-script")
	if e = syscall.Unlinkat(fd, name); e != nil {
		f.Close()
		return nil, e
	}
	if _, e = f.WriteString(script); e != nil {
		f.Close()
		return nil, e
	}
	if _, e = f.Seek(0, io.SeekStart); e != nil {
		f.Close()
		return nil, e
	}
	return f, nil
}
func stringHex(b []byte) string {
	const h = "0123456789abcdef"
	v := make([]byte, 2*len(b))
	for i, x := range b {
		v[2*i] = h[x>>4]
		v[2*i+1] = h[x&15]
	}
	return string(v)
}

// runPayload is ONLY called inside the transient service, never a host fallback.
// Main-process cancellation is complemented by systemd's cgroup deactivation,
// five-second grace and independent RuntimeMaxSec, not a process-group promise.
func runPayload(ctx context.Context, c Config, s contracts.ExecutionSpec) Result {
	script, e := privateScript(c.RuntimeDir, s.Script)
	if e != nil {
		return unknown(false)
	}
	defer script.Close()
	devnull, e := os.Open("/dev/null")
	if e != nil {
		return unknown(false)
	}
	defer devnull.Close()
	cmd := exec.Command(s.Shell, "--noprofile", "--norc", "/proc/self/fd/3")
	cmd.Dir = s.Cwd
	cmd.Env = []string{"PATH=" + s.Environment.PATH, "HOME=" + s.Environment.HOME, "LANG=" + s.Environment.LANG}
	cmd.Stdin = devnull
	cmd.ExtraFiles = []*os.File{script}
	// Own the pipe drainers: os/exec's Wait must not close them before they drain.
	stdout, ow, e := os.Pipe()
	if e != nil {
		return unknown(false)
	}
	defer stdout.Close()
	defer ow.Close()
	stderr, ew, e := os.Pipe()
	if e != nil {
		return unknown(false)
	}
	defer stderr.Close()
	defer ew.Close()
	cmd.Stdout = ow
	cmd.Stderr = ew
	if e = cmd.Start(); e != nil {
		return unknown(false)
	}
	ow.Close()
	ew.Close()
	cap := &capture{stdout: []byte{}, stderr: []byte{}}
	var wg sync.WaitGroup
	wg.Add(2)
	go func() { defer wg.Done(); io.Copy(captureWriter{cap, false}, stdout) }()
	go func() { defer wg.Done(); io.Copy(captureWriter{cap, true}, stderr) }()
	done := make(chan error, 1)
	go func() { done <- cmd.Wait() }()
	cancelled := false
	select {
	case <-done:
	case <-ctx.Done():
		cancelled = true
		cmd.Process.Kill()
		select {
		case <-done:
		case <-time.After(time.Second):
			return unknown(true)
		}
	}
	drained := make(chan struct{})
	go func() { wg.Wait(); close(drained) }()
	select {
	case <-drained:
	case <-time.After(time.Second):
		stdout.Close()
		stderr.Close()
		<-drained
		cap.mu.Lock()
		cap.truncated = true
		cap.mu.Unlock()
	}
	cap.mu.Lock()
	r := Result{Started: true, Stdout: cap.stdout, Stderr: cap.stderr, Truncated: cap.truncated}
	cap.mu.Unlock()
	if cancelled {
		r.Exit = contracts.ExitInfo{Kind: "cancelled", Code: -1}
		if errors.Is(ctx.Err(), context.DeadlineExceeded) {
			r.Exit.Kind = "timed_out"
		}
		return r
	}
	st, ok := cmd.ProcessState.Sys().(syscall.WaitStatus)
	if !ok {
		return unknown(true)
	}
	if st.Signaled() {
		r.Exit = contracts.ExitInfo{Kind: "signaled", Code: -1, Signal: int(st.Signal())}
	} else {
		r.Exit = contracts.ExitInfo{Kind: "exited", Code: st.ExitStatus()}
	}
	return r
}
