package probe

import (
	"context"
	"io"
	"os"
	"os/exec"
	"sync"
	"syscall"
	"time"
)

// Capture retains a combined stdout/stderr budget, then keeps accepting bytes.
// Streams' interleaving is intentionally unspecified; byte counts are separate.
type Capture struct {
	mu               sync.Mutex
	Out, Err         []byte
	SeenOut, SeenErr int64
	Limit            int
}

func (c *Capture) writer(stderr bool) io.Writer { return captureWriter{c, stderr} }

type captureWriter struct {
	c      *Capture
	stderr bool
}

func (w captureWriter) Write(p []byte) (int, error) {
	c := w.c
	c.mu.Lock()
	defer c.mu.Unlock()
	n := len(p)
	keep := c.Limit - len(c.Out) - len(c.Err)
	if keep < 0 {
		keep = 0
	}
	if keep > n {
		keep = n
	}
	if w.stderr {
		c.SeenErr += int64(n)
		c.Err = append(c.Err, p[:keep]...)
	} else {
		c.SeenOut += int64(n)
		c.Out = append(c.Out, p[:keep]...)
	}
	return n, nil
}

// LocalProcess is ONLY an unprivileged process-group test backend. It is not a
// substitute for cgroups: setsid descendants escape it. No root execution here.
type LocalProcess struct {
	done    chan struct{}
	result  Result
	pid     int
	Capture *Capture
}

func (p *LocalProcess) Done() <-chan struct{} { return p.done }
func (p *LocalProcess) Result() Result        { return p.result }
func (p *LocalProcess) Stop(ctx context.Context) error {
	_ = syscall.Kill(-p.pid, syscall.SIGTERM)
	select {
	case <-p.done:
		return nil
	case <-ctx.Done():
		_ = syscall.Kill(-p.pid, syscall.SIGKILL)
		return ctx.Err() // reaping/drain completion not yet confirmed
	}
}

// StartLocal executes a fixed argv and an explicitly supplied clean environment.
// nil Stdin maps to /dev/null in os/exec. The root production helper must build
// this only from the validated manifest, never from supervisor CLI arguments.
func StartLocal(argv, env []string, cwd string, extra []*os.File, drainGrace time.Duration) (*LocalProcess, error) {
	cmd := exec.Command(argv[0], argv[1:]...)
	cmd.Env = append([]string{}, env...)
	cmd.Dir = cwd
	cmd.ExtraFiles = extra
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
	outR, outW, err := os.Pipe()
	if err != nil {
		return nil, err
	}
	errR, errW, err := os.Pipe()
	if err != nil {
		outR.Close()
		outW.Close()
		return nil, err
	}
	cmd.Stdout = outW
	cmd.Stderr = errW
	if err = cmd.Start(); err != nil {
		outR.Close()
		outW.Close()
		errR.Close()
		errW.Close()
		return nil, err
	}
	outW.Close()
	errW.Close()
	p := &LocalProcess{done: make(chan struct{}), pid: cmd.Process.Pid, Capture: &Capture{Limit: CaptureLimit}}
	var drains sync.WaitGroup
	drains.Add(2)
	go func() { defer drains.Done(); io.Copy(p.Capture.writer(false), outR) }()
	go func() { defer drains.Done(); io.Copy(p.Capture.writer(true), errR) }()
	go func() {
		err := cmd.Wait()
		p.result = Result{cmd.ProcessState.ExitCode(), "exited"}
		if err != nil && p.result.ExitCode < 0 {
			p.result.Reason = "signalled"
		}
		// Main process exit must not leave ordinary process-group descendants behind.
		// This is test cleanup, not production's grace policy or a cgroup guarantee.
		_ = syscall.Kill(-p.pid, syscall.SIGKILL)
		drained := make(chan struct{})
		go func() { drains.Wait(); close(drained) }()
		timer := time.NewTimer(drainGrace)
		select {
		case <-drained:
		case <-timer.C:
		}
		timer.Stop()
		outR.Close()
		errR.Close()
		<-drained
		close(p.done)
	}()
	return p, nil
}

// ScriptFile proves a shell can consume the approved script via fd 3 while
// stdin is /dev/null, without script text in argv. A production alternative is
// a pipe with concurrent bounded writer; a prefilled pipe can deadlock at 64KiB.
// dir MUST be private to the invoking identity. The unlinked file stays open.
func ScriptFile(dir, script string) (*os.File, error) {
	f, err := os.CreateTemp(dir, "script-")
	if err != nil {
		return nil, err
	}
	if err = os.Remove(f.Name()); err != nil {
		f.Close()
		return nil, err
	}
	if _, err = f.WriteString(script); err != nil {
		f.Close()
		return nil, err
	}
	if _, err = f.Seek(0, io.SeekStart); err != nil {
		f.Close()
		return nil, err
	}
	return f, nil
}
