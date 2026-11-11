package execution

import (
	"context"
	"errors"
	"io"
	"os"
	"os/exec"
	"sync/atomic"
	"time"

	"host-command-review/contracts"
)

const AuthTimeout = 60 * time.Second

// Report is host-only. AuthCancelled is known pkexec 126 before RUNNER_READY;
// all other missing terminal frames remain uncertain, including pre-ready 127.
type Report struct {
	Ready         bool
	AuthCancelled bool
	Result        Result
}

// Execute must only be called after a successful durable Controller.Consume.
// ctx cancellation requests stop; it never promises to reverse side effects.
func Execute(ctx context.Context, c Config, s contracts.ExecutionSpec) Report {
	report := Report{Result: unknown(false)}
	if c.Validate() != nil || os.Getuid() != c.ExpectedUID || os.Geteuid() != c.ExpectedUID {
		return report
	}
	manifest, digest, e := Manifest(s)
	if e != nil || s.Shell != c.Shell {
		return report
	}
	path := c.Runner
	args := []string{"--user", "--approved-sha256", digest}
	if s.User == contracts.Root {
		path, args, e = c.PKExecArgs(digest)
		if e != nil {
			return report
		}
	}
	if ctx.Err() != nil {
		return report
	}
	cmd := exec.Command(path, args...)
	cmd.Env = cleanCLIEnv(false)
	cmd.Stderr = io.Discard
	in, iw, e := os.Pipe()
	if e != nil {
		return report
	}
	defer in.Close()
	defer iw.Close()
	out, ow, e := os.Pipe()
	if e != nil {
		return report
	}
	defer out.Close()
	defer ow.Close()
	cmd.Stdin = in
	cmd.Stdout = ow
	if e = cmd.Start(); e != nil {
		return report
	}
	in.Close()
	ow.Close()
	lifetime, cancel := context.WithTimeout(ctx, AuthTimeout+time.Duration(s.TimeoutSeconds)*time.Second+30*time.Second)
	defer cancel()
	finished := make(chan struct{})
	defer close(finished)
	var ready atomic.Bool
	authTimer := time.AfterFunc(AuthTimeout, func() {
		if !ready.Load() {
			iw.Close()
			cmd.Process.Kill()
		}
	})
	defer authTimer.Stop()
	go func() {
		select {
		case <-finished:
			return
		case <-lifetime.Done():
			iw.Close()
			if !ready.Load() {
				cmd.Process.Kill()
				return
			}
			select {
			case <-finished:
				return
			case <-time.After(20 * time.Second):
				cmd.Process.Kill()
			}
		}
	}()
	iw.SetWriteDeadline(time.Now().Add(AuthTimeout))
	if e = WriteFrame(iw, "manifest", manifest); e == nil {
		var f Frame
		f, e = receive(out, AuthTimeout)
		if e == nil && f.Type == "runner_ready" && len(f.Data) == 32 && lifetime.Err() == nil {
			report.Ready = true
			ready.Store(true)
			if e = send(iw, "start", f.Data); e == nil {
				stopLease := make(chan struct{})
				exited := make(chan struct{})
				go func() {
					defer close(exited)
					ticker := time.NewTicker(Renew)
					defer ticker.Stop()
					for {
						select {
						case <-stopLease:
							return
						case <-lifetime.Done():
							return
						case <-ticker.C:
							if send(iw, "lease", nil) != nil {
								return
							}
						}
					}
				}()
				f, e = receive(out, time.Duration(s.TimeoutSeconds)*time.Second+30*time.Second)
				close(stopLease)
				<-exited
				if e == nil && f.Type == "result" {
					r, err := parseResult(f.Data)
					if err == nil {
						report.Result = r
					} else {
						report.Result = unknown(true)
					}
				} else {
					report.Result = unknown(true)
				}
			}
		}
	}
	iw.Close()
	// Bound reaping even if authentication/transport stalls. Killing pkexec cannot
	// prove a submitted unit stopped; report stays uncertain without terminal.
	waited := make(chan error, 1)
	go func() { waited <- cmd.Wait() }()
	var waitErr error
	select {
	case waitErr = <-waited:
	case <-time.After(20 * time.Second):
		cmd.Process.Kill()
		select {
		case <-waited:
		case <-time.After(time.Second):
		}
		report.Result = unknown(report.Ready)
		return report
	}
	if !report.Ready && s.User == contracts.Root {
		var exit *exec.ExitError
		if errors.As(waitErr, &exit) && exit.ExitCode() == 126 {
			report.AuthCancelled = true
		}
	}
	if report.Ready && waitErr != nil {
		report.Result = unknown(true)
	}
	if !report.Result.Uncertain {
		out.SetReadDeadline(time.Now().Add(time.Second))
		var tail [1]byte
		n, e := out.Read(tail[:])
		if n != 0 || e != io.EOF {
			report.Result = unknown(report.Ready)
		}
	}
	return report
}
