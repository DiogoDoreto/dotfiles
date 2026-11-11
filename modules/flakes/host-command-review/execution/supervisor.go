package execution

import (
	"bytes"
	"context"
	"errors"
	"io"
	"os"
	"os/exec"
	"time"

	"host-command-review/contracts"
)

// Backend is a test seam for transient-service submission. Production uses ONLY
// Systemd. Cancellation may race submission; unknown is an explicit valid result.
type Backend interface {
	Run(context.Context, Config, contracts.ExecutionSpec, []byte, string) Result
}
type Systemd struct {
	command func(context.Context, string, ...string) *exec.Cmd
}

func (b Systemd) cmd(ctx context.Context, path string, args ...string) *exec.Cmd {
	if b.command != nil {
		return b.command(ctx, path, args...)
	}
	return exec.CommandContext(ctx, path, args...)
}

// Supervisor serves one authenticated invocation. CheckRunner MUST be called by
// the immutable entrypoint first. No terminal/126/127 code is a payload result.
func Supervisor(c Config, digest string, root bool, in, out *os.File, b Backend) error {
	if e := c.Validate(); e != nil {
		return e
	}
	f, e := receive(in, Handshake)
	if e != nil || f.Type != "manifest" {
		return errors.New("missing runner manifest")
	}
	s, e := parseManifest(f.Data, digest, c)
	if e != nil {
		return e
	}
	if (s.User == contracts.Root) != root {
		return errors.New("target identity mismatch")
	}
	challenge := nonce()
	if e = send(out, "runner_ready", challenge); e != nil {
		return e
	}
	f, e = receive(in, Handshake)
	if e != nil || f.Type != "start" || !bytes.Equal(f.Data, challenge) {
		return errors.New("runner gate denied")
	}
	ctx, cancel := context.WithTimeout(context.Background(), time.Duration(s.TimeoutSeconds)*time.Second+2*Handshake+10*time.Second)
	defer cancel()
	go func() {
		for {
			f, e := receive(in, Lease)
			if e != nil || f.Type != "lease" || len(f.Data) != 0 {
				cancel()
				return
			}
		}
	}()
	if b == nil {
		b = Systemd{}
	}
	r := b.Run(ctx, c, s, fManifest(s), digest)
	if r.Validate() != nil {
		r = unknown(true)
	}
	return send(out, "result", resultBytes(r))
}
func fManifest(s contracts.ExecutionSpec) []byte { b, _, _ := Manifest(s); return b }

func (backend Systemd) Run(ctx context.Context, c Config, s contracts.ExecutionSpec, manifest []byte, digest string) Result {
	unit := "host-review-" + stringHex(nonce()[:16]) + ".service"
	args, e := c.UnitArgs(s, unit, digest)
	if e != nil {
		return unknown(false)
	}
	if ctx.Err() != nil {
		return unknown(false)
	}
	cmd := backend.cmd(context.Background(), c.SystemdRun, args...)
	cmd.Env = cleanCLIEnv(s.User == contracts.Root)
	cmd.Stderr = io.Discard
	in, iw, e := os.Pipe()
	if e != nil {
		return unknown(false)
	}
	defer in.Close()
	defer iw.Close()
	out, ow, e := os.Pipe()
	if e != nil {
		return unknown(false)
	}
	defer out.Close()
	defer ow.Close()
	cmd.Stdin = in
	cmd.Stdout = ow
	if e = cmd.Start(); e != nil {
		return unknown(false)
	}
	in.Close()
	ow.Close()
	waited := make(chan error, 1)
	go func() { waited <- cmd.Wait() }()
	finished := make(chan struct{})
	// Never assume killing systemd-run stops the service. Stop by unique unit
	// name, close second-gate input first, bound stop, and preserve uncertainty.
	go func() {
		select {
		case <-finished:
			return
		case <-ctx.Done():
			iw.Close()
			stopCtx, cancel := context.WithTimeout(context.Background(), 8*time.Second)
			defer cancel()
			stop := backend.cmd(stopCtx, c.Systemctl, mode(s), "--no-ask-password", "stop", unit)
			stop.Env = cleanCLIEnv(s.User == contracts.Root)
			stop.Stdout = io.Discard
			stop.Stderr = io.Discard
			stop.Run()
			out.Close()
		}
	}()
	defer close(finished)
	transportOK := false
	var result Result
	if e = send(iw, "manifest", manifest); e == nil {
		var f Frame
		f, e = receive(out, Handshake)
		if e == nil && f.Type == "unit_ready" && len(f.Data) == 32 && ctx.Err() == nil {
			if e = send(iw, "unit_start", f.Data); e == nil {
				// Only this goroutine writes helper control. Lease writes have deadlines;
				// no full script/START is prebuffered before the fresh second challenge.
				leaseDone := make(chan struct{})
				leaseExited := make(chan struct{})
				go func() {
					defer close(leaseExited)
					ticker := time.NewTicker(Renew)
					defer ticker.Stop()
					for {
						select {
						case <-leaseDone:
							return
						case <-ctx.Done():
							return
						case <-ticker.C:
							if send(iw, "lease", nil) != nil {
								return
							}
						}
					}
				}()
				f, e = receive(out, time.Duration(s.TimeoutSeconds)*time.Second+Handshake+8*time.Second)
				close(leaseDone)
				<-leaseExited
				if e == nil && f.Type == "result" {
					result, e = parseResult(f.Data)
					transportOK = e == nil
				}
			}
		}
	}
	iw.Close()
	// If protocol fails before context cancellation, stopping is still mandatory.
	// A separate finite stop is safe to repeat for this never-reused unit name.
	var waitErr error
	select {
	case waitErr = <-waited:
	case <-time.After(8 * time.Second):
		stopCtx, cancel := context.WithTimeout(context.Background(), 8*time.Second)
		stop := backend.cmd(stopCtx, c.Systemctl, mode(s), "--no-ask-password", "stop", unit)
		stop.Env = cleanCLIEnv(s.User == contracts.Root)
		stop.Stdout = io.Discard
		stop.Stderr = io.Discard
		stop.Run()
		cancel()
		cmd.Process.Kill()
		select {
		case <-waited:
		case <-time.After(time.Second):
		}
		return unknown(true)
	}
	// --wait completion PLUS the helper's framed terminal is required. Nonzero
	// CLI status or missing helper frame is never guessed into a payload exit.
	if !transportOK || waitErr != nil {
		stopCtx, cancel := context.WithTimeout(context.Background(), 8*time.Second)
		stop := backend.cmd(stopCtx, c.Systemctl, mode(s), "--no-ask-password", "stop", unit)
		stop.Env = cleanCLIEnv(s.User == contracts.Root)
		stop.Stdout = io.Discard
		stop.Stderr = io.Discard
		stop.Run()
		cancel()
		return unknown(true)
	}
	// Reject trailing helper protocol, but do not wait on an unbounded pipe.
	out.SetReadDeadline(time.Now().Add(time.Second))
	var tail [1]byte
	n, tailErr := out.Read(tail[:])
	if n != 0 || tailErr != io.EOF {
		return unknown(true)
	}
	return result
}
