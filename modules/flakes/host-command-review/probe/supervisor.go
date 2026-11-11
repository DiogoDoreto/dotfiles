package probe

import (
	"context"
	"crypto/rand"
	"encoding/hex"
	"errors"
	"io"
	"time"
)

const DefaultRuntime = 2 * time.Minute
const MaxRuntime = 30 * time.Minute
const StopGrace = 5 * time.Second

type Result struct {
	ExitCode int
	Reason   string
}
type Execution interface {
	Done() <-chan struct{}
	Result() Result             // only after Done
	Stop(context.Context) error // must honor context, including failed start races
}
type Backend interface {
	// Start must honor ctx and leave no resources on an error. A successfully
	// created execution must be returned even if cancellation raced creation, so
	// Stop can clean it up. The manifest must already pass the production
	// validator. This interface is NOT itself an authorization boundary.
	Start(context.Context, []byte) (Execution, error)
}
type Options struct {
	Runtime   time.Duration
	Handshake time.Duration
	Lease     time.Duration
	Grace     time.Duration
}

func (o Options) valid() bool {
	return o.Runtime > 0 && o.Runtime <= MaxRuntime && o.Handshake > 0 && o.Lease > 0 && o.Grace > 0 && o.Grace <= StopGrace
}

type incoming struct {
	f   Frame
	err error
}

// Supervise models a post-pkexec runner. Output here is ONLY the ready challenge;
// production needs a serialized output-frame writer for captures and final status.
// Closing input must interrupt Read (an os.File pipe or socket satisfies this).
// Emit must be context-bounded by its caller; a blocked reviewer output pipe must
// never block the lifecycle goroutine. Tests use an in-memory/buffered emitter.
func Supervise(ctx context.Context, input io.ReadCloser, emit func(Frame) error, approved string, o Options, b Backend) Result {
	if !o.valid() {
		input.Close()
		return Result{-1, "invalid-options"}
	}
	readCtx, cancel := context.WithCancel(ctx)
	defer cancel()
	defer input.Close()
	frames := make(chan incoming, 1)
	go func() {
		for {
			f, err := ReadFrame(input)
			select {
			case frames <- incoming{f, err}:
			case <-readCtx.Done():
				return
			}
			if err != nil {
				return
			}
		}
	}()
	handshake := time.NewTimer(o.Handshake)
	defer handshake.Stop()
	next := func() (Frame, error) {
		select {
		case <-ctx.Done():
			return Frame{}, ctx.Err()
		case <-handshake.C:
			return Frame{}, errors.New("handshake timeout")
		case v := <-frames:
			return v.f, v.err
		}
	}
	f, err := next()
	if err != nil || f.Type != Manifest || VerifyApproved(f.Body, approved) != nil {
		return Result{-1, "manifest-rejected"}
	}
	raw := f.Body
	nonce := make([]byte, 32)
	if _, err = rand.Read(nonce); err != nil {
		return Result{-1, "random-failed"}
	}
	challenge := []byte(hex.EncodeToString(nonce))
	if err = emit(Frame{Ready, challenge}); err != nil {
		return Result{-1, "reviewer-unavailable"}
	}
	f, err = next()
	if err != nil || f.Type != Start || string(f.Body) != string(challenge) {
		return Result{-1, "start-rejected"}
	}
	// The challenge response proves control-peer participation after auth, NOT a
	// race-free assertion that its PID is alive at the next instruction. Lease/EOF
	// handles death after this point; process identity must not rely on PID alone.
	runCtx, runCancel := context.WithTimeout(ctx, o.Runtime)
	defer runCancel()
	// Do not create a unit if EOF/cancel was already observed. A concurrent death
	// can still race Start; all backends need an independent runtime limit.
	select {
	case v := <-frames:
		if v.err != nil || v.f.Type != Lease || len(v.f.Body) != 0 {
			return Result{-1, "pre-start-disconnect"}
		}
	default:
	}
	type started struct {
		ex  Execution
		err error
	}
	created := make(chan started, 1)
	go func() { ex, err := b.Start(runCtx, raw); created <- started{ex, err} }()
	var ex Execution
	var done <-chan struct{}
	lease := time.NewTimer(o.Lease)
	defer lease.Stop()
	stop := func(reason string) Result {
		runCancel()
		stopCtx, stopCancel := context.WithTimeout(context.Background(), o.Grace)
		defer stopCancel()
		if ex == nil {
			select {
			case v := <-created:
				if v.err != nil {
					return Result{-1, reason}
				}
				ex = v.ex
			case <-stopCtx.Done():
				return Result{-1, reason + ":start-unconfirmed"}
			}
		}
		if err := ex.Stop(stopCtx); err != nil {
			return Result{-1, reason + ":stop-unconfirmed"}
		}
		return Result{-1, reason}
	}
	for {
		select {
		case v := <-created:
			if v.err != nil {
				return Result{-1, "start-failed"}
			}
			ex, done = v.ex, v.ex.Done()
		case <-done:
			return ex.Result()
		case <-runCtx.Done():
			if ctx.Err() != nil {
				return stop("cancelled")
			}
			return stop("timeout")
		case <-lease.C:
			return stop("lease-expired")
		case v := <-frames:
			if v.err != nil {
				return stop("reviewer-eof")
			}
			if v.f.Type == Cancel && len(v.f.Body) == 0 {
				return stop("cancelled")
			}
			if v.f.Type != Lease || len(v.f.Body) != 0 {
				return stop("protocol-error")
			}
			if !lease.Stop() {
				select {
				case <-lease.C:
				default:
				}
			}
			lease.Reset(o.Lease)
		}
	}
}
