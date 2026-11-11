package probe

import (
	"bytes"
	"context"
	"errors"
	"io"
	"testing"
	"time"
)

type startingBackend struct {
	entered chan struct{}
	stopped chan struct{}
	ex      *fakeExec
}

func (b startingBackend) Start(ctx context.Context, _ []byte) (Execution, error) {
	close(b.entered)
	<-ctx.Done()
	close(b.stopped)
	// Deliberately simulate success racing cancellation: unit exists after cancel.
	return b.ex, nil
}
func TestCancellationDuringSubmission(t *testing.T) {
	r, w := io.Pipe()
	defer w.Close()
	ready := make(chan Frame, 1)
	done := make(chan Result, 1)
	b := startingBackend{make(chan struct{}), make(chan struct{}), newFake().ex}
	go func() {
		done <- Supervise(context.Background(), r, func(f Frame) error { ready <- f; return nil }, digest([]byte("ok")), opts(), b)
	}()
	send(t, w, Frame{Manifest, []byte("ok")})
	f := <-ready
	send(t, w, Frame{Start, f.Body})
	<-b.entered
	send(t, w, Frame{Cancel, nil})
	select {
	case got := <-done:
		if got.Reason != "cancelled" || b.ex.stops != 1 {
			t.Fatal(got, b.ex.stops)
		}
	case <-time.After(time.Second):
		t.Fatal("submission blocked cancellation")
	}
	<-b.stopped
}

type failingStop struct{ *fakeExec }

func (e failingStop) Stop(context.Context) error { return errors.New("manager unavailable") }

type fixedBackend struct {
	ex  Execution
	err error
}

func (b fixedBackend) Start(context.Context, []byte) (Execution, error) { return b.ex, b.err }
func TestBackendFailuresAndParentCancellation(t *testing.T) {
	for _, kind := range []string{"start", "stop", "context"} {
		t.Run(kind, func(t *testing.T) {
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			r, w := io.Pipe()
			defer w.Close()
			ready := make(chan Frame, 1)
			done := make(chan Result, 1)
			ex := newFake().ex
			b := fixedBackend{ex, nil}
			if kind == "start" {
				b.err = errors.New("cannot submit")
			}
			if kind == "stop" {
				b.ex = failingStop{ex}
			}
			go func() {
				done <- Supervise(ctx, r, func(f Frame) error { ready <- f; return nil }, digest([]byte("ok")), opts(), b)
			}()
			send(t, w, Frame{Manifest, []byte("ok")})
			f := <-ready
			send(t, w, Frame{Start, f.Body})
			if kind == "context" {
				cancel()
			}
			select {
			case got := <-done:
				want := map[string]string{"start": "start-failed", "stop": "lease-expired:stop-unconfirmed", "context": "cancelled"}[kind]
				if got.Reason != want {
					t.Fatal(got, want)
				}
			case <-time.After(time.Second):
				t.Fatal("unbounded failure")
			}
		})
	}
}
func TestManifestTimeoutWithoutEOF(t *testing.T) {
	r, w := io.Pipe()
	defer w.Close()
	got := Supervise(context.Background(), r, func(Frame) error { return nil }, digest(nil), Options{time.Second, 20 * time.Millisecond, time.Second, time.Second}, newFake())
	if got.Reason != "manifest-rejected" {
		t.Fatal(got)
	}
}
func TestReadyWriteFailureNeverStarts(t *testing.T) {
	var input bytes.Buffer
	WriteFrame(&input, Frame{Manifest, []byte("ok")})
	b := newFake()
	got := Supervise(context.Background(), io.NopCloser(&input), func(Frame) error { return io.ErrClosedPipe }, digest([]byte("ok")), opts(), b)
	if got.Reason != "reviewer-unavailable" || b.starts != 0 {
		t.Fatal(got)
	}
}
