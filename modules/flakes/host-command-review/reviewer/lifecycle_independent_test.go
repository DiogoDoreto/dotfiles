package reviewer

import (
	"context"
	"errors"
	"sync"
	"testing"
	"time"

	"host-command-review/contracts"
	"host-command-review/execution"
	"host-command-review/host"
	"host-command-review/transport"
	"host-command-review/web"
)

// End must not confuse cancellation delivery with actual supervisor completion.
func TestIndependentRestartWaitsForReapBeforeClosingRoute(t *testing.T) {
	a, g, _ := application(t)
	ctx := context.Background()
	if err := a.Shutdown(); err != nil {
		t.Fatal(err)
	}
	events := make(chan transport.Request, 100)
	a.Exchange = independentExchangeFunc(func(ctx context.Context, q transport.Request) (transport.Response, error) {
		events <- q
		return g.Exchange(ctx, q)
	})
	if err := a.Start(); err != nil {
		t.Fatal(err)
	}
	r := proposal(t, a, g, contracts.Root)
	cancelled, release := make(chan struct{}), make(chan struct{})
	var once sync.Once
	unblock := func() { once.Do(func() { close(release) }) }
	defer unblock()
	a.adapter.Execute = func(ctx context.Context, _ execution.Config, _ contracts.ExecutionSpec) execution.Report {
		<-ctx.Done()
		close(cancelled)
		<-release
		return execution.Report{AuthCancelled: true}
	}
	oldID := a.adapter.Controller.SessionID()
	if err := a.Apply(ctx, web.Operation{Action: "run_root", Binding: r.Binding}); err != nil {
		t.Fatal(err)
	}
	done := make(chan error, 1)
	go func() { done <- a.Apply(ctx, web.Operation{Action: "restart"}) }()
	select {
	case <-cancelled:
	case <-time.After(3 * time.Second):
		t.Fatal("restart did not cancel root authentication")
	}
	select {
	case err := <-done:
		t.Fatalf("restart returned before reap: %v", err)
	case <-time.After(50 * time.Millisecond):
	}
	for len(events) > 0 {
		q := <-events
		if q.Operation == "close" {
			t.Fatal("route closed before execution was joined")
		}
	}
	unblock()
	select {
	case err := <-done:
		if err != nil {
			t.Fatal(err)
		}
	case <-time.After(5 * time.Second):
		t.Fatal("restart failed to finish after reap")
	}
	seenStopped, seenClose := false, false
	for len(events) > 0 {
		q := <-events
		if q.SessionID != oldID {
			continue
		}
		if q.Operation == "publish" && q.Reply != nil && q.Reply.Status == contracts.Cancelled {
			seenStopped = true
		}
		if q.Operation == "close" {
			seenClose = true
			if !seenStopped {
				t.Error("route closed before publishing final cancelled state")
			}
		}
	}
	if !seenClose {
		t.Fatal("old route never closed")
	}
}

type independentExchangeFunc func(context.Context, transport.Request) (transport.Response, error)

func (f independentExchangeFunc) Call(ctx context.Context, q transport.Request) (transport.Response, error) {
	return f(ctx, q)
}

func TestIndependentUnreconciledExecutionBlocksReplacement(t *testing.T) {
	a, g, _ := application(t)
	r := proposal(t, a, g, contracts.Dog)
	// An executor that cannot supply a valid completion must leave durable intent,
	// not turn shutdown into a successful new session.
	a.adapter.Execute = func(ctx context.Context, _ execution.Config, _ contracts.ExecutionSpec) execution.Report {
		<-ctx.Done()
		return execution.Report{}
	}
	if err := a.Apply(context.Background(), web.Operation{Action: "run", Binding: r.Binding}); err != nil {
		t.Fatal(err)
	}
	if err := a.Apply(context.Background(), web.Operation{Action: "restart"}); err == nil {
		t.Fatal("restart accepted unreconciled completion")
	}
	v, err := a.View(context.Background())
	if err != nil || v.State != "cleanup_failed" {
		t.Fatalf("view = %+v, %v", v, err)
	}
	for _, action := range []string{"start", "restart"} {
		if err := a.Apply(context.Background(), web.Operation{Action: action}); err == nil {
			t.Errorf("%s bypassed failed reconciliation", action)
		}
	}
}

func TestIndependentExpiryCancelsRootAuthenticationWithoutQuittingApplication(t *testing.T) {
	a, g, _ := application(t)
	if err := a.Shutdown(); err != nil {
		t.Fatal(err)
	}
	clock := &appClock{start: time.Now()}
	a.Clock = clock
	quit := false
	a.Quit = func() { quit = true }
	if err := a.Start(); err != nil {
		t.Fatal(err)
	}
	r := proposal(t, a, g, contracts.Root)
	oldID := a.adapter.Controller.SessionID()
	started, reaped := make(chan struct{}), make(chan struct{})
	a.adapter.Execute = func(ctx context.Context, _ execution.Config, _ contracts.ExecutionSpec) execution.Report {
		close(started)
		<-ctx.Done()
		close(reaped)
		return execution.Report{AuthCancelled: true}
	}
	if err := a.Apply(context.Background(), web.Operation{Action: "run_root", Binding: r.Binding}); err != nil {
		t.Fatal(err)
	}
	select {
	case <-started:
	case <-time.After(3 * time.Second):
		t.Fatal("root authentication never started")
	}
	clock.elapsed.Store(int64(4 * time.Hour))
	if err := a.Tick(); err != nil {
		t.Fatal(err)
	}
	select {
	case <-reaped:
	default:
		t.Fatal("expiry returned without joining root authentication")
	}
	v, err := a.View(context.Background())
	if err != nil || v.State != "idle" || quit {
		t.Fatalf("expiry did not leave live idle application: %+v err=%v quit=%v", v, err, quit)
	}
	if err := a.Start(); err != nil {
		t.Fatal(err)
	}
	if err := a.Reviewer(context.Background(), time.Now().Add(time.Second)); err != nil {
		t.Fatal(err)
	}
	if err := a.Apply(context.Background(), web.Operation{Action: "run_root", Binding: r.Binding}); err == nil {
		t.Fatal("expired approval accepted after fresh heartbeat")
	}
	v, err = a.View(context.Background())
	if err != nil || v.SessionID == oldID || len(v.Requests) != 0 {
		t.Fatalf("expired work transferred: %+v %v", v, err)
	}
}

func TestIndependentAliasAllocationExhaustionCanRecoverWithoutReusingIdentity(t *testing.T) {
	a, g, _ := application(t)
	if err := a.Shutdown(); err != nil {
		t.Fatal(err)
	}
	attempts, closes := 0, 0
	failedID := ""
	a.Exchange = independentExchangeFunc(func(ctx context.Context, q transport.Request) (transport.Response, error) {
		if q.Operation == "announce" {
			attempts++
			if failedID == "" {
				failedID = q.SessionID
			}
			if failedID != q.SessionID {
				t.Error("allocation retry replaced internal identity")
			}
			return transport.Response{Version: 1, SessionID: q.SessionID, Error: "conflict", Items: []transport.Item{}}, nil
		}
		if q.Operation == "close" {
			closes++
		}
		return g.Exchange(ctx, q)
	})
	if err := a.Start(); err == nil {
		t.Fatal("collision exhaustion accepted")
	}
	if attempts != 16 || closes != 1 {
		t.Fatalf("announce attempts=%d close attempts=%d", attempts, closes)
	}
	v, err := a.View(context.Background())
	if err != nil || v.State != "idle" {
		t.Fatalf("failed allocation poisoned application: %+v %v", v, err)
	}
	a.Exchange = localExchange{g}
	if err := a.Start(); err != nil {
		t.Fatal(err)
	}
	v, err = a.View(context.Background())
	if err != nil || v.SessionID == failedID || v.State != "active" {
		t.Fatalf("recovery reused abandoned session: %+v %v", v, err)
	}
	if !errors.Is(a.Start(), host.ErrGuard) {
		t.Fatal("active Start did not fail closed")
	}
}
