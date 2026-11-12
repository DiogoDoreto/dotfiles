package reviewer

import (
	"context"
	"errors"
	"host-command-review/contracts"
	"host-command-review/execution"
	"host-command-review/host"
	"host-command-review/transport"
	"host-command-review/web"
	"sync/atomic"
	"testing"
	"time"
)

func TestBrowserLivenessRecheckedAtConsumption(t *testing.T) {
	c, g, w, _, b := setup(t)
	submit(t, c, g, "4")
	if err := w.Cycle(context.Background()); err != nil {
		t.Fatal(err)
	}
	v := b.view().Requests[0]
	// Simulate a valid authenticated click followed by lease loss in the gap
	// between durable approval and Consume. Approval alone cannot authorize launch.
	if err := c.ReviewAction(v.Binding, "run", nil, nil); err != nil {
		t.Fatal(err)
	}
	if err := c.HeartbeatUntil(time.Now().Add(-time.Second)); err != nil {
		t.Fatal(err)
	}
	if _, err := c.Consume(v.Binding); !errors.Is(err, host.ErrGuard) {
		t.Fatalf("consumed without live browser: %v", err)
	}
	b.heartbeat()
	b.op(web.Operation{Action: "run", Binding: v.Binding}, 409)
	r, err := c.View(v.SnapshotID)
	if err != nil || r.Phase != contracts.Reviewing || len(r.Spent) != 0 {
		t.Fatalf("stale approval not revoked: %+v %v", r, err)
	}
}

func TestShutdownCancelsAndJoinsActiveExecution(t *testing.T) {
	c, g, w, a, b := setup(t)
	q := submit(t, c, g, "5")
	if err := w.Cycle(context.Background()); err != nil {
		t.Fatal(err)
	}
	v := b.view().Requests[0]
	started, reaped := make(chan struct{}), make(chan struct{})
	var calls atomic.Int32
	a.Execute = func(ctx context.Context, _ execution.Config, _ contracts.ExecutionSpec) execution.Report {
		calls.Add(1)
		close(started)
		<-ctx.Done()
		close(reaped)
		return execution.Report{Result: execution.Result{Uncertain: true}}
	}
	b.op(web.Operation{Action: "run", Binding: v.Binding}, 200)
	<-started
	if err := a.Shutdown(); err != nil {
		t.Fatal(err)
	}
	select {
	case <-reaped:
	default:
		t.Fatal("shutdown abandoned execution child")
	}
	if err := w.Publish(context.Background()); err != nil {
		t.Fatal(err)
	}
	if r := get(t, g, q); r.Status != contracts.Uncertain || r.Artifact != nil {
		t.Fatal("shutdown invented success or released data")
	}
	if calls.Load() != 1 {
		t.Fatal("shutdown retried execution")
	}
}

// A stuck pull must not starve the independent short-lease renewal worker.
type blockedPull struct {
	Exchange
	renewed chan struct{}
}

func (b blockedPull) Call(ctx context.Context, q transport.Request) (transport.Response, error) {
	if q.Operation == "pull" {
		<-ctx.Done()
		return transport.Response{}, ctx.Err()
	}
	r, err := b.Exchange.Call(ctx, q)
	if q.Operation == "renew" && err == nil {
		select {
		case b.renewed <- struct{}{}:
		default:
		}
	}
	return r, err
}
func TestWorkerRenewsIndependentlyAndClosesOnExit(t *testing.T) {
	c, g, w, _, _ := setup(t)
	w.Exchange = blockedPull{Exchange: w.Exchange, renewed: make(chan struct{}, 1)}
	x := w.Exchange.(blockedPull)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	done := make(chan error, 1)
	go func() { done <- w.Run(ctx) }()
	select {
	case <-x.renewed:
	case <-time.After(8 * time.Second):
		t.Fatal("blocked pull starved renewal")
	}
	cancel()
	select {
	case <-done:
	case <-time.After(3 * time.Second):
		t.Fatal("worker shutdown did not close route/join renewal")
	}
	q := contracts.Request{Version: 1, Operation: contracts.Submit, SessionID: c.SessionID(), RequestID: "8888888888888888888888888888888888888888888888888888888888888888", Proposal: &contracts.Proposal{Reason: "Inspect host state", Script: "true", Cwd: "/", User: contracts.Dog, TimeoutSeconds: 120}}
	r, err := g.Handle(context.Background(), q)
	if err != nil || r.Error != "no_reviewer" {
		t.Fatalf("worker exit left a live route: %+v %v", r, err)
	}
}

func TestGuestRouteClosedRejectsFurtherProposals(t *testing.T) {
	c, g, w, _, _ := setup(t)
	q := submit(t, c, g, "6")
	if _, err := w.call(context.Background(), "close"); err != nil {
		t.Fatal(err)
	}
	if r := get(t, g, q); r.Status != contracts.Rejected || r.Error != "session_expired" {
		t.Fatalf("unacknowledged proposal survived close: %+v", r)
	}
	q.RequestID = "7777777777777777777777777777777777777777777777777777777777777777"
	r, err := g.Handle(context.Background(), q)
	if err != nil || r.Status != contracts.Rejected || r.Error != "no_reviewer" {
		t.Fatalf("accepted without reviewer: %+v %v", r, err)
	}
	if _, err := w.call(context.Background(), "renew"); err == nil {
		t.Fatal("closed session renewed")
	}
}
