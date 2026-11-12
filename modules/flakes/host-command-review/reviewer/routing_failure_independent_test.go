package reviewer

import (
	"context"
	"testing"
	"time"

	"host-command-review/transport"
)

func TestIndependentWorkerStopsOnRejectedGuestRenewal(t *testing.T) {
	_, g, w, _, _ := setup(t)
	underlying := w.Exchange
	rejected := make(chan struct{}, 1)
	w.Exchange = independentExchangeFunc(func(ctx context.Context, q transport.Request) (transport.Response, error) {
		if q.Operation == "renew" {
			// Simulate the guest losing/revoking its route while SSH remains healthy.
			// The guest returns session_expired over healthy SSH. The SSH adapter
			// must classify that refusal as failure, and the worker must stop.
			if _, err := g.Exchange(ctx, transport.Request{Version: 1, Operation: "close", SessionID: q.SessionID}); err != nil {
				return transport.Response{}, err
			}
			r, err := underlying.Call(ctx, q)
			if r.Error == "session_expired" {
				select {
				case rejected <- struct{}{}:
				default:
				}
			}
			return r, err
		}
		return underlying.Call(ctx, q)
	})
	ctx, cancel := context.WithCancel(context.Background())
	done := make(chan error, 1)
	go func() { done <- w.Run(ctx) }()
	defer func() {
		cancel()
		select {
		case <-done:
		case <-time.After(3 * time.Second):
			t.Error("worker failed to join after cancellation")
		}
	}()
	select {
	case <-rejected:
	case <-time.After(8 * time.Second):
		t.Fatal("renewal was not exercised")
	}
	select {
	case err := <-done:
		// Keep the completion available for the deferred join.
		done <- err
		if err == nil {
			t.Fatal("route rejection reported success")
		}
	case <-time.After(time.Second):
		t.Fatal("worker retained a rejected route; a protocol error must end the review session even when SSH succeeds")
	}
}
