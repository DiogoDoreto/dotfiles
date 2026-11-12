// Package reviewer joins the host authority, private browser and untrusted SSH queue.
package reviewer

import (
	"context"
	"errors"
	"host-command-review/contracts"
	"host-command-review/execution"
	"host-command-review/host"
	"host-command-review/web"
	"sync"
	"time"
)

type Adapter struct {
	Controller *host.Controller
	Execution  execution.Config
	// Execute is a trusted test seam; production always uses RunConsumed.
	Execute func(context.Context, execution.Config, contracts.ExecutionSpec) execution.Report
	Stop    context.CancelFunc
	Context context.Context
	mu      sync.Mutex
	wg      sync.WaitGroup
	closing bool
}

func (a *Adapter) View(ctx context.Context) (web.View, error) {
	a.mu.Lock()
	defer a.mu.Unlock()
	out := web.View{SessionID: a.Controller.SessionID(), Requests: []web.RequestView{}}
	if a.closing {
		return out, host.ErrGuard
	}
	rs, e := a.Controller.Reviews()
	if e != nil {
		return out, e
	}
	for _, r := range rs {
		out.Requests = append(out.Requests, web.RequestView{SnapshotID: r.Record.Snapshot.SnapshotID, Reason: "Guest request " + r.Record.Snapshot.RequestID, Phase: r.Record.Phase, Spec: r.Record.Spec, Binding: r.Binding, Held: r.Held})
	}
	return out, nil
}
func (a *Adapter) Reviewer(ctx context.Context, deadline time.Time) error {
	a.mu.Lock()
	defer a.mu.Unlock()
	if deadline.IsZero() || !time.Now().Before(deadline) {
		return a.Controller.Disconnect()
	}
	if a.closing {
		return host.ErrGuard
	}
	return a.Controller.HeartbeatUntil(deadline)
}
func (a *Adapter) Apply(ctx context.Context, o web.Operation) error {
	a.mu.Lock()
	defer a.mu.Unlock()
	if a.closing {
		return host.ErrGuard
	}
	if o.Action == "end" {
		a.closing = true
		a.Stop()
		return nil
	}
	if ctx.Err() != nil {
		return ctx.Err()
	}
	if e := a.Controller.ReviewAction(o.Binding, o.Action, o.Proposal, o.Artifact); e != nil {
		return e
	}
	if o.Action != "run" && o.Action != "run_root" {
		return nil
	}
	spec, e := a.Controller.Consume(o.Binding)
	if e != nil {
		_ = a.Controller.Invalidate()
		return e
	}
	a.wg.Add(1)
	go func() {
		defer a.wg.Done()
		var e error
		if a.Execute != nil {
			e = a.Controller.RunConsumedWithExecutor(a.Context, o.Binding, spec, a.Execution, a.Execute)
		} else {
			e = a.Controller.RunConsumed(a.Context, o.Binding, spec, a.Execution)
		}
		if e != nil {
			a.Stop()
		}
	}()
	return nil
}

// Quiesce stops authority and joins executions, keeping captures and the
// singleton for final publication and route closure before cleanup.
func (a *Adapter) Quiesce() error {
	a.mu.Lock()
	a.closing = true
	a.Stop()
	first := a.Controller.BeginEnd()
	a.mu.Unlock()
	a.wg.Wait()
	return errors.Join(first, a.Controller.BeginEnd())
}
func (a *Adapter) Shutdown() error {
	first := a.Quiesce()
	if e := a.Controller.End(); e != nil {
		_ = a.Controller.CloseUnclean()
		return errors.Join(first, e)
	}
	return first
}
