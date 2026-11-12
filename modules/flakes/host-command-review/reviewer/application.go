package reviewer

import (
	"context"
	"errors"
	"sync"
	"time"

	"host-command-review/contracts"
	"host-command-review/execution"
	"host-command-review/host"
	"host-command-review/transport"
	"host-command-review/web"
)

// Application owns session replacement. No controller, worker, binding, or run
// context is shared between sessions. The HTTP authentication lifetime is separate.
type Application struct {
	mu        sync.Mutex
	Store     *host.Store
	Execution execution.Config
	Exchange  Exchange
	Quit      context.CancelFunc
	// Execute is only a trusted test seam.
	Execute    func(context.Context, execution.Config, contracts.ExecutionSpec) execution.Report
	Clock      host.Clock
	adapter    *Adapter
	worker     *Worker
	workerStop context.CancelFunc
	done       chan error
	code       string
	expires    time.Time
	blocked    bool
	endErr     error
	transport  string
}

func (a *Application) start() error {
	if a.adapter != nil || a.blocked {
		return host.ErrGuard
	}
	c, err := host.NewController(a.Store, host.Config{Shell: a.Execution.Shell, BrowserLease: 5 * time.Second, Clock: a.Clock})
	if err != nil {
		return err
	}
	var code string
	allocated := false
	// Allocation is checked against all guest tombstones, not merely live routes.
	for attempts := 0; attempts < 16; attempts++ {
		code, err = transport.NewAlias()
		if err != nil {
			break
		}
		ctx, cancel := context.WithTimeout(context.Background(), 4*time.Second)
		var r transport.Response
		r, err = a.Exchange.Call(ctx, transport.Request{Version: 1, Operation: "announce", SessionID: c.SessionID(), Label: "lapdog", Alias: code})
		cancel()
		if r.Validate() == nil && r.SessionID == c.SessionID() && r.Error == "conflict" {
			continue
		}
		if err == nil && (r.Validate() != nil || r.SessionID != c.SessionID() || r.Error != "") {
			err = ErrTransport
		}
		if err == nil {
			allocated = true
			break
		}
		break
	}
	if err != nil || !allocated {
		ctx, cancel := context.WithTimeout(context.Background(), 4*time.Second)
		_, _ = a.Exchange.Call(ctx, transport.Request{Version: 1, Operation: "close", SessionID: c.SessionID()})
		cancel()
		cleanupErr := c.End()
		_ = c.CloseUnclean()
		if cleanupErr != nil {
			a.blocked = true
		}
		return errors.Join(err, ErrTransport, cleanupErr)
	}
	// The worker's duplicate announce does not renew or change this lease.
	runCtx, stopRun := context.WithCancel(context.Background())
	ctx, cancel := context.WithCancel(context.Background())
	a.adapter = &Adapter{Controller: c, Execution: a.Execution, Execute: a.Execute, Context: runCtx, Stop: stopRun}
	a.worker = &Worker{Controller: c, Exchange: a.Exchange, Alias: code}
	a.workerStop = cancel
	a.done = make(chan error, 1)
	a.code = code
	a.expires = time.Unix(c.ExpiresUnix(), 0)
	a.transport = "connected"
	go func(w *Worker, done chan error) { done <- w.Run(ctx) }(a.worker, a.done)
	return nil
}
func (a *Application) end() error {
	if a.adapter == nil {
		return a.endErr
	}
	// Deny all authority and join executions; keep renewal and raw data until
	// final permitted publication. Close/join the route before deleting captures.
	err := a.adapter.Quiesce()
	ctx, cancel := context.WithTimeout(context.Background(), 4*time.Second)
	_ = a.worker.Publish(ctx)
	cancel()
	a.workerStop()
	<-a.done // Run closes the route and joins its renewal loop before returning.
	err = errors.Join(err, a.adapter.Controller.End())
	_ = a.adapter.Controller.CloseUnclean()
	a.adapter = nil
	a.worker = nil
	a.code = ""
	a.expires = time.Time{}
	a.transport = "closed"
	if err != nil {
		a.blocked = true
		a.endErr = err
	}
	return err
}
func (a *Application) Start() error    { a.mu.Lock(); defer a.mu.Unlock(); return a.start() }
func (a *Application) Shutdown() error { a.mu.Lock(); defer a.mu.Unlock(); return a.end() }
func (a *Application) View(ctx context.Context) (web.View, error) {
	a.mu.Lock()
	defer a.mu.Unlock()
	v := web.View{Requests: []web.RequestView{}, State: "idle", Transport: a.transport}
	if a.blocked {
		v.State = "cleanup_failed"
	}
	if a.adapter == nil {
		return v, nil
	}
	out, err := a.adapter.View(ctx)
	if err != nil {
		return v, err
	}
	out.State = "active"
	out.RoutingCode = a.code
	out.ExpiresUnix = a.expires.Unix()
	out.Transport = a.transport
	return out, nil
}
func (a *Application) Reviewer(ctx context.Context, until time.Time) error {
	// Lifecycle transitions can wait on reaping/transport while holding mu.
	// Browser requests must fail closed promptly, not wait beyond their deadline.
	if ctx.Err() != nil {
		return ctx.Err()
	}
	if !a.mu.TryLock() {
		return host.ErrGuard
	}
	defer a.mu.Unlock()
	if a.adapter == nil {
		return nil
	}
	return a.adapter.Reviewer(ctx, until)
}
func (a *Application) Apply(ctx context.Context, o web.Operation) error {
	a.mu.Lock()
	defer a.mu.Unlock()
	switch o.Action {
	case "start":
		return a.start()
	case "end":
		return a.end()
	case "restart":
		if err := a.end(); err != nil {
			return err
		}
		return a.start()
	case "quit":
		err := a.end()
		// end has joined execution and closed the route even if raw cleanup
		// failed. Exit now; Shutdown retains the error for launcher reporting.
		a.Quit()
		return err
	}
	if a.adapter == nil {
		return host.ErrGuard
	}
	return a.adapter.Apply(ctx, o)
}

// Tick is independent of browser traffic. Expiry/lost transport leaves an idle
// application; cleanup failures latch a non-restartable state until process exit.
func (a *Application) Tick() error {
	a.mu.Lock()
	defer a.mu.Unlock()
	if a.adapter == nil {
		return nil
	}
	select {
	case err := <-a.done:
		a.done <- err // end owns the join
		a.transport = "lost"
		_ = a.end()
		return nil
	default:
	}
	if err := a.adapter.Controller.Tick(); err != nil {
		_ = a.end()
		return nil
	}
	if a.adapter.Context.Err() != nil {
		_ = a.end()
		return nil
	}
	return nil
}
func (a *Application) Invalidate() error {
	a.mu.Lock()
	defer a.mu.Unlock()
	if a.adapter == nil {
		return nil
	}
	return a.adapter.Controller.Invalidate()
}
func (a *Application) Sweep() error {
	a.mu.Lock()
	defer a.mu.Unlock()
	if a.adapter == nil {
		return nil
	}
	return a.adapter.Controller.Sweep(false)
}
