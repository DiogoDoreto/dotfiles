package reviewer

import (
	"bytes"
	"context"
	"errors"
	"host-command-review/contracts"
	"host-command-review/host"
	"host-command-review/transport"
	"sync"
	"time"
)

type Worker struct {
	Controller *host.Controller
	Exchange   Exchange
	Alias      string
}

func (w Worker) call(ctx context.Context, op string) (transport.Response, error) {
	return w.Exchange.Call(ctx, transport.Request{Version: 1, SessionID: w.Controller.SessionID(), Operation: op})
}
func (w Worker) Cycle(ctx context.Context) error {
	r, e := w.call(ctx, "pull")
	if e != nil {
		return e
	}
	if r.Validate() != nil || r.SessionID != w.Controller.SessionID() {
		return ErrTransport
	}
	for _, item := range r.Items {
		_, e := w.Controller.Import(item.Request)
		if e != nil {
			// Never substitute changed guest contents for an existing immutable import.
			if errors.Is(e, host.ErrFull) {
				q := contracts.Reply{Version: 1, SessionID: r.SessionID, RequestID: item.Request.RequestID, Status: contracts.Rejected, Error: "queue_full"}
				_, e = w.Exchange.Call(ctx, transport.Request{Version: 1, Operation: "publish", SessionID: r.SessionID, Reply: &q})
				if e != nil {
					return e
				}
				continue
			}
			return e
		}
		if item.CancelRequested {
			if _, e = w.Controller.Cancel(r.SessionID, item.Request.RequestID); e != nil {
				return e
			}
		}
	}
	return w.Publish(ctx)
}

// Publish never imports proposals and remains available during final shutdown.
func (w Worker) Publish(ctx context.Context) error {
	rs, e := w.Controller.List()
	if e != nil {
		return e
	}
	var failures []error
	for _, record := range rs {
		if ctx.Err() != nil {
			return errors.Join(append(failures, ctx.Err())...)
		}
		if record.Snapshot.SessionID != w.Controller.SessionID() {
			continue
		}
		// The ONLY outbound host data source is the durable release allowlist.
		b, e := w.Controller.StatusBytes(record.Snapshot.SessionID, record.Snapshot.RequestID)
		if e != nil {
			failures = append(failures, e)
			continue
		}
		reply, e := contracts.ParseReply(bytes.NewReader(b))
		if e != nil {
			failures = append(failures, e)
			continue
		}
		if _, e = w.Exchange.Call(ctx, transport.Request{Version: 1, Operation: "publish", SessionID: reply.SessionID, Reply: &reply}); e != nil {
			failures = append(failures, e)
		}
	}
	// One record's refusal must not starve later statuses/releases. Preserve all
	// errors for the caller; retries still send only durable host-approved data.
	return errors.Join(failures...)
}

// Run renews independently of pulls/publications. Any lost renewal ends this
// session, never reconnects it. Publication retries are data-only, not execution.
func (w Worker) Run(ctx context.Context) error {
	if _, e := w.Exchange.Call(ctx, transport.Request{Version: 1, Operation: "announce", SessionID: w.Controller.SessionID(), Label: "lapdog", Alias: w.Alias}); e != nil {
		return e
	}
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()
	var wg sync.WaitGroup
	wg.Add(1)
	renewErr := make(chan error, 1)
	go func() {
		defer wg.Done()
		t := time.NewTicker(5 * time.Second)
		defer t.Stop()
		for {
			select {
			case <-ctx.Done():
				return
			case <-t.C:
				if _, e := w.call(ctx, "renew"); e != nil {
					renewErr <- e
					cancel()
					return
				}
			}
		}
	}()
	defer wg.Wait()
	defer func() {
		cancel()
		closeCtx, stop := context.WithTimeout(context.Background(), 4*time.Second)
		defer stop()
		_, _ = w.call(closeCtx, "close")
	}()
	ticker := time.NewTicker(time.Second)
	defer ticker.Stop()
	for {
		// A failed publication can have committed; retries send identical ledger data.
		_ = w.Cycle(ctx)
		select {
		case e := <-renewErr:
			return e
		case <-ctx.Done():
			return ctx.Err()
		case <-ticker.C:
		}
	}
}
