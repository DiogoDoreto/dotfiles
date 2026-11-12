package reviewer

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"host-command-review/contracts"
	"host-command-review/execution"
	"host-command-review/guest"
	"host-command-review/host"
	"host-command-review/transport"
	"host-command-review/web"
)

type localExchange struct{ g *guest.Store }

func (x localExchange) Call(ctx context.Context, q transport.Request) (transport.Response, error) {
	return x.g.Exchange(ctx, q)
}
func application(t *testing.T) (*Application, *guest.Store, string) {
	t.Helper()
	path := filepath.Join(private(t), "host")
	s, err := host.OpenStore(path, nil)
	if err != nil {
		t.Fatal(err)
	}
	g := &guest.Store{Dir: filepath.Join(private(t), "guest")}
	a := &Application{Store: s, Execution: execution.Config{Shell: testShell}, Exchange: localExchange{g}, Quit: func() {}}
	t.Cleanup(func() { _ = a.Shutdown(); s.Close() })
	if err = a.Start(); err != nil {
		t.Fatal(err)
	}
	return a, g, path
}
func proposal(t *testing.T, a *Application, g *guest.Store, user contracts.User) web.RequestView {
	t.Helper()
	ctx := context.Background()
	q := contracts.Request{Version: 1, Operation: contracts.Submit, SessionID: a.adapter.Controller.SessionID(), RequestID: strings.Repeat("a", 64), Proposal: &contracts.Proposal{Reason: "Inspect host state", Script: "true", Cwd: "/", User: user, TimeoutSeconds: 120}}
	if _, err := g.Handle(ctx, q); err != nil {
		t.Fatal(err)
	}
	if err := a.worker.Cycle(ctx); err != nil {
		t.Fatal(err)
	}
	if err := a.Reviewer(ctx, time.Now().Add(time.Second)); err != nil {
		t.Fatal(err)
	}
	v, err := a.View(ctx)
	if err != nil || len(v.Requests) != 1 {
		t.Fatal(v, err)
	}
	return v.Requests[0]
}
func TestApplicationRestartReapsActiveWorkAndRejectsOldAuthority(t *testing.T) {
	for _, user := range []contracts.User{contracts.Root, contracts.Dog} {
		t.Run(string(user), func(t *testing.T) { restartActiveWork(t, user) })
	}
}
func restartActiveWork(t *testing.T, user contracts.User) {
	a, g, _ := application(t)
	ctx := context.Background()
	oldCode := a.code
	oldID := a.adapter.Controller.SessionID()
	started, reaped := make(chan struct{}), make(chan struct{})
	a.adapter.Execute = func(ctx context.Context, _ execution.Config, _ contracts.ExecutionSpec) execution.Report {
		close(started)
		<-ctx.Done()
		close(reaped)
		return execution.Report{Result: execution.Result{Uncertain: true}}
	}
	r := proposal(t, a, g, user)
	action := "run"
	if user == contracts.Root {
		action = "run_root"
	}
	if err := a.Apply(ctx, web.Operation{Action: action, Binding: r.Binding}); err != nil {
		t.Fatal(err)
	}
	<-started
	if err := a.Apply(ctx, web.Operation{Action: "restart"}); err != nil {
		t.Fatal(err)
	}
	select {
	case <-reaped:
	default:
		t.Fatal("execution not joined")
	}
	if a.code == oldCode || a.adapter.Controller.SessionID() == oldID {
		t.Fatal("session reused")
	}
	if err := a.Reviewer(ctx, time.Now().Add(time.Second)); err != nil {
		t.Fatal(err)
	}
	if err := a.Apply(ctx, web.Operation{Action: action, Binding: r.Binding}); err == nil {
		t.Fatal("old binding accepted")
	}
	id, err := g.ResolveRoute(ctx, strings.ToLower(strings.ReplaceAll(oldCode, "-", "")))
	if err != nil || id != oldID {
		t.Fatal("old alias redirected", id, err)
	}
	out, err := g.Handle(ctx, contracts.Request{Version: 1, Operation: contracts.Submit, SessionID: id, RequestID: strings.Repeat("b", 64), Proposal: &contracts.Proposal{Reason: "Inspect host state", Script: "true", Cwd: "/", User: contracts.Dog, TimeoutSeconds: 120}})
	if err != nil || out.Error != "no_reviewer" {
		t.Fatal(out, err)
	}
	v, err := a.View(ctx)
	if err != nil || len(v.Requests) != 0 {
		t.Fatal("old requests transferred", v, err)
	}
}
func TestApplicationHeldCleanupFailureBlocksRestart(t *testing.T) {
	a, g, path := application(t)
	r := proposal(t, a, g, contracts.Dog)
	a.adapter.Execute = func(context.Context, execution.Config, contracts.ExecutionSpec) execution.Report {
		return execution.Report{Ready: true, Result: execution.Result{Started: true, Stdout: []byte("private"), Stderr: []byte{}, Exit: contracts.ExitInfo{Kind: "exited", Code: 0}}}
	}
	if err := a.Apply(context.Background(), web.Operation{Action: "run", Binding: r.Binding}); err != nil {
		t.Fatal(err)
	}
	a.adapter.wg.Wait()
	v0, err := a.View(context.Background())
	if err != nil || v0.Requests[0].Held == nil {
		t.Fatal("output not held", v0, err)
	}
	// Corrupting a private capture directory is an injected cleanup failure. It
	// must never be treated as an empty directory or allow a replacement session.
	if err := os.WriteFile(filepath.Join(path, "raw", "unexpected"), []byte("bad"), 0600); err != nil {
		t.Fatal(err)
	}
	if err := a.Apply(context.Background(), web.Operation{Action: "restart"}); err == nil {
		t.Fatal("cleanup failure ignored")
	}
	v, err := a.View(context.Background())
	if err != nil || v.State != "cleanup_failed" {
		t.Fatal(v, err)
	}
	if err := a.Start(); err == nil {
		t.Fatal("restart after failed cleanup")
	}
}
