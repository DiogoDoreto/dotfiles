package reviewer

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"host-command-review/contracts"
	"host-command-review/execution"
	"host-command-review/guest"
	"host-command-review/host"
	"host-command-review/transport"
	"host-command-review/web"
	"io"
	"net/http"
	"net/url"
	"os"
	"path/filepath"
	"strings"
	"sync/atomic"
	"testing"
	"time"
)

const testShell = "/nix/store/0123456789abcdfghijklmnpqrsvwxyz-bash-5.3/bin/bash"

func private(t *testing.T) string {
	t.Helper()
	p, e := os.MkdirTemp(".", ".integration-")
	if e != nil {
		t.Fatal(e)
	}
	p, _ = filepath.Abs(p)
	t.Cleanup(func() { os.RemoveAll(p) })
	return p
}

// Actual subprocess playing the fixed remote exchange behind fake SSH. No real
// network, polkit, systemd, privileged payload or external SSH daemon is invoked.
func TestSSHChild(t *testing.T) {
	pos := -1
	for i, s := range os.Args {
		if s == "--fake-ssh" {
			pos = i
			break
		}
	}
	if pos < 0 {
		return
	}
	mode, dir := os.Args[pos+1], os.Args[pos+2]
	if os.Args[len(os.Args)-1] != RemoteCommand {
		os.Exit(91)
	}
	switch mode {
	case "flood":
		_, _ = os.Stdout.Write(bytes.Repeat([]byte("x"), transport.MaxResponseBytes+1))
		os.Exit(0)
	case "hang":
		time.Sleep(20 * time.Second)
		os.Exit(0)
	case "wrong":
		q, e := transport.ParseRequest(os.Stdin)
		if e != nil {
			os.Exit(92)
		}
		_ = q
		json.NewEncoder(os.Stdout).Encode(transport.Response{Version: 1, SessionID: strings.Repeat("f", 64), LeaseSeconds: 15, Items: []transport.Item{}})
		os.Exit(0)
	}
	s := guest.Store{Dir: dir}
	if e := guest.ExchangeIO(context.Background(), &s, os.Stdin, os.Stdout); e != nil {
		os.Exit(93)
	}
	os.Exit(0)
}
func fakeSSH(t *testing.T, dir, mode string) SSH {
	t.Helper()
	exe, e := os.Executable()
	if e != nil {
		t.Fatal(e)
	}
	script := filepath.Join(private(t), "ssh")
	// All values are test-generated paths without shell metacharacters.
	if e = os.WriteFile(script, []byte(fmt.Sprintf("#!/bin/sh\nexport GORACE=atexit_sleep_ms=0\nexec '%s' -test.run '^TestSSHChild$' -- --fake-ssh '%s' '%s' \"$@\"\n", exe, mode, dir)), 0700); e != nil {
		t.Fatal(e)
	}
	return SSH{Executable: script, Address: "10.0.101.2", User: "agent", Identity: "/fixed/key", KnownHosts: "/fixed/pins"}
}

type browser struct {
	t                  *testing.T
	origin, auth, csrf string
}

func (b *browser) request(path string, body any, want int) []byte {
	b.t.Helper()
	var in io.Reader
	method := "GET"
	if body != nil {
		data, e := json.Marshal(body)
		if e != nil {
			b.t.Fatal(e)
		}
		in = bytes.NewReader(data)
		method = "POST"
	}
	req, e := http.NewRequest(method, b.origin+"/api/"+path, in)
	if e != nil {
		b.t.Fatal(e)
	}
	req.Header.Set("Origin", b.origin)
	req.Header.Set("Authorization", "Bearer "+b.auth)
	req.Header.Set("X-CSRF-Token", b.csrf)
	if body != nil {
		req.Header.Set("Content-Type", "application/json")
	}
	client := http.Client{Timeout: 5 * time.Second}
	resp, e := client.Do(req)
	if e != nil {
		b.t.Fatal(e)
	}
	defer resp.Body.Close()
	data, _ := io.ReadAll(resp.Body)
	if resp.StatusCode != want {
		b.t.Fatalf("%s: status %d want %d: %s", path, resp.StatusCode, want, data)
	}
	return data
}
func (b *browser) view() web.View {
	var v web.View
	if e := json.Unmarshal(b.request("view", nil, 200), &v); e != nil {
		b.t.Fatal(e)
	}
	return v
}
func (b *browser) op(o web.Operation, want int) { b.request("operation", o, want) }
func (b *browser) heartbeat()                   { b.request("heartbeat", map[string]any{}, 200) }
func setup(t *testing.T) (*host.Controller, *guest.Store, Worker, *Adapter, *browser) {
	t.Helper()
	storePath := private(t)
	s, e := host.OpenStore(storePath, nil)
	if e != nil {
		t.Fatal(e)
	}
	c, e := host.NewController(s, host.Config{Shell: testShell, BrowserLease: 5 * time.Second})
	if e != nil {
		t.Fatal(e)
	}
	ctx, stop := context.WithCancel(context.Background())
	a := &Adapter{Controller: c, Context: ctx, Stop: stop}
	server, secret, e := web.New(a)
	if e != nil {
		t.Fatal(e)
	}
	go server.Serve()
	u, _ := url.Parse(secret)
	b := &browser{t: t, origin: "http://" + u.Host}
	var credentials struct {
		Auth string `json:"auth"`
		CSRF string `json:"csrf"`
	}
	json.Unmarshal(b.request("bootstrap", map[string]string{"token": u.Fragment}, 200), &credentials)
	b.auth = credentials.Auth
	b.csrf = credentials.CSRF
	b.heartbeat()
	g := &guest.Store{Dir: private(t)}
	w := Worker{Controller: c, Exchange: fakeSSH(t, g.Dir, "normal")}
	_, e = w.Exchange.Call(ctx, transport.Request{Version: 1, Operation: "announce", SessionID: c.SessionID(), Label: "lapdog"})
	if e != nil {
		t.Fatal(e)
	}
	t.Cleanup(func() {
		stop()
		server.Close()
		if err := a.Shutdown(); err != nil {
			t.Error(err)
		}
		files, err := os.ReadDir(filepath.Join(storePath, "raw"))
		if err != nil || len(files) != 0 {
			t.Errorf("clean end retained raw capture: %v %v", files, err)
		}
		c.CloseUnclean()
		s.Close()
	})
	return c, g, w, a, b
}
func submit(t *testing.T, c *host.Controller, g *guest.Store, n string) contracts.Request {
	t.Helper()
	p := contracts.Proposal{Script: "printf PRIVATE_SCRIPT", Cwd: "/private/cwd", User: contracts.Dog, TimeoutSeconds: 120}
	q := contracts.Request{Version: 1, Operation: contracts.Submit, SessionID: c.SessionID(), RequestID: strings.Repeat(n, 64), Proposal: &p}
	r, e := g.Handle(context.Background(), q)
	if e != nil || r.Status != contracts.Pending {
		t.Fatalf("submit %v %v", r, e)
	}
	return q
}
func get(t *testing.T, g *guest.Store, q contracts.Request) contracts.Reply {
	t.Helper()
	q.Operation = contracts.Check
	q.Proposal = nil
	r, e := g.Handle(context.Background(), q)
	if e != nil {
		t.Fatal(e)
	}
	return r
}
func waitPhase(t *testing.T, c *host.Controller, id string, phase contracts.Phase) {
	t.Helper()
	deadline := time.Now().Add(3 * time.Second)
	for time.Now().Before(deadline) {
		r, e := c.View(id)
		if e != nil {
			t.Fatal(e)
		}
		if r.Phase == phase {
			return
		}
		time.Sleep(time.Millisecond)
	}
	r, _ := c.View(id)
	t.Fatalf("phase %s want %s", r.Phase, phase)
}
func TestSSHHostWebHeldReleaseLifecycle(t *testing.T) {
	c, g, w, a, b := setup(t)
	q := submit(t, c, g, "1")
	if e := w.Cycle(context.Background()); e != nil {
		t.Fatal(e)
	}
	b.heartbeat()
	first := b.view().Requests[0]
	edited := *q.Proposal
	edited.Script = "printf HOST_SECRET_SCRIPT"
	edited.Cwd = "/host/secret"
	b.op(web.Operation{Action: "edit", Binding: first.Binding, Proposal: &edited}, 200)
	b.op(web.Operation{Action: "run", Binding: first.Binding}, 409)
	current := b.view().Requests[0]
	if current.Binding.Revision != 2 || current.Spec.Script != edited.Script {
		t.Fatal("edit not frozen")
	}
	var calls atomic.Int32
	a.Execute = func(ctx context.Context, cfg execution.Config, s contracts.ExecutionSpec) execution.Report {
		calls.Add(1)
		if s.Script != edited.Script || s.Cwd != edited.Cwd {
			t.Error("executed guest/stale spec")
		}
		return execution.Report{Ready: true, Result: execution.Result{Started: true, Stdout: []byte("HOST_SECRET_OUTPUT"), Stderr: []byte{}, Exit: contracts.ExitInfo{Kind: "exited", Code: 0}}}
	}
	b.op(web.Operation{Action: "run", Binding: current.Binding}, 200)
	b.op(web.Operation{Action: "run", Binding: current.Binding}, 409)
	waitPhase(t, c, current.SnapshotID, contracts.OutputHeld)
	if e := w.Cycle(context.Background()); e != nil {
		t.Fatal(e)
	}
	held := get(t, g, q)
	raw, _ := json.Marshal(held)
	if held.Status != contracts.Held || held.Artifact != nil || bytes.Contains(raw, []byte("SECRET")) {
		t.Fatal("private data leaked")
	}
	b.heartbeat()
	v := b.view().Requests[0]
	if v.Held == nil || v.Held.Executed.Script != edited.Script {
		t.Fatal("whole metadata unavailable")
	}
	release := *v.Held
	release.Executed.Script = "[REDACTED]"
	release.Executed.Cwd = "/[REDACTED]"
	release.Stdout = []byte("safe")
	release.RedactedFields = []string{"executed.script", "executed.cwd", "stdout"}
	bad := release
	bad.Exit.Code = 42
	b.op(web.Operation{Action: "release", Binding: v.Binding, Artifact: &bad}, 409)
	b.op(web.Operation{Action: "release", Binding: v.Binding, Artifact: &release}, 200)
	for range 3 {
		if e := w.Cycle(context.Background()); e != nil {
			t.Fatal(e)
		}
	}
	r := get(t, g, q)
	x, _ := json.Marshal(r)
	if r.Artifact == nil || r.Artifact.Executed.Script != "[REDACTED]" || r.Artifact.ExecutionDigest != v.Binding.Digest || bytes.Contains(x, []byte("SECRET")) {
		t.Fatal("wrong release")
	}
	retry, e := g.Handle(context.Background(), q)
	if e != nil {
		t.Fatal(e)
	}
	y, _ := json.Marshal(retry)
	if !bytes.Equal(x, y) || calls.Load() != 1 {
		t.Fatal("retry mutated release/reexecuted")
	}
	// Stale disclosure is not a second mutable publication.
	b.op(web.Operation{Action: "release", Binding: v.Binding, Artifact: &release}, 409)
	if e = a.Shutdown(); e != nil {
		t.Fatal(e)
	}
}

func TestCancellationSuspendAndShutdown(t *testing.T) {
	c, g, w, a, b := setup(t)
	q := submit(t, c, g, "2")
	if e := w.Cycle(context.Background()); e != nil {
		t.Fatal(e)
	}
	v := b.view().Requests[0]
	if e := c.Invalidate(); e != nil {
		t.Fatal(e)
	}
	b.heartbeat()
	b.op(web.Operation{Action: "run", Binding: v.Binding}, 409)
	v = b.view().Requests[0]
	var calls atomic.Int32
	started := make(chan struct{})
	a.Execute = func(ctx context.Context, _ execution.Config, _ contracts.ExecutionSpec) execution.Report {
		calls.Add(1)
		close(started)
		<-ctx.Done()
		return execution.Report{Result: execution.Result{Uncertain: true}}
	}
	b.op(web.Operation{Action: "run_root", Binding: v.Binding}, 409)
	b.op(web.Operation{Action: "run", Binding: v.Binding}, 200)
	<-started
	cancelQ := q
	cancelQ.Operation = contracts.Cancel
	cancelQ.Proposal = nil
	if _, e := g.Handle(context.Background(), cancelQ); e != nil {
		t.Fatal(e)
	}
	if e := w.Cycle(context.Background()); e != nil {
		t.Fatal(e)
	}
	waitPhase(t, c, v.SnapshotID, contracts.Ambiguous)
	if e := w.Cycle(context.Background()); e != nil {
		t.Fatal(e)
	}
	if r := get(t, g, q); r.Status != contracts.Uncertain || r.Artifact != nil {
		t.Fatal("cancel fabricated success/output")
	}
	q2 := submit(t, c, g, "3")
	if e := w.Cycle(context.Background()); e != nil {
		t.Fatal(e)
	}
	if e := a.Shutdown(); e != nil {
		t.Fatal(e)
	}
	if e := w.Publish(context.Background()); e != nil {
		t.Fatal(e)
	}
	if r := get(t, g, q2); r.Status != contracts.Cancelled {
		t.Fatal("shutdown left accepted pending request")
	}
	if calls.Load() != 1 {
		t.Fatal("cancel retried execution")
	}
}
func TestSSHBoundsAndBinding(t *testing.T) {
	q := transport.Request{Version: 1, Operation: "pull", SessionID: strings.Repeat("a", 64)}
	for _, mode := range []string{"flood", "wrong", "hang"} {
		t.Run(mode, func(t *testing.T) {
			ctx, cancel := context.WithTimeout(context.Background(), 150*time.Millisecond)
			defer cancel()
			s := fakeSSH(t, private(t), mode)
			if _, e := s.Call(ctx, q); e == nil {
				t.Fatal("host accepted invalid peer")
			}
		})
	}
	args := strings.Join((SSH{}).Args(), " ")
	for _, required := range []string{"StrictHostKeyChecking=yes", "GlobalKnownHostsFile=/dev/null", "IdentityAgent=none", "ForwardAgent=no", "-F /dev/null", RemoteCommand} {
		if !strings.Contains(args, required) {
			t.Fatal(required)
		}
	}
}

// Publish a real active guest status while the fake executor is blocked in auth.
// Denial must return to pending without restoring the consumed host capability.
func TestAuthCancelledPublicationAndOtherRelease(t *testing.T) {
	c, g, w, a, b := setup(t)
	ctx := context.Background()
	q := submit(t, c, g, "4")
	other := submit(t, c, g, "5")
	if e := w.Cycle(ctx); e != nil {
		t.Fatal(e)
	}
	find := func(requestID string) web.RequestView {
		t.Helper()
		for _, v := range b.view().Requests {
			r, e := c.View(v.SnapshotID)
			if e != nil {
				t.Fatal(e)
			}
			if r.Snapshot.RequestID == requestID {
				return v
			}
		}
		t.Fatal("missing review")
		return web.RequestView{}
	}
	v := find(q.RequestID)
	root := *q.Proposal
	root.User = contracts.Root
	b.op(web.Operation{Action: "edit", Binding: v.Binding, Proposal: &root}, 200)
	v = find(q.RequestID)
	entered, finishAuth := make(chan struct{}), make(chan struct{})
	a.Execute = func(ctx context.Context, _ execution.Config, spec contracts.ExecutionSpec) execution.Report {
		if spec.User == contracts.Root {
			close(entered)
			select {
			case <-finishAuth:
				return execution.Report{AuthCancelled: true}
			case <-ctx.Done():
				return execution.Report{Result: execution.Result{Uncertain: true}}
			}
		}
		return execution.Report{Ready: true, Result: execution.Result{Started: true, Stdout: []byte("held output"), Stderr: []byte{}, Exit: contracts.ExitInfo{Kind: "exited", Code: 0}}}
	}
	b.op(web.Operation{Action: "run_root", Binding: v.Binding}, 200)
	<-entered
	if e := w.Publish(ctx); e != nil {
		t.Fatal(e)
	}
	if r := get(t, g, q); r.Status != contracts.Active {
		t.Fatalf("auth status %s", r.Status)
	}
	close(finishAuth)
	waitPhase(t, c, v.SnapshotID, contracts.Reviewing)
	if e := w.Publish(ctx); e != nil {
		t.Fatalf("auth retry publication: %v", e)
	}
	if r := get(t, g, q); r.Status != contracts.Pending || r.Artifact != nil {
		t.Fatal("auth retry did not remain coarse pending")
	}
	record, e := c.View(v.SnapshotID)
	if e != nil {
		t.Fatal(e)
	}
	if record.Approval != nil || len(record.Spent) != 1 || record.Spent[0] != v.Binding {
		t.Fatal("consumed approval was restored")
	}
	b.op(web.Operation{Action: "run_root", Binding: v.Binding}, 409)
	fresh := find(q.RequestID)
	if fresh.Binding.Nonce == v.Binding.Nonce {
		t.Fatal("retry did not require new offer")
	}
	otherView := find(other.RequestID)
	b.op(web.Operation{Action: "run", Binding: otherView.Binding}, 200)
	waitPhase(t, c, otherView.SnapshotID, contracts.OutputHeld)
	otherView = find(other.RequestID)
	b.op(web.Operation{Action: "release", Binding: otherView.Binding, Artifact: otherView.Held}, 200)
	for range 2 {
		if e := w.Publish(ctx); e != nil {
			t.Fatal(e)
		}
	}
	if r := get(t, g, other); r.Status != contracts.Released || r.Artifact == nil {
		t.Fatal("other request release starved")
	}
	if r := get(t, g, q); r.Status != contracts.Pending {
		t.Fatal("publication changed auth retry state")
	}
}

type failPublication struct {
	Exchange
	requestID string
}

func (f failPublication) Call(ctx context.Context, q transport.Request) (transport.Response, error) {
	if q.Operation == "publish" && q.Reply.RequestID == f.requestID {
		return transport.Response{}, ErrTransport
	}
	return f.Exchange.Call(ctx, q)
}
func TestPublishContinuesAfterRecordError(t *testing.T) {
	c, g, w, a, b := setup(t)
	submit(t, c, g, "6")
	submit(t, c, g, "7")
	if e := w.Cycle(context.Background()); e != nil {
		t.Fatal(e)
	}
	records, e := c.List()
	if e != nil {
		t.Fatal(e)
	}
	a.Execute = func(context.Context, execution.Config, contracts.ExecutionSpec) execution.Report {
		return execution.Report{Ready: true, Result: execution.Result{Started: true, Stdout: []byte("reviewed"), Stderr: []byte{}, Exit: contracts.ExitInfo{Kind: "exited", Code: 0}}}
	}
	for _, v := range b.view().Requests {
		if v.SnapshotID == records[0].Snapshot.SnapshotID {
			b.op(web.Operation{Action: "reject", Binding: v.Binding}, 200)
		} else {
			b.op(web.Operation{Action: "run", Binding: v.Binding}, 200)
			waitPhase(t, c, v.SnapshotID, contracts.OutputHeld)
		}
	}
	for _, v := range b.view().Requests {
		if v.Phase == contracts.OutputHeld {
			b.op(web.Operation{Action: "release", Binding: v.Binding, Artifact: v.Held}, 200)
		}
	}
	original := w.Exchange
	w.Exchange = failPublication{original, records[0].Snapshot.RequestID}
	if e := w.Publish(context.Background()); !errors.Is(e, ErrTransport) {
		t.Fatalf("publication error lost: %v", e)
	}
	q := contracts.Request{Version: 1, SessionID: c.SessionID(), RequestID: records[1].Snapshot.RequestID}
	if r := get(t, g, q); r.Status != contracts.Released || r.Artifact == nil {
		t.Fatal("later record release starved by first publication error")
	}
	w.Exchange = original
	if e := w.Publish(context.Background()); e != nil {
		t.Fatal(e)
	}
	q.RequestID = records[0].Snapshot.RequestID
	if r := get(t, g, q); r.Status != contracts.Rejected {
		t.Fatal("failed publication did not recover on next pass")
	}
}
