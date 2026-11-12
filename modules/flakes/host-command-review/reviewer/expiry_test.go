package reviewer

import (
	"context"
	"host-command-review/contracts"
	"host-command-review/execution"
	"host-command-review/guest"
	"host-command-review/host"
	"host-command-review/web"
	"os"
	"path/filepath"
	"sync/atomic"
	"testing"
	"time"
)

type appClock struct {
	elapsed atomic.Int64
	start   time.Time
}

func (c *appClock) Now() time.Time                      { return c.start.Add(time.Duration(c.elapsed.Load())) }
func (c *appClock) BootElapsed() (time.Duration, error) { return time.Duration(c.elapsed.Load()), nil }
func TestSessionExpiryLeavesIdleAndFreshStart(t *testing.T) {
	s, err := host.OpenStore(filepath.Join(private(t), "host"), nil)
	if err != nil {
		t.Fatal(err)
	}
	defer s.Close()
	clock := &appClock{start: time.Now()}
	a := &Application{Store: s, Clock: clock, Execution: execution.Config{Shell: testShell}, Exchange: localExchange{&guest.Store{Dir: filepath.Join(private(t), "guest")}}}
	defer a.Shutdown()
	if err := a.Start(); err != nil {
		t.Fatal(err)
	}
	old := a.code
	clock.elapsed.Store(int64(4 * time.Hour))
	if err := a.Tick(); err != nil {
		t.Fatal(err)
	}
	v, err := a.View(context.Background())
	if err != nil || v.State != "idle" {
		t.Fatal(v, err)
	}
	if err := a.Start(); err != nil {
		t.Fatal(err)
	}
	if a.code == old {
		t.Fatal("expired code revived")
	}
}
func TestEndHeldOutputDeletesRawAndDoesNotTransferIt(t *testing.T) {
	a, g, path := application(t)
	r := proposal(t, a, g, contracts.Dog)
	a.adapter.Execute = func(context.Context, execution.Config, contracts.ExecutionSpec) execution.Report {
		return execution.Report{Ready: true, Result: execution.Result{Started: true, Stdout: []byte("held secret"), Stderr: []byte{}, Exit: contracts.ExitInfo{Kind: "exited", Code: 0}}}
	}
	if err := a.Apply(context.Background(), web.Operation{Action: "run", Binding: r.Binding}); err != nil {
		t.Fatal(err)
	}
	a.adapter.wg.Wait()
	names, err := os.ReadDir(filepath.Join(path, "raw"))
	if err != nil || len(names) == 0 {
		t.Fatal("missing capture", err)
	}
	if err := a.Apply(context.Background(), web.Operation{Action: "end"}); err != nil {
		t.Fatal(err)
	}
	names, err = os.ReadDir(filepath.Join(path, "raw"))
	if err != nil || len(names) != 0 {
		t.Fatal("raw survived end", err)
	}
	if err := a.Start(); err != nil {
		t.Fatal(err)
	}
	v, err := a.View(context.Background())
	if err != nil || len(v.Requests) != 0 {
		t.Fatal("old output transferred", v, err)
	}
}
