package host

import (
	"bytes"
	"context"
	"testing"

	"host-command-review/contracts"
	"host-command-review/execution"
)

func TestLiveBeforeFinalAndCleanup(t *testing.T) {
	for _, end := range []bool{false, true} {
		t.Run(map[bool]string{false: "finish", true: "shutdown"}[end], func(t *testing.T) {
			c, _ := setup(t)
			r := imported(t, c, "1")
			a := approved(t, c, r)
			spec, e := c.Consume(a)
			if e != nil {
				t.Fatal(e)
			}
			before, e := c.StatusBytes(c.session, r.Snapshot.RequestID)
			if e != nil {
				t.Fatal(e)
			}
			ready := make(chan execution.Observer, 1)
			finish := make(chan struct{})
			done := make(chan error, 1)
			go func() {
				done <- c.RunConsumedWithObservedExecutor(context.Background(), a, spec, execution.Config{}, func(ctx context.Context, _ execution.Config, _ contracts.ExecutionSpec, observe execution.Observer) execution.Report {
					observe(execution.Progress{Stdout: []byte("host secret"), Stderr: []byte{}})
					ready <- observe
					<-finish
					return execution.Report{Ready: true, Result: execution.Result{Started: true, Stdout: []byte("host secret"), Stderr: []byte{}, Exit: contracts.ExitInfo{Kind: "exited", Code: 0}}}
				})
			}()
			observe := <-ready
			reviews, e := c.Reviews()
			if e != nil || len(reviews) != 1 || reviews[0].Live == nil {
				t.Fatalf("live missing: %v %v", reviews, e)
			}
			live := reviews[0].Live
			if live.RunID == "" || live.Sequence != 1 || string(live.Stdout) != "host secret" {
				t.Fatal(live)
			}
			live.Stdout[0] = 'X'
			reviews, e = c.Reviews()
			if e != nil || string(reviews[0].Live.Stdout) != "host secret" {
				t.Fatal("not a defensive copy")
			}
			after, e := c.StatusBytes(c.session, r.Snapshot.RequestID)
			if e != nil || !bytes.Equal(before, after) || bytes.Contains(after, []byte("secret")) {
				t.Fatal("guest leakage")
			}
			if end {
				if e := c.BeginEnd(); e != nil {
					t.Fatal(e)
				}
				if c.copyLive(a.SnapshotID) != nil {
					t.Fatal("shutdown retained live")
				}
				observe(execution.Progress{StdoutOffset: 11, Stdout: []byte("late"), Stderr: []byte{}})
				if c.copyLive(a.SnapshotID) != nil {
					t.Fatal("late callback resurrected capture")
				}
			}
			close(finish)
			if e := <-done; e != nil {
				t.Fatal(e)
			}
			if c.copyLive(a.SnapshotID) != nil {
				t.Fatal("terminal retained live")
			}
			observe(execution.Progress{StdoutOffset: 11, Stdout: []byte("late"), Stderr: []byte{}})
			if c.copyLive(a.SnapshotID) != nil {
				t.Fatal("stale callback resurrected capture")
			}
		})
	}
}

func TestLiveRejectsInvalidUpdates(t *testing.T) {
	c, _ := setup(t)
	for _, p := range []execution.Progress{
		{StdoutOffset: 1, Stdout: []byte("x"), Stderr: []byte{}},
		{Stdout: make([]byte, execution.ProgressBytes+1), Stderr: []byte{}},
		{Stdout: nil, Stderr: []byte{}},
	} {
		c.liveMu.Lock()
		c.live = map[string]*LiveCapture{"id": {RunID: "fresh", Stdout: []byte{}, Stderr: []byte{}}}
		c.liveMu.Unlock()
		c.observeLive("id", "old", p)
		if c.copyLive("id") == nil {
			t.Fatal("old token affected fresh run")
		}
		c.observeLive("id", "fresh", p)
		if c.copyLive("id") != nil {
			t.Fatal("invalid progress retained")
		}
	}
}
