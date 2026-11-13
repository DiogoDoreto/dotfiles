package reviewer

import (
	"bytes"
	"context"
	"encoding/json"
	"host-command-review/contracts"
	"host-command-review/execution"
	"host-command-review/web"
	"sync"
	"testing"
	"time"
)

// Real authenticated HTTP and guest publication, but no privileged executor.
func TestObservedOutputIsHostPrivateUntilFinalHeldReview(t *testing.T) {
	c, g, w, a, b := setup(t)
	q := submit(t, c, g, "8")
	if err := w.Cycle(context.Background()); err != nil {
		t.Fatal(err)
	}
	first := b.view().Requests[0]
	progress := make(chan struct{})
	finish := make(chan struct{})
	var once sync.Once
	unblock := func() { once.Do(func() { close(finish) }) }
	// Registered after setup cleanup: never leave Shutdown waiting for the fake.
	t.Cleanup(unblock)
	a.ExecuteObserved = func(ctx context.Context, _ execution.Config, _ contracts.ExecutionSpec, observe execution.Observer) execution.Report {
		observe(execution.Progress{Stdout: []byte("LIVE_PRIVATE_STDOUT"), Stderr: []byte("LIVE_PRIVATE_STDERR"), Truncated: true})
		close(progress)
		select {
		case <-finish:
		case <-ctx.Done():
		}
		return execution.Report{Ready: true, Result: execution.Result{Started: true, Stdout: []byte("FINAL_PRIVATE_STDOUT"), Stderr: []byte("FINAL_PRIVATE_STDERR"), Truncated: true, Exit: contracts.ExitInfo{Kind: "exited", Code: 0}}}
	}
	b.op(web.Operation{Action: "run", Binding: first.Binding}, 200)
	select {
	case <-progress:
	case <-time.After(3 * time.Second):
		t.Fatal("observed executor never started")
	}
	running := b.view().Requests[0]
	if running.Phase != contracts.Starting && running.Phase != contracts.Running {
		t.Fatalf("not active: %s", running.Phase)
	}
	if running.Live == nil || running.Live.RunID == "" || string(running.Live.Stdout) != "LIVE_PRIVATE_STDOUT" || string(running.Live.Stderr) != "LIVE_PRIVATE_STDERR" || !running.Live.Truncated || running.Held != nil {
		t.Fatalf("missing private live snapshot: %+v", running)
	}
	second := b.view().Requests[0]
	if second.Live == nil || second.Live.RunID != running.Live.RunID || second.Live.Sequence != running.Live.Sequence {
		t.Fatal("read changed live snapshot")
	}
	assertPrivate := func() {
		t.Helper()
		if err := w.Publish(context.Background()); err != nil {
			t.Fatal(err)
		}
		reply := get(t, g, q)
		data, err := json.Marshal(reply)
		if err != nil {
			t.Fatal(err)
		}
		for _, secret := range []string{"live", "run_id", "sequence", "stdout", "stderr", "LIVE_PRIVATE", "FINAL_PRIVATE", "TElWRV9QUklWQVRF", "RklOQUxfUFJJVkFURQ"} {
			if bytes.Contains(data, []byte(secret)) {
				t.Fatalf("guest publication disclosed %q: %s", secret, data)
			}
		}
		if reply.Artifact != nil {
			t.Fatal("automatic release")
		}
	}
	assertPrivate()
	// A live snapshot is not an artifact and cannot be submitted for release.
	b.op(web.Operation{Action: "release", Binding: running.Binding}, 400)
	unblock()
	waitPhase(t, c, first.SnapshotID, contracts.OutputHeld)
	held := b.view().Requests[0]
	if held.Live != nil || held.Held == nil || string(held.Held.Stdout) != "FINAL_PRIVATE_STDOUT" || string(held.Held.Stderr) != "FINAL_PRIVATE_STDERR" {
		t.Fatalf("final held did not replace live: %+v", held)
	}
	assertPrivate()
}
