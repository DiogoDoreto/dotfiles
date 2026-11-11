package host

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"host-command-review/contracts"
)

func TestKnownVMShareRootsRejected(t *testing.T) {
	for _, path := range []string{"/home/dog/projects/reviewer", "/var/lib/agent-vm/reviewer"} {
		if s, e := OpenStore(path, nil); e == nil {
			s.Close()
			t.Fatal("VM-share store accepted")
		}
	}
}
func TestQueueBoundAndExpiredSessionNotRevived(t *testing.T) {
	c, clock := setup(t)
	c.cfg.MaxPendingPerSession = 1
	imported(t, c, "1")
	p := proposal()
	req := contracts.Request{Version: 1, Operation: contracts.Submit, SessionID: c.session, RequestID: strings.Repeat("2", 64), Proposal: &p}
	if _, e := c.Import(req); !errors.Is(e, ErrFull) {
		t.Fatal(e)
	}
	clock.boot += 4 * time.Hour
	clock.now = clock.now.Add(4 * time.Hour)
	if _, e := c.Import(req); !errors.Is(e, ErrGuard) {
		t.Fatal("expired session admitted", e)
	}
	if e := c.Heartbeat(); e == nil {
		t.Fatal("heartbeat revived expired session")
	}
}
func TestCompletedHistoryDoesNotConsumeHostAdmissionSlot(t *testing.T) {
	c, _ := setup(t)
	c.cfg.MaxPendingPerSession = 1
	r := imported(t, c, "1")
	if e := c.Reject(r.Snapshot.SnapshotID, r.Revision); e != nil {
		t.Fatal(e)
	}
	if _, e := c.View(r.Snapshot.SnapshotID); e != nil {
		t.Fatal(e)
	}
	imported(t, c, "2")
	// Duplicate history must stay retrievable even when the work queue is full.
	if got := imported(t, c, "1"); got.Phase != contracts.Denied {
		t.Fatal(got)
	}
	p := proposal()
	p.Script = "changed"
	_, e := c.Import(contracts.Request{Version: 1, Operation: contracts.Submit, SessionID: c.session, RequestID: strings.Repeat("1", 64), Proposal: &p})
	if !errors.Is(e, ErrConflict) {
		t.Fatalf("history conflict lost: %v", e)
	}
}

func TestHeldWorkConsumesHostSlotButReleaseFreesItWithoutLosingHistory(t *testing.T) {
	c, _ := setup(t)
	c.cfg.MaxPendingPerSession = 1
	r, artifact := held(t, c)
	p := proposal()
	q := contracts.Request{Version: 1, Operation: contracts.Submit, SessionID: c.session, RequestID: strings.Repeat("2", 64), Proposal: &p}
	if _, err := c.Import(q); !errors.Is(err, ErrFull) {
		t.Fatal(err)
	}
	if err := c.Release(r.Snapshot.SnapshotID, r.Revision, artifact); err != nil {
		t.Fatal(err)
	}
	before, err := c.StatusBytes(c.session, r.Snapshot.RequestID)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := c.Import(q); err != nil {
		t.Fatal(err)
	}
	after, err := c.StatusBytes(c.session, r.Snapshot.RequestID)
	if err != nil || !bytes.Equal(before, after) {
		t.Fatalf("released history changed: %v", err)
	}
}

func TestPreviousSessionHistoryDoesNotConsumeHostAdmissionSlot(t *testing.T) {
	c, clock := setup(t)
	c.cfg.MaxPendingPerSession = 1
	old := imported(t, c, "1")
	if e := c.CloseUnclean(); e != nil {
		t.Fatal(e)
	}
	next, e := NewController(c.store, Config{Shell: shell, Clock: clock, MaxPendingPerSession: 1})
	if e != nil {
		t.Fatal(e)
	}
	t.Cleanup(func() { next.CloseUnclean() })
	imported(t, next, "2")
	if _, e := next.View(old.Snapshot.SnapshotID); e != nil {
		t.Fatal(e)
	}
}

func TestHostHistoryCanExceedOld256RecordLimit(t *testing.T) {
	c, _ := setup(t)
	base := imported(t, c, "1")
	base.Phase = contracts.Denied
	if e := c.save(base); e != nil {
		t.Fatal(e)
	}
	// Seed canonical retained history without repeatedly walking it via Import.
	for i := 1; i < 256; i++ {
		r := base
		r.Snapshot.SnapshotID = randomID()
		r.Snapshot.RequestID = fmt.Sprintf("%064x", i)
		for dir, value := range map[string]any{"snapshots": r.Snapshot, "records": r} {
			b, e := json.Marshal(value)
			if e != nil {
				t.Fatal(e)
			}
			if e = os.WriteFile(filepath.Join(c.store.root.Name(), dir, r.Snapshot.SnapshotID+".json"), b, 0600); e != nil {
				t.Fatal(e)
			}
		}
	}
	imported(t, c, "2")
	if e := c.Sweep(false); e != nil {
		t.Fatal(e)
	}
	if e := c.CloseUnclean(); e != nil {
		t.Fatal(e)
	}
	next, e := NewController(c.store, c.cfg)
	if e != nil {
		t.Fatalf("recovery blocked by retained history: %v", e)
	}
	t.Cleanup(func() { next.CloseUnclean() })
	imported(t, next, "3")
}

func TestResumeSessionExpiryCancelsManagedRun(t *testing.T) {
	c, clock := setup(t)
	cancelled := false
	c.executions[randomID()] = func() { cancelled = true }
	clock.boot += 4 * time.Hour
	if e := c.Invalidate(); e != nil {
		t.Fatal(e)
	}
	if !cancelled || !c.ended {
		t.Fatal("resume did not end expired execution authority")
	}
}
func TestLeaseReconnectNeedsFreshApproval(t *testing.T) {
	c, clock := setup(t)
	r := imported(t, c, "1")
	old := approved(t, c, r)
	clock.boot += 3 * time.Second
	clock.now = clock.now.Add(3 * time.Second)
	if e := c.Heartbeat(); e != nil {
		t.Fatal(e)
	}
	if _, e := c.Consume(old); e == nil {
		t.Fatal("reconnect revived approval")
	}
	next := approved(t, c, r)
	if next.Epoch == old.Epoch {
		t.Fatal("lease lapse epoch not changed")
	}
	if _, e := c.Consume(next); e != nil {
		t.Fatal(e)
	}
}
