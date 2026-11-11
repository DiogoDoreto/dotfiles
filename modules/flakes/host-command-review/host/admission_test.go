package host

import (
	"errors"
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
	c.cfg.MaxRecords = 1
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
