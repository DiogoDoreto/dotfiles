package host

import (
	"host-command-review/contracts"
	"testing"
)

func TestReasonIsImmutableAcrossHostEditsAndOfferBinding(t *testing.T) {
	c, _ := setup(t)
	r := imported(t, c, "1")
	reviews, err := c.Reviews()
	if err != nil || len(reviews) != 1 {
		t.Fatal(reviews, err)
	}
	old := reviews[0].Binding
	changed := r.Snapshot.Proposal
	changed.Reason += " changed"
	if err := c.Edit(r.Snapshot.SnapshotID, r.Revision, changed); err == nil {
		t.Fatal("host edit rewrote reason")
	}
	if err := c.ReviewAction(old, "edit", &changed, nil); err == nil {
		t.Fatal("browser edit rewrote reason")
	}
	// Defense in depth: a reason mismatch in a host-private offer is substantive,
	// even though normal APIs never permit changing the immutable original reason.
	offer := c.offers[r.Snapshot.SnapshotID]
	offer.Reason = changed.Reason
	c.offers[r.Snapshot.SnapshotID] = offer
	if err := c.ReviewAction(old, "run", nil, nil); err == nil {
		t.Fatal("stale reason offer authorized execution")
	}
	reviews, err = c.Reviews()
	if err != nil {
		t.Fatal(err)
	}
	fresh := reviews[0].Binding
	if fresh.Nonce == old.Nonce || fresh.Digest != old.Digest || fresh.Revision != old.Revision {
		t.Fatal("reason binding should renew outside execution digest/revision")
	}
	if err := c.ReviewAction(old, "run", nil, nil); err == nil {
		t.Fatal("old binding survived renewal")
	}
	changed.Reason = r.Snapshot.Proposal.Reason
	changed.Script = "true"
	if err := c.ReviewAction(fresh, "edit", &changed, nil); err != nil {
		t.Fatal(err)
	}
	current, err := c.View(r.Snapshot.SnapshotID)
	if err != nil || current.Snapshot != r.Snapshot || current.Revision != r.Revision+1 || current.Phase != contracts.Reviewing {
		t.Fatal("edit damaged original or review semantics", err)
	}
}
