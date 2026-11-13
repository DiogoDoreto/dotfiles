package host

import (
	"bytes"
	"encoding/json"
	"host-command-review/contracts"
)

// Offers are host-private UI capabilities, not approvals. GET never approves.
// Phase is included in their validity so a stale page cannot act on a later phase.
type reviewOffer struct {
	Binding contracts.ApprovalBinding
	Phase   contracts.Phase
	Reason  string
}
type Review struct {
	Record  Record
	Binding contracts.ApprovalBinding
	Held    *contracts.Artifact
	Live    *LiveCapture
}

func (c *Controller) Reviews() (out []Review, err error) {
	err = c.store.withLock(func() error {
		if e := c.guard(false); e != nil {
			return e
		}
		rs, e := c.records()
		if e != nil {
			return e
		}
		out = []Review{}
		for _, r := range rs {
			if r.Snapshot.SessionID != c.session {
				continue
			}
			offer, ok := c.offers[r.Snapshot.SnapshotID]
			if !ok || offer.Reason != r.Snapshot.Proposal.Reason || offer.Phase != r.Phase || offer.Binding.Revision != r.Revision || offer.Binding.Epoch != c.epoch || offer.Binding.ExpiresUnix <= c.cfg.Clock.Now().Unix() {
				offer = reviewOffer{contracts.ApprovalBinding{Version: 1, SessionID: c.session, SnapshotID: r.Snapshot.SnapshotID, Revision: r.Revision, Digest: r.Digest, Epoch: c.epoch, Nonce: randomID(), ExpiresUnix: c.cfg.Clock.Now().Add(c.cfg.ApprovalTTL).Unix()}, r.Phase, r.Snapshot.Proposal.Reason}
				c.offers[r.Snapshot.SnapshotID] = offer
			}
			v := Review{Record: r, Binding: offer.Binding}
			if r.Phase == contracts.Starting || r.Phase == contracts.Running {
				v.Live = c.copyLive(r.Snapshot.SnapshotID)
			}
			if r.Phase == contracts.OutputHeld {
				b, e := c.store.read("raw", r.Snapshot.SnapshotID)
				if e != nil {
					return e
				}
				var a contracts.Artifact
				if e = decode(b, &a); e != nil {
					return e
				}
				if a.Validate() != nil {
					return ErrGuard
				}
				v.Held = &a
			}
			out = append(out, v)
		}
		return nil
	})
	return
}

// ReviewAction validates the exact immutable offer and mutates under one lock.
// For run, it installs THAT binding as approval; Consume must follow synchronously
// before returning HTTP success. Consume independently rechecks all guards.
func (c *Controller) ReviewAction(a contracts.ApprovalBinding, action string, p *contracts.Proposal, artifact *contracts.Artifact) error {
	return c.store.withLock(func() error {
		if c.guard(true) != nil || a.Validate() != nil || a.SessionID != c.session || a.Epoch != c.epoch || a.ExpiresUnix <= c.cfg.Clock.Now().Unix() {
			return ErrGuard
		}
		r, e := c.load(a.SnapshotID)
		if e != nil {
			return e
		}
		offer, ok := c.offers[a.SnapshotID]
		if !ok || offer.Reason != r.Snapshot.Proposal.Reason || offer.Binding != a || offer.Phase != r.Phase || a.Revision != r.Revision || a.Digest != r.Digest {
			return ErrGuard
		}
		switch action {
		case "edit":
			if p == nil || p.Reason != r.Snapshot.Proposal.Reason || r.Phase != contracts.Reviewing {
				return ErrGuard
			}
			s, e := contracts.Freeze(*p, c.cfg.Shell)
			if e != nil {
				return e
			}
			if r.Revision == ^uint64(0) {
				return ErrGuard
			}
			r.Spec = s
			r.Digest, _ = s.Digest()
			r.Revision++
			r.CancelRequested = false
		case "run", "run_root":
			if r.Phase != contracts.Reviewing || len(r.Spent) >= 64 || (action == "run_root") != (r.Spec.User == contracts.Root) {
				return ErrGuard
			}
			rs, e := c.records()
			if e != nil {
				return e
			}
			for _, other := range rs {
				if other.Phase == contracts.Starting || other.Phase == contracts.Running {
					return ErrGuard
				}
			}
			r.Approval = &a
			r.Phase = contracts.Approved
			r.CancelRequested = false
		case "reject":
			if r.Phase != contracts.Reviewing {
				return ErrGuard
			}
			r.Phase = contracts.Denied
		case "cancel":
			switch r.Phase {
			case contracts.Reviewing, contracts.Approved:
				r.Phase = contracts.Stopped
				r.Approval = nil
			case contracts.Starting, contracts.Running:
				r.CancelRequested = true
			default:
				return ErrGuard
			}
		case "release":
			if artifact == nil || r.Phase != contracts.OutputHeld {
				return ErrGuard
			}
			b, e := c.store.read("raw", a.SnapshotID)
			if e != nil {
				return e
			}
			var original contracts.Artifact
			if e = decode(b, &original); e != nil {
				return e
			}
			if e = validateRelease(original, *artifact); e != nil {
				return e
			}
			if e = c.store.put("releases", artifact.ArtifactID, *artifact, true); e != nil {
				return e
			}
			r.ReleaseID = artifact.ArtifactID
			r.Phase = contracts.OutputReleased
		default:
			return ErrGuard
		}
		if e = c.save(r); e != nil {
			return e
		}
		delete(c.offers, a.SnapshotID)
		if r.CancelRequested {
			if cancel := c.executions[a.SnapshotID]; cancel != nil {
				cancel()
			}
		}
		return nil
	})
}

func validateRelease(original, a contracts.Artifact) error {
	if original.Validate() != nil || a.Validate() != nil {
		return ErrGuard
	}
	expected := original
	expected.ArtifactID = a.ArtifactID
	expected.RedactedFields = a.RedactedFields
	for _, f := range a.RedactedFields {
		switch f {
		case "executed.script":
			expected.Executed.Script = a.Executed.Script
		case "executed.cwd":
			expected.Executed.Cwd = a.Executed.Cwd
		case "stdout":
			expected.Stdout = a.Stdout
		case "stderr":
			expected.Stderr = a.Stderr
		}
	}
	x, _ := json.Marshal(expected)
	y, _ := json.Marshal(a)
	if !bytes.Equal(x, y) {
		return ErrGuard
	}
	return nil
}

// Tick enforces expiry and liveness independently of browser/guest activity.
func (c *Controller) Tick() error { return c.store.withLock(func() error { return c.guard(false) }) }
