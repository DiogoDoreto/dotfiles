// Package web is a host-private loopback reviewer, not a guest transport or executor.
package web

import (
	"context"
	"host-command-review/contracts"
	"time"
)

// Backend is authoritative. Implementations must serialize revision/epoch/digest/
// nonce checks, one-command admission (including root authentication), durable
// nonce consumption, and release publication. Never export View through guest RPC.
// No operation may return raw output to a guest; only an explicitly published
// contracts.Artifact may cross that boundary. See ../docs/state-machine.md.
type Backend interface {
	View(context.Context) (View, error)
	Apply(context.Context, Operation) error
	// Reviewer updates the authenticated browser lease. The backend MUST recheck
	// this deadline at approval AND consumption, not just at HTTP request entry.
	// A zero deadline revokes liveness. Already consumed work may finish.
	Reviewer(context.Context, time.Time) error
}

type View struct {
	SessionID   string        `json:"session_id"`
	Requests    []RequestView `json:"requests"`
	RoutingCode string        `json:"routing_code"`
	ExpiresUnix int64         `json:"expires_unix"`
	State       string        `json:"state"`
	Transport   string        `json:"transport"`
}

type RequestView struct {
	SnapshotID string                  `json:"snapshot_id"`
	Reason     string                  `json:"reason"` // guest-controlled, never HTML
	Phase      contracts.Phase         `json:"phase"`
	Spec       contracts.ExecutionSpec `json:"spec"`
	// Binding is minted by the host for this exact review, never by JavaScript.
	Binding contracts.ApprovalBinding `json:"binding"`
	// Held is host-private original capture, NOT a guest Reply. It remains held
	// until a separately reviewed immutable artifact is explicitly published.
	Held *contracts.Artifact `json:"held,omitempty"`
}

type Operation struct {
	Action   string                    `json:"action"` // edit, run, run_root, cancel, reject, release, end
	Binding  contracts.ApprovalBinding `json:"binding"`
	Proposal *contracts.Proposal       `json:"proposal,omitempty"`
	Artifact *contracts.Artifact       `json:"artifact,omitempty"`
}

func (o Operation) valid() bool {
	if o.Action == "end" || o.Action == "start" || o.Action == "restart" || o.Action == "quit" {
		return o.Binding == (contracts.ApprovalBinding{}) && o.Proposal == nil && o.Artifact == nil
	}
	if o.Binding.Validate() != nil {
		return false
	}
	switch o.Action {
	case "edit":
		return o.Proposal != nil && o.Proposal.Validate() == nil && o.Artifact == nil
	case "run", "run_root", "cancel", "reject":
		return o.Proposal == nil && o.Artifact == nil
	case "release":
		return o.Proposal == nil && o.Artifact != nil && o.Artifact.Validate() == nil
	}
	return false
}
