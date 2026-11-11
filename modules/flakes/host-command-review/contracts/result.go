package contracts

import "fmt"

// CoarseStatus is deliberately less informative than host Phase. It never
// includes edits, execution metadata, errors from a process, or output bytes.
type CoarseStatus string

const (
	Pending   CoarseStatus = "pending"
	Active    CoarseStatus = "active"
	Held      CoarseStatus = "held"
	Released  CoarseStatus = "released"
	Rejected  CoarseStatus = "rejected"
	Cancelled CoarseStatus = "cancelled"
	Uncertain CoarseStatus = "uncertain"
)

// Reply is guest-visible. Artifact is absent until explicitly released.
// Error codes are fixed coarse protocol outcomes, never arbitrary diagnostics.
type Reply struct {
	Version   int          `json:"version"`
	SessionID string       `json:"session_id"`
	RequestID string       `json:"request_id"`
	Status    CoarseStatus `json:"status"`
	Error     string       `json:"error,omitempty"`
	Artifact  *Artifact    `json:"artifact,omitempty"`
}

// Artifact is an immutable explicit disclosure. Execution metadata may contain
// secrets too. Stdout/Stderr encode arbitrary bytes using JSON base64 strings.
// RedactedFields identifies removed or edited fields; redaction creates a new
// artifact before publication, never mutates a published artifact.
type Artifact struct {
	Version         int           `json:"version"`
	ArtifactID      string        `json:"artifact_id"`
	SnapshotID      string        `json:"snapshot_id"`
	Revision        uint64        `json:"revision"`
	ExecutionDigest string        `json:"execution_digest"`
	Executed        ExecutionSpec `json:"executed"`
	Stdout          []byte        `json:"stdout"`
	Stderr          []byte        `json:"stderr"`
	Exit            ExitInfo      `json:"exit"`
	Truncated       bool          `json:"truncated"`
	RedactedFields  []string      `json:"redacted_fields"`
}

// Redacted Executed fields are represented by reviewable replacement text;
// ExecutionDigest always refers to the ORIGINAL frozen spec, not a redaction.
// A release redacting a field that cannot hold replacement text must instead
// redact the whole release outside v1 (do not send it). v1 allows only these
// text/byte fields: executed.script, executed.cwd, stdout, stderr.
type ExitInfo struct {
	Kind   string `json:"kind"`   // exited, signaled, timed_out, cancelled, unknown
	Code   int    `json:"code"`   // 0..255 for exited, otherwise -1
	Signal int    `json:"signal"` // 1..64 for signaled, otherwise 0
}

func (a Artifact) Validate() error {
	if a.Version != Version || !idPattern.MatchString(a.ArtifactID) || !idPattern.MatchString(a.SnapshotID) || !idPattern.MatchString(a.ExecutionDigest) || a.Revision == 0 {
		return fmt.Errorf("invalid artifact identity")
	}
	if err := a.Executed.Validate(); err != nil {
		return err
	}
	if a.Stdout == nil || a.Stderr == nil || a.RedactedFields == nil || len(a.Stdout)+len(a.Stderr) > CaptureBytes {
		return fmt.Errorf("invalid artifact capture")
	}
	seen := map[string]bool{}
	metadataRedacted := false
	for _, f := range a.RedactedFields {
		switch f {
		case "executed.script", "executed.cwd":
			metadataRedacted = true
		case "stdout", "stderr":
		default:
			return fmt.Errorf("unknown redaction field")
		}
		if seen[f] {
			return fmt.Errorf("duplicate redaction field")
		}
		seen[f] = true
	}
	if !metadataRedacted {
		digest, err := a.Executed.Digest()
		if err != nil || digest != a.ExecutionDigest {
			return fmt.Errorf("execution digest mismatch")
		}
	}
	switch a.Exit.Kind {
	case "exited":
		if a.Exit.Code < 0 || a.Exit.Code > 255 || a.Exit.Signal != 0 {
			return fmt.Errorf("invalid exit code")
		}
	case "signaled":
		if a.Exit.Code != -1 || a.Exit.Signal < 1 || a.Exit.Signal > 64 {
			return fmt.Errorf("invalid signal")
		}
	case "timed_out", "cancelled", "unknown":
		if a.Exit.Code != -1 || a.Exit.Signal != 0 {
			return fmt.Errorf("invalid exit outcome")
		}
	default:
		return fmt.Errorf("unknown exit kind")
	}
	return nil
}

func (r Reply) Validate() error {
	if r.Version != Version || !idPattern.MatchString(r.SessionID) || !idPattern.MatchString(r.RequestID) {
		return fmt.Errorf("invalid reply identity")
	}
	switch r.Status {
	case Pending, Active, Held, Released, Rejected, Cancelled, Uncertain:
	default:
		return fmt.Errorf("unknown status")
	}
	switch r.Error {
	case "":
	case "no_reviewer", "session_expired", "invalid_request", "queue_full", "not_found", "conflict":
		if r.Status != Rejected || r.Artifact != nil {
			return fmt.Errorf("errors require rejected status")
		}
	default:
		return fmt.Errorf("unknown error code")
	}
	if r.Status == Released {
		if r.Artifact == nil {
			return fmt.Errorf("released requires artifact")
		}
		return r.Artifact.Validate()
	}
	if r.Artifact != nil {
		return fmt.Errorf("artifact withheld until release")
	}
	return nil
}
