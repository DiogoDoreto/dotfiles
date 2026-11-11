package contracts

import (
	"fmt"
	"io"
)

// Snapshot is the immutable host copy of the original guest proposal. Reviewed
// edits belong to the host ledger's ExecutionSpec, never to this object.
// ImportedUnix and SnapshotID are assigned by the host, not taken from guest data.
type Snapshot struct {
	Version      int      `json:"version"`
	SessionID    string   `json:"session_id"`
	SnapshotID   string   `json:"snapshot_id"`
	RequestID    string   `json:"request_id"`
	ImportedUnix int64    `json:"imported_unix"`
	Proposal     Proposal `json:"proposal"`
}

func (s Snapshot) Validate() error {
	if s.Version != Version || !idPattern.MatchString(s.SessionID) || !idPattern.MatchString(s.SnapshotID) || !idPattern.MatchString(s.RequestID) || s.ImportedUnix <= 0 {
		return fmt.Errorf("invalid snapshot identity")
	}
	return s.Proposal.Validate()
}

func ParseSnapshot(r io.Reader) (Snapshot, error) {
	var v Snapshot
	err := decode(r, MaxRequestBytes, &v)
	if err == nil {
		err = v.Validate()
	}
	if err != nil {
		return Snapshot{}, err
	}
	return v, nil
}
