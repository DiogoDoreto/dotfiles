package contracts

import (
	"bytes"
	"strings"
	"testing"
)

func TestSnapshot(t *testing.T) {
	req, err := ParseRequest(bytes.NewReader(fixture(t, "submit")))
	if err != nil {
		t.Fatal(err)
	}
	s := Snapshot{Version: Version, SessionID: req.SessionID, SnapshotID: strings.Repeat("3", 64), RequestID: req.RequestID, ImportedUnix: 100, Proposal: *req.Proposal}
	got, err := ParseSnapshot(bytes.NewReader(encoded(t, s)))
	if err != nil || got != s {
		t.Fatalf("snapshot: %v", err)
	}
	frozen, err := Freeze(s.Proposal, testShell)
	if err != nil {
		t.Fatal(err)
	}
	frozen.Script = "edited on host"
	if s.Proposal.Script == frozen.Script {
		t.Fatal("edit mutated snapshot")
	}
	s.SnapshotID = "../unsafe"
	if _, err := ParseSnapshot(bytes.NewReader(encoded(t, s))); err == nil {
		t.Fatal("unsafe ID")
	}
}
