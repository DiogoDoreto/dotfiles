package guest

import (
	"context"
	"strings"
	"testing"

	"host-command-review/transport"
)

func TestIndependentRejectedAliasMutationDoesNotReserveOrRedirectCodes(t *testing.T) {
	s := store(t)
	ctx := context.Background()
	announce := func(id, alias string) transport.Response {
		t.Helper()
		r, err := s.Exchange(ctx, transport.Request{Version: 1, Operation: "announce", SessionID: id, Label: "lapdog", Alias: alias})
		if err != nil {
			t.Fatal(err)
		}
		return r
	}
	if r := announce(sid, "ABCD-EFGH"); r.Error != "" {
		t.Fatal(r)
	}
	if r := announce(sid, "JKLM-NPQR"); r.Error != "conflict" {
		t.Fatalf("live identity changed alias: %+v", r)
	}
	if _, err := s.ResolveRoute(ctx, "jklmnpqr"); err == nil {
		t.Fatal("failed alias mutation reserved a route")
	}
	// The rejected mutation must not poison a legitimate subsequent allocation.
	nextID := strings.Repeat("e", 64)
	if r := announce(nextID, "JKLM-NPQR"); r.Error != "" {
		t.Fatalf("allocation after conflict failed: %+v", r)
	}
	if _, err := s.Exchange(ctx, transport.Request{Version: 1, Operation: "close", SessionID: sid}); err != nil {
		t.Fatal(err)
	}
	// Reopen the store, rather than relying on any in-memory alias cache.
	s = &Store{Dir: s.Dir}
	if r := announce(sid, "ABCD-EFGH"); r.Error != "session_expired" {
		t.Fatalf("closed identity reactivated: %+v", r)
	}
	if r := announce(strings.Repeat("f", 64), "ABCD-EFGH"); r.Error != "conflict" {
		t.Fatalf("closed alias recycled after reopen: %+v", r)
	}
	for input, want := range map[string]string{"abcd-efgh": sid, "JKLMNPQR": nextID} {
		got, err := s.ResolveRoute(ctx, input)
		if err != nil || got != want {
			t.Errorf("ResolveRoute(%q) = %q, %v; want %q", input, got, err, want)
		}
	}
	if _, err := s.ResolveRoute(ctx, "2345-6789"); err == nil {
		t.Fatal("unknown code fell back to active session")
	}
}
