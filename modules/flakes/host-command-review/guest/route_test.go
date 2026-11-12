package guest

import (
	"context"
	"host-command-review/transport"
	"strings"
	"testing"
)

func TestAliasCollisionClosedRouteAndExactResolution(t *testing.T) {
	s := store(t)
	ctx := context.Background()
	q := transport.Request{Version: 1, Operation: "announce", SessionID: sid, Label: "lapdog", Alias: "ABCD-EFGH"}
	if r, e := s.Exchange(ctx, q); e != nil || r.Error != "" {
		t.Fatal(r, e)
	}
	for _, input := range []string{"abcd-efgh", "ABCDEFGH"} {
		id, e := s.ResolveRoute(ctx, input)
		if e != nil || id != sid {
			t.Fatal(id, e)
		}
	}
	if _, e := s.ResolveRoute(ctx, "ABCD"); e == nil {
		t.Fatal("prefix matched")
	}
	if r, e := s.Exchange(ctx, transport.Request{Version: 1, Operation: "close", SessionID: sid}); e != nil || r.Error != "" {
		t.Fatal(r, e)
	}
	q.SessionID = strings.Repeat("c", 64)
	if r, e := s.Exchange(ctx, q); e != nil || r.Error != "conflict" {
		t.Fatal("closed alias reused", r, e)
	}
	id, e := s.ResolveRoute(ctx, "abcd-efgh")
	if e != nil || id != sid {
		t.Fatal("stale alias redirected", id, e)
	}
}
