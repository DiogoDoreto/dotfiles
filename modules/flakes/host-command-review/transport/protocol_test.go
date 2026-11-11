package transport

import (
	"encoding/json"
	"host-command-review/contracts"
	"strings"
	"testing"
)

func TestEnvelopeClosed(t *testing.T) {
	valid := `{"version":1,"operation":"announce","session_id":"` + strings.Repeat("a", 64) + `","label":"lapdog"}`
	for _, s := range []string{
		`{"version":1,"operation":"renew","session_id":"` + strings.Repeat("a", 64) + `","label":""}`,
		valid + `{}`, strings.Replace(valid, `"version":1`, `"version":1.0`, 1), strings.Replace(valid, `"label":"lapdog"`, `"label":null`, 1),
		strings.Replace(valid, `"label":"lapdog"`, `"label":"../bad"`, 1), strings.Replace(valid, `"operation":"announce"`, `"operation":"renew"`, 1),
		strings.Replace(valid, `"version":1`, `"version":1,"versi\u006fn":1`, 1), strings.Replace(valid, `"version":1`, `"Version":1`, 1),
		strings.Repeat("x", MaxRequestBytes+1),
	} {
		if _, err := ParseRequest(strings.NewReader(s)); err == nil {
			t.Fatal("accepted malformed envelope")
		}
	}
	q, err := ParseRequest(strings.NewReader(valid))
	if err != nil {
		t.Fatal(err)
	}
	b, _ := json.Marshal(q)
	if _, err = ParseRequest(strings.NewReader(string(b))); err != nil {
		t.Fatal(err)
	}
}
func TestResponseSessionBindingAndBounds(t *testing.T) {
	sid := strings.Repeat("a", 64)
	rid := strings.Repeat("b", 64)
	q := contracts.Request{Version: 1, Operation: contracts.Submit, SessionID: sid, RequestID: rid, Proposal: &contracts.Proposal{Script: "x", Cwd: "/", User: contracts.Dog, TimeoutSeconds: 120}}
	r := Response{Version: 1, SessionID: sid, Items: []Item{{Request: q}}, LeaseSeconds: 15}
	if err := r.Validate(); err != nil {
		t.Fatal(err)
	}
	r.Items = append(r.Items, r.Items[0])
	if r.Validate() == nil {
		t.Fatal("duplicate proposal")
	}
	r.Items = r.Items[:1]
	r.SessionID = rid
	if r.Validate() == nil {
		t.Fatal("wrong session")
	}
	r.SessionID = sid
	r.Error = "conflict"
	if r.Validate() == nil {
		t.Fatal("error with data")
	}
}
func FuzzEnvelope(f *testing.F) {
	f.Add(`{"version":1,"operation":"pull","session_id":"` + strings.Repeat("a", 64) + `"}`)
	f.Add(`null`)
	f.Fuzz(func(t *testing.T, s string) {
		q, err := ParseRequest(strings.NewReader(s))
		if err == nil && q.Validate() != nil {
			t.Fatal("parsed invalid")
		}
	})
}
