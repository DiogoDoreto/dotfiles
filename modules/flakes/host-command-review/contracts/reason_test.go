package contracts

import (
	"bytes"
	"encoding/json"
	"strings"
	"testing"
)

func TestReasonValidationAndDigestExclusion(t *testing.T) {
	p := Proposal{Reason: "original\nreason", Script: "true", Cwd: "/", User: Dog, TimeoutSeconds: 120}
	first, err := Freeze(p, testShell)
	if err != nil {
		t.Fatal(err)
	}
	d, _ := first.Digest()
	for _, reason := range []string{"other reason", strings.Repeat("é", 1024)} {
		p.Reason = reason
		next, err := Freeze(p, testShell)
		if err != nil {
			t.Fatal(err)
		}
		got, _ := next.Digest()
		if next != first || got != d {
			t.Fatal("reason entered executable spec/digest")
		}
	}
	for _, reason := range []string{"", " \n\t\u2003", strings.Repeat("é", 1024) + "a", "nul\x00", string([]byte{0xff})} {
		p.Reason = reason
		if p.Validate() == nil {
			t.Fatalf("accepted reason %q", reason)
		}
	}
	p.Reason = "valid"
	q := Request{Version: 1, Operation: Submit, SessionID: strings.Repeat("a", 64), RequestID: strings.Repeat("b", 64), Proposal: &p}
	data, _ := json.Marshal(q)
	for _, bad := range [][]byte{
		bytes.Replace(data, []byte(`"reason":"valid",`), nil, 1),
		bytes.Replace(data, []byte(`"reason":"valid"`), []byte(`"reason":null`), 1),
		bytes.Replace(data, []byte(`"reason":"valid"`), []byte(`"reason":"\ud800"`), 1),
		bytes.Replace(data, []byte(`"reason":"valid"`), []byte(`"reason":"valid","reason":"duplicate"`), 1),
		bytes.Replace(data, []byte("valid"), []byte{0xff}, 1),
	} {
		if _, err := ParseRequest(bytes.NewReader(bad)); err == nil {
			t.Fatalf("accepted invalid reason JSON %q", bad)
		}
	}
}
