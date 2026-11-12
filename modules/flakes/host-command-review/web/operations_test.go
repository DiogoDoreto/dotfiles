package web

import (
	"encoding/json"
	"host-command-review/contracts"
	"net/http/httptest"
	"strings"
	"testing"
)

func TestEditReleaseAndRootAreDistinct(t *testing.T) {
	s, m, token := fixture(t)
	login(t, s, token)
	_ = request(s, "POST", "/api/heartbeat", "{}", true)
	binding := m.view.Requests[0].Binding
	if w := operation(t, s, Operation{Action: "run_root", Binding: binding}); w.Code != 409 {
		t.Fatal("root action accepted for dog spec", w.Code)
	}
	p := contracts.Proposal{Reason: "Inspect host state", Script: "true", Cwd: "/", User: contracts.Root, TimeoutSeconds: contracts.DefaultTimeoutSeconds}
	if w := operation(t, s, Operation{Action: "edit", Binding: binding, Proposal: &p}); w.Code != 200 {
		t.Fatal(w.Code)
	}
	p.TimeoutSeconds = 1801
	if w := operation(t, s, Operation{Action: "edit", Binding: binding, Proposal: &p}); w.Code != 400 {
		t.Fatal(w.Code)
	}
	a := contracts.Artifact{Version: 1, ArtifactID: strings.Repeat("b", 64), SnapshotID: binding.SnapshotID, Revision: 1, ExecutionDigest: binding.Digest, Executed: m.view.Requests[0].Spec, Stdout: []byte("host secret"), Stderr: []byte{}, Exit: contracts.ExitInfo{Kind: "exited", Code: 0}, RedactedFields: []string{}}
	m.view.Requests[0].Phase = contracts.OutputHeld
	m.view.Requests[0].Held = &a
	before := len(m.ops)
	_ = request(s, "GET", "/api/view", "", true)
	if len(m.ops) != before {
		t.Fatal("reading released output")
	}
	a.Stdout = []byte("[REDACTED]")
	a.RedactedFields = []string{"stdout"}
	if w := operation(t, s, Operation{Action: "release", Binding: binding, Artifact: &a}); w.Code != 200 {
		t.Fatal(w.Code, w.Body.String())
	}
	got := m.ops[len(m.ops)-1].Artifact
	if string(got.Stdout) != "[REDACTED]" || got.ExecutionDigest != binding.Digest || len(got.RedactedFields) != 1 {
		t.Fatal("artifact altered")
	}
	// Transport validation is not publication authority: the real adapter must
	// additionally check phase, original identity, immutable fields, and markers.
	a.RedactedFields = []string{"executed.environment"}
	if w := operation(t, s, Operation{Action: "release", Binding: binding, Artifact: &a}); w.Code != 400 {
		t.Fatal(w.Code)
	}
}

func TestNestedStrictJSON(t *testing.T) {
	s, m, token := fixture(t)
	login(t, s, token)
	b, _ := json.Marshal(Operation{Action: "cancel", Binding: m.view.Requests[0].Binding})
	base := string(b)
	cases := []string{
		strings.Replace(base, `"revision":1`, `"revision":1e0`, 1),
		strings.Replace(base, `"revision":1`, `"revision":1.0`, 1),
		strings.Replace(base, `"revision":1`, `"revision":18446744073709551616`, 1),
		strings.Replace(base, `"revision":1`, `"Revision":1`, 1),
		strings.Replace(base, `"revision":1`, `"revision":1,"revision":1`, 1),
		strings.Replace(base, `"revision":1,`, ``, 1),
		strings.Replace(base, `"revision":1`, `"revision":null`, 1),
		strings.Replace(base, `"action":"cancel"`, `"action":"cancel","proposal":null`, 1),
	}
	for _, body := range cases {
		if w := request(s, "POST", "/api/operation", body, true); w.Code != 400 {
			t.Fatalf("%s => %d", body, w.Code)
		}
	}
}
func TestContentTypeAndDuplicateHeaders(t *testing.T) {
	s, _, token := fixture(t)
	login(t, s, token)
	for _, ct := range []string{"text/plain", "application/json; charset=utf-8", ""} {
		r := httptest.NewRequest("POST", "/api/heartbeat", strings.NewReader("{}"))
		r.Host = strings.TrimPrefix(s.origin, "http://")
		r.Header.Set("Origin", s.origin)
		r.Header.Set("Content-Type", ct)
		r.Header.Set("Authorization", "Bearer "+s.auth)
		r.Header.Set("X-CSRF-Token", s.csrf)
		w := httptest.NewRecorder()
		s.ServeHTTP(w, r)
		if w.Code != 415 {
			t.Fatal(ct, w.Code)
		}
	}
	for _, header := range []string{"Origin", "Authorization", "X-CSRF-Token"} {
		r := httptest.NewRequest("POST", "/api/heartbeat", strings.NewReader("{}"))
		r.Host = strings.TrimPrefix(s.origin, "http://")
		r.Header.Set("Origin", s.origin)
		r.Header.Set("Content-Type", "application/json")
		r.Header.Set("Authorization", "Bearer "+s.auth)
		r.Header.Set("X-CSRF-Token", s.csrf)
		r.Header.Add(header, r.Header.Get(header))
		w := httptest.NewRecorder()
		s.ServeHTTP(w, r)
		if w.Code == 200 {
			t.Fatal("duplicate header accepted", header)
		}
	}
}
func TestUnicodeEscapeValidation(t *testing.T) {
	for _, s := range []string{`{"token":"\ud83d\ude00"}`, `{"token":"\\ud800"}`, `{"token":"plain"}`} {
		if !validEscapes([]byte(s)) {
			t.Fatal(s)
		}
	}
	for _, s := range []string{`{"token":"\ud800x"}`, `{"token":"\ud800\u0061"}`, `{"token":"\udc00"}`} {
		if validEscapes([]byte(s)) {
			t.Fatal(s)
		}
	}
}
