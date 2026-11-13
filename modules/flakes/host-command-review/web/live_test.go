package web

import (
	"encoding/json"
	"host-command-review/contracts"
	"net/http/httptest"
	"strings"
	"testing"
)

func TestLiveViewUsesExistingAuthenticationNoStoreAndNonRenewingGET(t *testing.T) {
	s, m, token := fixture(t)
	m.view.Requests[0].Phase = contracts.Starting
	m.view.Requests[0].Live = &LiveCapture{RunID: "private-run", Sequence: 3, Stdout: []byte("private stdout"), Stderr: []byte{0xff, 0}, Truncated: true}
	login(t, s, token)
	if w := request(s, "POST", "/api/heartbeat", "{}", true); w.Code != 200 {
		t.Fatal(w.Code)
	}
	lease := m.lease
	if w := request(s, "GET", "/api/view", "", false); w.Code != 401 || strings.Contains(w.Body.String(), "private") {
		t.Fatal("unauthenticated live disclosure", w.Code)
	}
	r := httptest.NewRequest("GET", "/api/view", nil)
	r.Host = strings.TrimPrefix(s.origin, "http://")
	r.Header.Set("Authorization", "Bearer "+s.auth)
	r.Header.Set("X-Review-Tab", strings.Repeat("2", 64))
	w := httptest.NewRecorder()
	s.ServeHTTP(w, r)
	if w.Code != 409 || strings.Contains(w.Body.String(), "private") {
		t.Fatal("old tab live disclosure", w.Code)
	}
	w = request(s, "GET", "/api/view", "", true)
	if w.Code != 200 || w.Header().Get("Cache-Control") != "no-store" {
		t.Fatal(w.Code, w.Header())
	}
	var v View
	if err := json.Unmarshal(w.Body.Bytes(), &v); err != nil {
		t.Fatal(err)
	}
	live := v.Requests[0].Live
	if live == nil || live.RunID != "private-run" || live.Sequence != 3 || string(live.Stdout) != "private stdout" || string(live.Stderr) != string([]byte{0xff, 0}) || !live.Truncated {
		t.Fatalf("lost snapshot: %+v", live)
	}
	if !m.lease.Equal(lease) || len(m.ops) != 0 {
		t.Fatal("GET changed browser liveness or executed an operation")
	}
	m.view.Requests[0].Live = nil
	w = request(s, "GET", "/api/view", "", true)
	if strings.Contains(w.Body.String(), `"live"`) {
		t.Fatal("live field must be optional")
	}
}
