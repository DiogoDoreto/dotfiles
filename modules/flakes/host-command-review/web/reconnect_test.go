package web

import (
	"net/http/httptest"
	"strings"
	"testing"
	"time"
)

func tabRequest(s *Server, tab, path, body string) *httptest.ResponseRecorder {
	r := httptest.NewRequest("POST", s.origin+path, strings.NewReader(body))
	r.URL.Scheme = ""
	r.URL.Host = ""
	r.Header.Set("Origin", s.origin)
	r.Header.Set("Content-Type", "application/json")
	r.Header.Set("Authorization", "Bearer "+s.auth)
	r.Header.Set("X-CSRF-Token", s.csrf)
	r.Header.Set("X-Review-Tab", tab)
	w := httptest.NewRecorder()
	s.ServeHTTP(w, r)
	return w
}
func TestReloadAndDuplicateTabClaimRevokesLiveness(t *testing.T) {
	s, m, token := fixture(t)
	login(t, s, token)
	created := s.created
	first, second := strings.Repeat("1", 64), strings.Repeat("2", 64)
	claim := func(tab string) {
		t.Helper()
		if w := tabRequest(s, tab, "/api/claim", `{"tab":"`+tab+`"}`); w.Code != 200 {
			t.Fatal(w.Code)
		}
	}
	claim(first)
	if w := tabRequest(s, first, "/api/heartbeat", "{}"); w.Code != 200 {
		t.Fatal(w.Code)
	}
	if m.lease.IsZero() {
		t.Fatal("missing heartbeat")
	}
	claim(second) // a reloaded or duplicated tab uses the same bounded stored auth
	if !m.lease.IsZero() || !s.lease.IsZero() {
		t.Fatal("credentials implied liveness")
	}
	if w := tabRequest(s, first, "/api/heartbeat", "{}"); w.Code != 409 {
		t.Fatal("old tab renewed", w.Code)
	}
	if w := tabRequest(s, second, "/api/heartbeat", "{}"); w.Code != 200 {
		t.Fatal(w.Code)
	}
	if !created.Equal(s.created) {
		t.Fatal("reload extended auth lifetime")
	}
	s.created = time.Now().Add(-4 * time.Hour)
	if w := tabRequest(s, second, "/api/heartbeat", "{}"); w.Code != 401 {
		t.Fatal("auth cap extended", w.Code)
	}
}
func TestLocalBootstrapRotationDoesNotIssueOverHTTP(t *testing.T) {
	s, m, token := fixture(t)
	login(t, s, token)
	_ = request(s, "POST", "/api/heartbeat", "{}", true)
	auth := s.auth
	url, err := s.BootstrapURL()
	if err != nil {
		t.Fatal(err)
	}
	if s.auth != auth || m.lease.IsZero() {
		t.Fatal("undelivered activation revoked previous browser")
	}
	if w := request(s, "POST", "/api/bootstrap", `{"token":"`+token+`"}`, false); w.Code != 401 {
		t.Fatal("old bootstrap replay")
	}
	login(t, s, strings.Split(url, "#")[1])
	if s.auth == auth || !m.lease.IsZero() {
		t.Fatal("bootstrap exchange did not revoke previous browser")
	}
	if w := request(s, "GET", "/api/bootstrap", "", true); w.Code != 405 {
		t.Fatal("GET issued auth")
	}
}
