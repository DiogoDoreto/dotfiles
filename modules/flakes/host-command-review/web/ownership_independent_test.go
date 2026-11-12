package web

import (
	"encoding/json"
	"net/http/httptest"
	"strings"
	"testing"
)

func TestIndependentSupersededTabCannotReadOrMutate(t *testing.T) {
	s, m, token := fixture(t)
	login(t, s, token)
	oldTab, newTab := strings.Repeat("1", 64), strings.Repeat("2", 64)
	if w := tabRequest(s, oldTab, "/api/heartbeat", "{}"); w.Code != 200 {
		t.Fatal(w.Code)
	}
	if w := tabRequest(s, newTab, "/api/claim", `{"tab":"`+newTab+`"}`); w.Code != 200 {
		t.Fatal(w.Code)
	}
	b, err := json.Marshal(Operation{Action: "reject", Binding: m.view.Requests[0].Binding})
	if err != nil {
		t.Fatal(err)
	}
	for _, path := range []string{"/api/view", "/api/heartbeat", "/api/operation"} {
		t.Run(path, func(t *testing.T) {
			method, body := "POST", "{}"
			if path == "/api/view" {
				method, body = "GET", ""
			}
			if path == "/api/operation" {
				body = string(b)
			}
			r := httptest.NewRequest(method, "/"+strings.TrimPrefix(path, "/"), strings.NewReader(body))
			r.Host = strings.TrimPrefix(s.origin, "http://")
			r.Header.Set("Origin", s.origin)
			r.Header.Set("Content-Type", "application/json")
			r.Header.Set("Authorization", "Bearer "+s.auth)
			r.Header.Set("X-CSRF-Token", s.csrf)
			r.Header.Set("X-Review-Tab", oldTab)
			w := httptest.NewRecorder()
			s.ServeHTTP(w, r)
			if w.Code != 409 {
				t.Fatalf("superseded tab %s = %d", path, w.Code)
			}
		})
	}
	if len(m.ops) != 0 || !m.lease.IsZero() {
		t.Fatal("old tab retained backend authority")
	}
	// A claim itself does not confer permission to run; heartbeat is mandatory.
	b, _ = json.Marshal(Operation{Action: "run", Binding: m.view.Requests[0].Binding})
	if w := tabRequest(s, newTab, "/api/operation", string(b)); w.Code != 409 {
		t.Fatalf("claim authorized execution: %d", w.Code)
	}
	if w := tabRequest(s, newTab, "/api/heartbeat", "{}"); w.Code != 200 {
		t.Fatal(w.Code)
	}
	if w := tabRequest(s, newTab, "/api/operation", string(b)); w.Code != 200 {
		t.Fatalf("fresh owner could not run: %d", w.Code)
	}
}

func TestIndependentMalformedClaimDoesNotEvictLiveOwner(t *testing.T) {
	s, m, token := fixture(t)
	login(t, s, token)
	if w := request(s, "POST", "/api/heartbeat", "{}", true); w.Code != 200 {
		t.Fatal(w.Code)
	}
	lease := m.lease
	for _, body := range []string{
		`{"tab":"` + strings.Repeat("z", 64) + `"}`,
		`{"tab":"` + strings.Repeat("2", 63) + `"}`,
		`{"tab":"` + strings.Repeat("2", 64) + `","tab":"` + strings.Repeat("3", 64) + `"}`,
		`{"tab":"` + strings.Repeat("2", 64) + `","extra":true}`,
	} {
		if w := request(s, "POST", "/api/claim", body, true); w.Code != 400 {
			t.Fatalf("malformed claim accepted: %d", w.Code)
		}
		if !m.lease.Equal(lease) {
			t.Fatal("invalid claim revoked live owner")
		}
		if w := request(s, "GET", "/api/view", "", true); w.Code != 200 {
			t.Fatalf("owner evicted by invalid claim: %d", w.Code)
		}
	}
}

func TestIndependentLocalActivationRejectsCapturedCredentialsAndUnusedFragments(t *testing.T) {
	s, _, token := fixture(t)
	login(t, s, token)
	// Capture an actual request: request() deliberately reads the current server
	// credentials, so using it after rotation would not test old authentication.
	stale := httptest.NewRequest("GET", "/api/view", nil)
	stale.Host = strings.TrimPrefix(s.origin, "http://")
	stale.Header.Set("Authorization", "Bearer "+s.auth)
	stale.Header.Set("X-Review-Tab", s.tab)
	first, err := s.BootstrapURL()
	if err != nil {
		t.Fatal(err)
	}
	second, err := s.BootstrapURL()
	if err != nil {
		t.Fatal(err)
	}
	if w := request(s, "POST", "/api/bootstrap", `{"token":"`+strings.Split(first, "#")[1]+`"}`, false); w.Code != 401 {
		t.Fatalf("superseded unused fragment accepted: %d", w.Code)
	}
	login(t, s, strings.Split(second, "#")[1])
	w := httptest.NewRecorder()
	s.ServeHTTP(w, stale)
	if w.Code != 401 {
		t.Fatalf("captured browser credential survived activation: %d", w.Code)
	}
	if w := request(s, "GET", "/api/view", "", true); w.Code != 200 {
		t.Fatalf("fresh credential unusable: %d", w.Code)
	}
}
