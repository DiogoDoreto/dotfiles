package web

import (
	"context"
	"encoding/json"
	"errors"
	"host-command-review/contracts"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
	"time"
)

type mockBackend struct {
	view  View
	lease time.Time
	ops   []Operation
	fail  bool
}

func (m *mockBackend) View(context.Context) (View, error)            { return m.view, nil }
func (m *mockBackend) Reviewer(_ context.Context, t time.Time) error { m.lease = t; return nil }
func (m *mockBackend) Apply(_ context.Context, o Operation) error {
	if m.fail {
		return errors.New("HOST SECRET: password")
	}
	if o.Action != "end" && o.Binding != m.view.Requests[0].Binding {
		return errors.New("stale")
	}
	if o.Action == "run" || o.Action == "run_root" {
		if !time.Now().Before(m.lease) {
			return errors.New("disconnected")
		}
		if (o.Action == "run_root") != (m.view.Requests[0].Spec.User == contracts.Root) {
			return errors.New("wrong user")
		}
		// Mock authoritative single-use behavior, NOT supplied by the HTTP layer.
		m.view.Requests[0].Binding.Nonce = strings.Repeat("e", 64)
	}
	m.ops = append(m.ops, o)
	return nil
}
func fixture(t *testing.T) (*Server, *mockBackend, string) {
	t.Helper()
	id := strings.Repeat("a", 64)
	spec, err := contracts.Freeze(contracts.Proposal{Reason: "Inspect host state", Script: "echo '<script>secret</script>'", Cwd: "/home/dog", User: contracts.Dog, TimeoutSeconds: 120}, "/nix/store/"+strings.Repeat("a", 32)+"-bash/bin/bash")
	if err != nil {
		t.Fatal(err)
	}
	digest, _ := spec.Digest()
	m := &mockBackend{view: View{SessionID: id, Requests: []RequestView{{SnapshotID: id, Reason: "<img src=x onerror=alert(1)>\x1b[31m", Phase: contracts.Reviewing, Spec: spec, Binding: contracts.ApprovalBinding{Version: 1, SessionID: id, SnapshotID: id, Revision: 1, Digest: digest, Nonce: id, Epoch: 1, ExpiresUnix: time.Now().Add(time.Minute).Unix()}}}}}
	s, url, err := New(m)
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = s.Close() })
	return s, m, strings.Split(url, "#")[1]
}
func request(s *Server, method, path, body string, auth bool) *httptest.ResponseRecorder {
	r := httptest.NewRequest(method, s.origin+path, strings.NewReader(body))
	r.URL.Scheme = ""
	r.URL.Host = ""
	if method == "POST" {
		r.Header.Set("Origin", s.origin)
		r.Header.Set("Content-Type", "application/json")
	}
	if auth {
		r.Header.Set("Authorization", "Bearer "+s.auth)
		r.Header.Set("X-CSRF-Token", s.csrf)
		r.Header.Set("X-Review-Tab", s.tab)
	}
	w := httptest.NewRecorder()
	s.ServeHTTP(w, r)
	return w
}
func login(t *testing.T, s *Server, token string) {
	t.Helper()
	w := request(s, "POST", "/api/bootstrap", `{"token":"`+token+`"}`, false)
	if w.Code != 200 {
		t.Fatal(w.Code, w.Body.String())
	}
	if w := request(s, "POST", "/api/claim", `{"tab":"`+strings.Repeat("1", 64)+`"}`, true); w.Code != 200 {
		t.Fatal(w.Code)
	}
}
func operation(t *testing.T, s *Server, o Operation) *httptest.ResponseRecorder {
	t.Helper()
	b, e := json.Marshal(o)
	if e != nil {
		t.Fatal(e)
	}
	return request(s, "POST", "/api/operation", string(b), true)
}

func TestBootstrapAuthenticationAndReplay(t *testing.T) {
	s, _, token := fixture(t)
	if w := request(s, "GET", "/api/view", "", true); w.Code != 401 {
		t.Fatal(w.Code)
	}
	login(t, s, token)
	if w := request(s, "POST", "/api/bootstrap", `{"token":"`+token+`"}`, false); w.Code != 401 {
		t.Fatal(w.Code)
	}
	if w := request(s, "GET", "/api/view", "", false); w.Code != 401 {
		t.Fatal(w.Code)
	}
	w := request(s, "GET", "/api/view", "", true)
	if w.Code != 200 || strings.Contains(w.Body.String(), "<img") {
		t.Fatal(w.Code, w.Body.String())
	}
	if w.Header().Get("Set-Cookie") != "" {
		t.Fatal("header-only authentication")
	}
}
func TestHostOriginCSRF(t *testing.T) {
	s, _, token := fixture(t)
	login(t, s, token)
	for _, tc := range []struct{ name, host, origin, csrf, site string }{
		{"rebind", "attacker.invalid", s.origin, s.csrf, ""},
		{"host-suffix", strings.TrimPrefix(s.origin, "http://") + ".evil", s.origin, s.csrf, ""},
		{"localhost-alias", "localhost:123", s.origin, s.csrf, ""},
		{"cross-origin", "", "https://evil.invalid", s.csrf, ""},
		{"null-origin", "", "null", s.csrf, ""},
		{"missing-origin", "", "", s.csrf, ""},
		{"missing-csrf", "", s.origin, "", ""},
		{"wrong-csrf", "", s.origin, strings.Repeat("0", 64), ""},
		{"fetch-site", "", s.origin, s.csrf, "cross-site"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			r := httptest.NewRequest("POST", "/api/heartbeat", strings.NewReader("{}"))
			r.Host = strings.TrimPrefix(s.origin, "http://")
			if tc.host != "" {
				r.Host = tc.host
			}
			r.Header.Set("Origin", tc.origin)
			r.Header.Set("Content-Type", "application/json")
			r.Header.Set("Authorization", "Bearer "+s.auth)
			r.Header.Set("X-CSRF-Token", tc.csrf)
			r.Header.Set("Sec-Fetch-Site", tc.site)
			w := httptest.NewRecorder()
			s.ServeHTTP(w, r)
			if w.Code != 403 {
				t.Fatal(w.Code)
			}
		})
	}
	// Exchange itself is also origin-protected (even before authentication).
	fresh, _, tok := fixture(t)
	r := httptest.NewRequest("POST", "/api/bootstrap", strings.NewReader(`{"token":"`+tok+`"}`))
	r.Host = strings.TrimPrefix(fresh.origin, "http://")
	r.Header.Set("Content-Type", "application/json")
	w := httptest.NewRecorder()
	fresh.ServeHTTP(w, r)
	if w.Code != 403 {
		t.Fatal(w.Code)
	}
}
func TestStrictJSONAndBounds(t *testing.T) {
	for _, body := range []string{`{"Token":"x"}`, `{"token":null}`, `{"token":"a","token":"b"}`, `{"token":"a","\u0074oken":"b"}`, `{"token":"\ud800"}`, `{"token":"\udc00"}`, `{"token":"a"} {}`, `{}`, `{"token":"x","extra":1}`, `{"token":"` + strings.Repeat("x", 1100) + `"}`, "{\"token\":\"\xff\"}"} {
		s, _, _ := fixture(t)
		if w := request(s, "POST", "/api/bootstrap", body, false); w.Code != 400 {
			t.Fatalf("%q => %d", body, w.Code)
		}
	}
	s, _, token := fixture(t)
	login(t, s, token)
	if w := request(s, "POST", "/api/operation", strings.Repeat(" ", maxBody+1), true); w.Code != 400 {
		t.Fatal(w.Code)
	}
	for _, body := range []string{`{"action":"end","binding":null}`, `{"action":"end"}`, `{"action":"run","binding":{}}`} {
		if w := request(s, "POST", "/api/operation", body, true); w.Code != 400 {
			t.Fatal(w.Code)
		}
	}
}
func TestExactBindingLivenessAndSingleUse(t *testing.T) {
	s, m, token := fixture(t)
	login(t, s, token)
	op := Operation{Action: "run", Binding: m.view.Requests[0].Binding}
	if w := operation(t, s, op); w.Code != 409 {
		t.Fatal("approval without heartbeat", w.Code)
	}
	if w := request(s, "POST", "/api/heartbeat", "{}", true); w.Code != 200 {
		t.Fatal(w.Code)
	}
	for _, field := range []string{"revision", "digest", "nonce", "epoch"} {
		stale := op
		switch field {
		case "revision":
			stale.Binding.Revision++
		case "digest":
			stale.Binding.Digest = strings.Repeat("b", 64)
		case "nonce":
			stale.Binding.Nonce = strings.Repeat("c", 64)
		case "epoch":
			stale.Binding.Epoch++
		}
		if w := operation(t, s, stale); w.Code != 409 {
			t.Fatalf("stale %s: %d", field, w.Code)
		}
	}
	if len(m.ops) != 0 {
		t.Fatal("stale binding accepted")
	}
	if w := operation(t, s, op); w.Code != 200 {
		t.Fatal(w.Code)
	}
	if m.ops[0].Binding != op.Binding {
		t.Fatal("binding rewritten by web")
	}
	if w := operation(t, s, op); w.Code != 409 {
		t.Fatal("replay accepted", w.Code)
	}
	s.lease = time.Now().Add(-time.Second)
	if w := operation(t, s, op); w.Code != 409 {
		t.Fatal(w.Code)
	}
	// Read polling never renews the lease.
	before := m.lease
	_ = request(s, "GET", "/api/view", "", true)
	if m.lease != before {
		t.Fatal("GET renewed lease")
	}
}
func TestExpiryAndEnd(t *testing.T) {
	s, _, token := fixture(t)
	s.created = time.Now().Add(-BootstrapLifetime - time.Second)
	if w := request(s, "POST", "/api/bootstrap", `{"token":"`+token+`"}`, false); w.Code != 401 {
		t.Fatal(w.Code)
	}
	s, m, token := fixture(t)
	login(t, s, token)
	_ = request(s, "POST", "/api/heartbeat", "{}", true)
	if w := operation(t, s, Operation{Action: "end"}); w.Code != 200 {
		t.Fatal(w.Code)
	}
	if !m.lease.IsZero() {
		t.Fatal("lease not revoked")
	}
	if w := request(s, "GET", "/api/view", "", true); w.Code != 200 {
		t.Fatal("application auth must survive session end", w.Code)
	}
	s, _, token = fixture(t)
	login(t, s, token)
	s.created = time.Now().Add(-4 * time.Hour)
	if w := request(s, "GET", "/api/view", "", true); w.Code != 401 {
		t.Fatal(w.Code)
	}
}
func TestSecurityHeadersAssetsAndMethods(t *testing.T) {
	s, m, token := fixture(t)
	login(t, s, token)
	for _, path := range []string{"/", "/app.js", "/style.css", "/api/view", "/missing"} {
		w := request(s, "GET", path, "", true)
		for k, v := range map[string]string{"Cache-Control": "no-store", "X-Content-Type-Options": "nosniff", "Referrer-Policy": "no-referrer", "X-Frame-Options": "DENY"} {
			if w.Header().Get(k) != v {
				t.Fatal(path, k)
			}
		}
		csp := w.Header().Get("Content-Security-Policy")
		if strings.Contains(csp, "unsafe-inline") || !strings.Contains(csp, "frame-ancestors 'none'") {
			t.Fatal(csp)
		}
	}
	js, _ := assets.ReadFile("assets/app.js")
	for _, bad := range []string{"innerHTML", "insertAdjacentHTML", "localStorage.", "eval("} {
		if strings.Contains(string(js), bad) {
			t.Fatal(bad)
		}
	}
	if w := request(s, "GET", "/api/operation", "", true); w.Code != 405 {
		t.Fatal(w.Code)
	}
	if w := request(s, "GET", "/api/view?token=secret", "", true); w.Code != 403 {
		t.Fatal(w.Code)
	}
	_ = request(s, "POST", "/api/heartbeat", "{}", true)
	m.fail = true
	w := operation(t, s, Operation{Action: "run", Binding: m.view.Requests[0].Binding})
	if w.Code != 409 || strings.Contains(w.Body.String(), "SECRET") {
		t.Fatal(w.Code, w.Body.String())
	}
}
func TestConstructorLoopbackAndRealHTTP(t *testing.T) {
	s, _, _ := fixture(t)
	if !strings.HasPrefix(s.listener.Addr().String(), "127.0.0.1:") {
		t.Fatal(s.listener.Addr())
	}
	done := make(chan error, 1)
	go func() { done <- s.Serve() }()
	response, err := http.Get(s.origin + "/")
	if err != nil {
		t.Fatal(err)
	}
	_ = response.Body.Close()
	if response.StatusCode != 200 {
		t.Fatal(response.StatusCode)
	}
	_ = s.Close()
	<-done
}
