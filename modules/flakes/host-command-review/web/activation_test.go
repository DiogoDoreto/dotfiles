package web

import (
	"strings"
	"testing"
	"time"
)

func TestActivationIssuancePreservesBrowserAndBoundsBusy(t *testing.T) {
	s, m, token := fixture(t)
	login(t, s, token)
	if w := request(s, "POST", "/api/heartbeat", "{}", true); w.Code != 200 {
		t.Fatal(w.Code)
	}
	auth, csrf, lease := s.auth, s.csrf, m.lease
	url, err := s.BootstrapURL()
	if err != nil {
		t.Fatal(err)
	}
	// An opener failure or lost socket response must leave the old tab usable.
	if s.auth != auth || s.csrf != csrf || !m.lease.Equal(lease) {
		t.Fatal("issuing an undelivered URL changed browser authority")
	}
	if w := request(s, "GET", "/api/view", "", true); w.Code != 200 {
		t.Fatal("old browser lost access", w.Code)
	}
	s.mu.Lock()
	done := make(chan error, 1)
	go func() { _, err := s.BootstrapURL(); done <- err }()
	select {
	case err := <-done:
		if err == nil {
			t.Error("activation succeeded during lifecycle transition")
		}
	case <-time.After(time.Second):
		t.Error("activation blocked on lifecycle mutex")
	}
	s.mu.Unlock()
	login(t, s, strings.Split(url, "#")[1])
	if s.auth == auth || !m.lease.IsZero() {
		t.Fatal("exchange failed to revoke old authority")
	}
}

func TestExplicitActivationCanReplaceExpiredBrowserAuthentication(t *testing.T) {
	s, _, token := fixture(t)
	login(t, s, token)
	s.created = time.Now().Add(-5 * time.Hour)
	url, err := s.BootstrapURL()
	if err != nil {
		t.Fatal(err)
	}
	if w := request(s, "GET", "/api/view", "", true); w.Code != 401 {
		t.Fatal("issuance revived expired credentials", w.Code)
	}
	login(t, s, strings.Split(url, "#")[1])
	if time.Since(s.created) > time.Second {
		t.Fatal("explicit new authentication did not get a fresh bounded lifetime")
	}
}
