package web

import (
	"crypto/rand"
	"encoding/hex"
	"errors"
	"time"
)

func browserTokens() (bootstrap, auth, csrf string, err error) {
	for _, p := range []*string{&bootstrap, &auth, &csrf} {
		var b [32]byte
		if _, err = rand.Read(b[:]); err != nil {
			return "", "", "", err
		}
		*p = hex.EncodeToString(b[:])
	}
	return
}

// BootstrapURL is for the permission-protected local control socket only, never
// an HTTP endpoint. Issuance preserves existing browser authority: only successful
// exchange revokes it, so failed delivery cannot lock out the existing browser.
func (s *Server) BootstrapURL() (string, error) {
	bootstrap, _, _, err := browserTokens()
	if err != nil {
		return "", err
	}
	// Lifecycle HTTP operations may hold mu while cancellation/reaping completes.
	// Activation fails promptly rather than outliving its local socket deadline.
	if !s.mu.TryLock() {
		return "", errors.New("reviewer busy")
	}
	defer s.mu.Unlock()
	if s.ended {
		return "", errors.New("reviewer closed")
	}
	s.bootstrap = bootstrap
	s.bootstrapIssued = time.Now()
	return s.origin + "/#" + bootstrap, nil
}
