// Package transport defines the fixed SSH executable's data-only envelope.
// Everything returned by the guest is untrusted, even after Validate succeeds.
package transport

import (
	"bytes"
	"encoding/json"
	"fmt"
	"host-command-review/contracts"
	"io"
	"regexp"
)

const (
	Version                = 1
	MaxRequestBytes        = contracts.MaxArtifactBytes + (64 << 10)
	MaxResponseBytes       = MaxPendingPerSession*contracts.MaxRequestBytes + (64 << 10)
	MaxPendingPerSession   = 100
	MaxLiveSessions        = 16
	LeaseSeconds           = 15
	ExchangeTimeoutSeconds = 10
)

var id = regexp.MustCompile(`^[0-9a-f]{64}$`)
var label = regexp.MustCompile(`^[a-zA-Z0-9 ._-]{1,64}$`)

func ValidID(s string) bool { return id.MatchString(s) }

// Request uses closed verb-specific fields. Publish carries ONLY a contract
// Reply: approval, raw capture and host ledger data have no representation.
type Request struct {
	Version   int              `json:"version"`
	Operation string           `json:"operation"`
	SessionID string           `json:"session_id"`
	Label     string           `json:"label,omitempty"`
	Reply     *contracts.Reply `json:"reply,omitempty"`
}
type Item struct {
	Request         contracts.Request `json:"request"`
	CancelRequested bool              `json:"cancel_requested"`
	Acknowledged    bool              `json:"acknowledged"`
}
type Response struct {
	Version      int    `json:"version"`
	SessionID    string `json:"session_id"`
	Error        string `json:"error,omitempty"`
	LeaseSeconds int    `json:"lease_seconds"`
	Items        []Item `json:"items"`
}

func (r Request) Validate() error {
	if r.Version != Version || !ValidID(r.SessionID) {
		return fmt.Errorf("invalid envelope")
	}
	switch r.Operation {
	case "announce":
		if !label.MatchString(r.Label) || r.Reply != nil {
			return fmt.Errorf("invalid announcement")
		}
	case "renew", "pull", "close":
		if r.Label != "" || r.Reply != nil {
			return fmt.Errorf("unexpected payload")
		}
	case "publish":
		if r.Label != "" || r.Reply == nil || r.Reply.SessionID != r.SessionID {
			return fmt.Errorf("invalid publication")
		}
		if err := r.Reply.Validate(); err != nil {
			return err
		}
	default:
		return fmt.Errorf("unknown operation")
	}
	return nil
}
func (r Response) Validate() error {
	if r.Version != Version || !ValidID(r.SessionID) || r.Items == nil || len(r.Items) > MaxPendingPerSession || r.LeaseSeconds < 0 || r.LeaseSeconds > LeaseSeconds {
		return fmt.Errorf("invalid response")
	}
	switch r.Error {
	case "", "session_expired", "not_found", "conflict", "queue_full", "invalid_request":
	default:
		return fmt.Errorf("invalid error")
	}
	if r.Error != "" && (len(r.Items) != 0 || r.LeaseSeconds != 0) {
		return fmt.Errorf("error with payload")
	}
	seen := map[string]bool{}
	for _, item := range r.Items {
		q := item.Request
		if q.Validate() != nil || q.Operation != contracts.Submit || q.SessionID != r.SessionID || seen[q.RequestID] {
			return fmt.Errorf("invalid proposal")
		}
		seen[q.RequestID] = true
	}
	return nil
}
func ParseRequest(in io.Reader) (Request, error) {
	var r Request
	b, err := io.ReadAll(io.LimitReader(in, MaxRequestBytes+1))
	if err != nil {
		return r, err
	}
	if len(b) > MaxRequestBytes {
		return r, fmt.Errorf("envelope exceeds bound")
	}
	err = DecodeStrict(bytes.NewReader(b), MaxRequestBytes, &r)
	if err == nil {
		err = r.Validate()
	}
	if err == nil {
		var fields map[string]json.RawMessage
		if err = json.Unmarshal(b, &fields); err == nil {
			if _, present := fields["label"]; present && r.Operation != "announce" {
				err = fmt.Errorf("label only allowed on announce")
			}
		}
	}
	if err != nil {
		return Request{}, err
	}
	return r, nil
}
func ParseResponse(in io.Reader) (Response, error) {
	var r Response
	err := DecodeStrict(in, MaxResponseBytes, &r)
	if err == nil {
		err = r.Validate()
	}
	if err != nil {
		return Response{}, err
	}
	return r, nil
}
