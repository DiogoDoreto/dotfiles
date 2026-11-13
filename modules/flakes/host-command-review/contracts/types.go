// Package contracts defines the v1 data boundary, not authorization or execution.
// Never unmarshal untrusted data directly: use the bounded Parse functions.
package contracts

import (
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"path"
	"regexp"
	"strings"
	"unicode/utf8"
)

const (
	Version                  = 1
	MaxRequestBytes          = 512 << 10 // includes worst-case JSON escaping of script/cwd
	MaxArtifactBytes         = 8 << 20   // base64 capture plus metadata
	MaxReasonBytes           = 2048
	MaxScriptBytes           = 64 << 10
	CaptureBytes             = 4 << 20
	DefaultTimeoutSeconds    = 120
	MaxTimeoutSeconds        = 1800
	StopGraceSeconds         = 5
	MaxSessionSeconds        = 4 * 60 * 60
	CrashRawRetentionSeconds = 24 * 60 * 60
	ReleasedRetentionSeconds = 7 * 24 * 60 * 60
)

// IDs are 32 random bytes, lowercase hex. SessionID is routing, NOT a credential.
// RequestID is guest-chosen and untrusted. SnapshotID is host-generated.
var idPattern = regexp.MustCompile(`^[0-9a-f]{64}$`)
var shellPattern = regexp.MustCompile(`^/nix/store/[0-9a-z]{32}-[A-Za-z0-9+._?=-]+/bin/bash$`)

type User string

const (
	Dog  User = "dog"
	Root User = "root"
)

// Proposal is guest input, copied into a host-private snapshot before review.
// It cannot set shell, environment, capture limit, or execution authority.
type Proposal struct {
	Reason         string `json:"reason"`
	Script         string `json:"script"`
	Cwd            string `json:"cwd"`
	User           User   `json:"user"`
	TimeoutSeconds int    `json:"timeout_seconds"`
}

type Operation string

const (
	Submit Operation = "submit"
	Check  Operation = "check"
	Cancel Operation = "cancel"
)

// One request per fixed guest executable invocation. Proposal exists only on submit.
type Request struct {
	Version   int       `json:"version"`
	Operation Operation `json:"operation"`
	SessionID string    `json:"session_id"`
	RequestID string    `json:"request_id"`
	Proposal  *Proposal `json:"proposal,omitempty"`
}

// ExecutionSpec is the complete reviewed execution input. Do not add runtime
// defaults after approval. Shell is a resolved immutable Nix store bash path.
// Its argv is fixed by unreleased v1: --noprofile --norc /proc/self/fd/3.
// Script bytes are an immutable unlinked file inherited on FD3, NEVER argv or env.
// stdin is /dev/null. $0/BASH_SOURCE name /proc/self/fd/3, not a -c invocation.
type ExecutionSpec struct {
	Version          int         `json:"version"`
	Script           string      `json:"script"`
	Cwd              string      `json:"cwd"`
	User             User        `json:"user"`
	Shell            string      `json:"shell"`
	Environment      Environment `json:"environment"`
	TimeoutSeconds   int         `json:"timeout_seconds"`
	StopGraceSeconds int         `json:"stop_grace_seconds"`
	CaptureBytes     int         `json:"capture_bytes"`
}

// A closed struct, not inherited process environment; no BASH_ENV, ENV, or loader variables.
type Environment struct {
	PATH string `json:"PATH"`
	HOME string `json:"HOME"`
	LANG string `json:"LANG"`
}

func CleanEnvironment(user User) Environment {
	home := "/home/dog"
	searchPath := "/etc/profiles/per-user/dog/bin:/home/dog/.nix-profile/bin:/run/current-system/sw/bin"
	if user == Root {
		home = "/root"
		// Never resolve root commands through dog-writable package profiles.
		searchPath = "/run/current-system/sw/bin"
	}
	return Environment{PATH: searchPath, HOME: home, LANG: "C.UTF-8"}
}

func Freeze(p Proposal, shell string) (ExecutionSpec, error) {
	if err := p.Validate(); err != nil {
		return ExecutionSpec{}, err
	}
	s := ExecutionSpec{Version: Version, Script: p.Script, Cwd: p.Cwd, User: p.User,
		Shell: shell, Environment: CleanEnvironment(p.User), TimeoutSeconds: p.TimeoutSeconds,
		StopGraceSeconds: StopGraceSeconds, CaptureBytes: CaptureBytes}
	return s, s.Validate()
}

func (p Proposal) Validate() error {
	if strings.TrimSpace(p.Reason) == "" || len(p.Reason) > MaxReasonBytes || !utf8.ValidString(p.Reason) || strings.ContainsRune(p.Reason, 0) {
		return fmt.Errorf("reason must be nonblank UTF-8, at most %d bytes, without NUL", MaxReasonBytes)
	}
	return p.validateExecutionFields()
}

func (p Proposal) validateExecutionFields() error {
	if p.Script == "" || len(p.Script) > MaxScriptBytes || !utf8.ValidString(p.Script) || strings.ContainsRune(p.Script, 0) {
		return fmt.Errorf("script must be 1..%d bytes without NUL", MaxScriptBytes)
	}
	if len(p.Cwd) > 4096 || !utf8.ValidString(p.Cwd) || !path.IsAbs(p.Cwd) || path.Clean(p.Cwd) != p.Cwd || strings.ContainsRune(p.Cwd, 0) {
		return fmt.Errorf("cwd must be a clean absolute path")
	}
	if p.User != Dog && p.User != Root {
		return fmt.Errorf("user must be dog or root")
	}
	if p.TimeoutSeconds < 1 || p.TimeoutSeconds > MaxTimeoutSeconds {
		return fmt.Errorf("timeout out of range")
	}
	return nil
}

func (r Request) Validate() error {
	if r.Version != Version || !idPattern.MatchString(r.SessionID) || !idPattern.MatchString(r.RequestID) {
		return fmt.Errorf("invalid version or routing IDs")
	}
	switch r.Operation {
	case Submit:
		if r.Proposal == nil {
			return fmt.Errorf("submit requires proposal")
		}
		return r.Proposal.Validate()
	case Check, Cancel:
		if r.Proposal != nil {
			return fmt.Errorf("proposal only allowed for submit")
		}
	default:
		return fmt.Errorf("unknown operation")
	}
	return nil
}

func (s ExecutionSpec) Validate() error {
	if err := (Proposal{Script: s.Script, Cwd: s.Cwd, User: s.User, TimeoutSeconds: s.TimeoutSeconds}).validateExecutionFields(); err != nil {
		return err
	}
	wantEnvironment := CleanEnvironment(s.User)
	// Persisted v1 dog specs used a system-only PATH. Keep accepting that
	// exact policy without rewriting approved inputs or their digests. New
	// Freeze calls always use the current package-profile policy.
	legacyEnvironment := wantEnvironment
	legacyEnvironment.PATH = "/run/current-system/sw/bin"
	validEnvironment := s.Environment == wantEnvironment || (s.User == Dog && s.Environment == legacyEnvironment)
	if s.Version != Version || len(s.Shell) > 4096 || !shellPattern.MatchString(s.Shell) || !validEnvironment || s.StopGraceSeconds != StopGraceSeconds || s.CaptureBytes != CaptureBytes {
		return fmt.Errorf("invalid execution policy")
	}
	return nil
}

// Digest is SHA-256 over encoding/json's compact struct encoding, in declared
// field order, with its default HTML escaping, no newline. Not input JSON bytes.
// This is a v1 canonicalization rule, not RFC 8785. Changing it requires v2.
func (s ExecutionSpec) Digest() (string, error) {
	if err := s.Validate(); err != nil {
		return "", err
	}
	b, err := json.Marshal(s)
	if err != nil {
		return "", err
	}
	sum := sha256.Sum256(b)
	return hex.EncodeToString(sum[:]), nil
}

// ApprovalBinding is host-only. A valid shape is NOT permission to execute.
// The host must additionally check the current record, browser/session liveness,
// suspend epoch, expiry, and atomically consume Nonce BEFORE starting anything.
type ApprovalBinding struct {
	Version     int    `json:"version"`
	SessionID   string `json:"session_id"`
	SnapshotID  string `json:"snapshot_id"`
	Revision    uint64 `json:"revision"`
	Digest      string `json:"digest"`
	Nonce       string `json:"nonce"`
	Epoch       uint64 `json:"epoch"`
	ExpiresUnix int64  `json:"expires_unix"`
}

func (a ApprovalBinding) Validate() error {
	if a.Version != Version || !idPattern.MatchString(a.SessionID) || !idPattern.MatchString(a.SnapshotID) || !idPattern.MatchString(a.Digest) || !idPattern.MatchString(a.Nonce) || a.Revision == 0 || a.Epoch == 0 || a.ExpiresUnix <= 0 {
		return fmt.Errorf("invalid approval binding")
	}
	return nil
}
