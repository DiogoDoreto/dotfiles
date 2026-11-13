package host

import (
	"bytes"
	"context"
	"crypto/rand"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"strconv"
	"strings"
	"sync"
	"syscall"
	"time"

	"host-command-review/contracts"
	"host-command-review/execution"
)

var ErrGuard = errors.New("host authority guard failed")
var ErrConflict = errors.New("request conflict")
var ErrFull = errors.New("session outstanding-work capacity reached")

func randomID() string {
	var b [32]byte
	if _, e := rand.Read(b[:]); e != nil {
		panic(e)
	}
	return hex.EncodeToString(b[:])
}

type Clock interface {
	Now() time.Time
	BootElapsed() (time.Duration, error)
}
type systemClock struct{}

func (systemClock) Now() time.Time { return time.Now() }
func (systemClock) BootElapsed() (time.Duration, error) {
	b, e := os.ReadFile("/proc/uptime")
	if e != nil {
		return 0, e
	}
	f := strings.Fields(string(b))
	if len(f) < 1 {
		return 0, ErrGuard
	}
	v, e := strconv.ParseFloat(f[0], 64)
	return time.Duration(v * float64(time.Second)), e
}

type Config struct {
	Shell                string
	MaxPendingPerSession int
	BrowserLease         time.Duration
	ApprovalTTL          time.Duration
	Clock                Clock
}
type Record struct {
	Version         int                         `json:"version"`
	Snapshot        contracts.Snapshot          `json:"snapshot"`
	Revision        uint64                      `json:"revision"`
	Phase           contracts.Phase             `json:"phase"`
	Spec            contracts.ExecutionSpec     `json:"spec"`
	Digest          string                      `json:"digest"`
	Approval        *contracts.ApprovalBinding  `json:"approval"`
	Spent           []contracts.ApprovalBinding `json:"spent"`
	CancelRequested bool                        `json:"cancel_requested"`
	ReleaseID       string                      `json:"release_id"`
}

func (r Record) validate() error {
	if r.Version != 1 || r.Revision == 0 || r.Spent == nil {
		return ErrGuard
	}
	if e := r.Snapshot.Validate(); e != nil {
		return e
	}
	d, e := r.Spec.Digest()
	if e != nil || d != r.Digest {
		return ErrGuard
	}
	if _, e = r.Phase.Coarse(); e != nil {
		return e
	}
	if (r.Phase == contracts.Approved) != (r.Approval != nil) {
		return ErrGuard
	}
	if r.Approval != nil {
		a := r.Approval
		if a.Validate() != nil || a.SnapshotID != r.Snapshot.SnapshotID || a.SessionID != r.Snapshot.SessionID || a.Revision != r.Revision || a.Digest != r.Digest {
			return ErrGuard
		}
	}
	seen := map[string]bool{}
	for _, a := range r.Spent {
		if a.Validate() != nil || a.SnapshotID != r.Snapshot.SnapshotID || a.SessionID != r.Snapshot.SessionID || seen[a.Nonce] {
			return ErrGuard
		}
		seen[a.Nonce] = true
	}
	if r.Approval != nil && seen[r.Approval.Nonce] {
		return ErrGuard
	}
	if r.Phase == contracts.Starting || r.Phase == contracts.Running || r.Phase == contracts.OutputHeld || r.Phase == contracts.OutputReleased {
		if len(r.Spent) == 0 {
			return ErrGuard
		}
		last := r.Spent[len(r.Spent)-1]
		if last.Digest != r.Digest || last.Revision != r.Revision {
			return ErrGuard
		}
	}
	if (r.Phase == contracts.OutputReleased) != validID(r.ReleaseID) {
		return ErrGuard
	}
	return nil
}

type Controller struct {
	store               *Store
	cfg                 Config
	session             string
	epoch               uint64
	started, last       time.Time
	bootStart, lastBoot time.Duration
	lease               time.Time
	ended               bool
	reviewer            *os.File
	executions          map[string]context.CancelFunc
	offers              map[string]reviewOffer
	liveMu              sync.Mutex
	live                map[string]*LiveCapture
}

// NewController takes the singleton reviewer lock and recovers dead-session
// intents as ambiguous. It NEVER reconnects or replays an old execution.
func NewController(s *Store, cfg Config) (c *Controller, err error) {
	if cfg.Clock == nil {
		cfg.Clock = systemClock{}
	}
	if cfg.MaxPendingPerSession == 0 {
		cfg.MaxPendingPerSession = 100
	}
	if cfg.MaxPendingPerSession < 1 || cfg.MaxPendingPerSession > 1024 {
		return nil, ErrGuard
	}
	if cfg.BrowserLease == 0 {
		cfg.BrowserLease = 2 * time.Second
	}
	if cfg.ApprovalTTL == 0 {
		cfg.ApprovalTTL = 30 * time.Second
	}
	if cfg.BrowserLease <= 0 || cfg.BrowserLease > 5*time.Second || cfg.ApprovalTTL <= 0 || cfg.ApprovalTTL > time.Minute {
		return nil, ErrGuard
	}
	if _, e := contracts.Freeze(contracts.Proposal{Reason: "Inspect host state", Script: "true", Cwd: "/", User: contracts.Dog, TimeoutSeconds: 120}, cfg.Shell); e != nil {
		return nil, e
	}
	fd, e := syscall.Openat(int(s.root.Fd()), "reviewer.lock", syscall.O_RDWR|syscall.O_CREAT|syscall.O_NOFOLLOW|syscall.O_CLOEXEC, 0600)
	if e != nil {
		return nil, e
	}
	f := os.NewFile(uintptr(fd), "reviewer.lock")
	defer func() {
		if err != nil {
			f.Close()
		}
	}()
	if e = checkFD(fd, false); e != nil {
		return nil, e
	}
	if e = syscall.Flock(fd, syscall.LOCK_EX|syscall.LOCK_NB); e != nil {
		return nil, e
	}
	boot, e := cfg.Clock.BootElapsed()
	if e != nil {
		return nil, e
	}
	now := cfg.Clock.Now()
	c = &Controller{store: s, cfg: cfg, session: randomID(), epoch: 1, started: now, last: now, bootStart: boot, lastBoot: boot, reviewer: f, executions: map[string]context.CancelFunc{}, offers: map[string]reviewOffer{}}
	err = s.withLock(func() error {
		// Clean interrupted immutable hard-link publications before checking
		// their expected single-link final inode in records/snapshots.
		for _, dir := range []string{"records", "snapshots", "raw", "releases"} {
			if _, e := s.names(dir); e != nil {
				return e
			}
		}
		rs, e := c.records()
		if e != nil {
			return e
		}
		for _, r := range rs {
			switch r.Phase {
			case contracts.Starting, contracts.Running:
				r.Phase = contracts.Ambiguous
			case contracts.Approved:
				r.Phase = contracts.Reviewing
				r.Approval = nil
			default:
				continue
			}
			if e = c.save(r); e != nil {
				return e
			}
		}
		return nil
	})
	if err != nil {
		return nil, err
	}
	if err = c.Sweep(false); err != nil {
		return nil, err
	}
	return c, nil
}
func (c *Controller) SessionID() string  { return c.session }
func (c *Controller) ExpiresUnix() int64 { return c.started.Add(4 * time.Hour).Unix() }
func (c *Controller) records() ([]Record, error) {
	ids, e := c.store.names("records")
	if e != nil {
		return nil, e
	}
	rs := make([]Record, 0, len(ids))
	for _, id := range ids {
		r, e := c.load(id)
		if e != nil {
			return nil, e
		}
		rs = append(rs, r)
	}
	return rs, nil
}
func (c *Controller) load(id string) (Record, error) {
	var r Record
	b, e := c.store.read("records", id)
	if e != nil {
		return r, e
	}
	if e = decode(b, &r); e != nil {
		return r, e
	}
	if e = r.validate(); e != nil {
		return r, e
	}
	if r.Snapshot.SnapshotID != id {
		return r, ErrGuard
	}
	b, e = c.store.read("snapshots", id)
	if e != nil {
		return r, e
	}
	snap, e := contracts.ParseSnapshot(bytes.NewReader(b))
	if e != nil || snap != r.Snapshot {
		return r, ErrGuard
	}
	return r, nil
}
func (c *Controller) save(r Record) error {
	if e := r.validate(); e != nil {
		return e
	}
	if r.Phase != contracts.Starting && r.Phase != contracts.Running {
		c.clearLive(r.Snapshot.SnapshotID, "")
	}
	return c.store.put("records", r.Snapshot.SnapshotID, r, false)
}

// guard checks wall, monotonic and suspend-inclusive clocks. An explicit suspend
// hook should still call Invalidate before suspend and before serving after resume.
func (c *Controller) guard(browser bool) error {
	now := c.cfg.Clock.Now()
	boot, e := c.cfg.Clock.BootElapsed()
	if e != nil {
		c.ended = true
		for _, cancel := range c.executions {
			cancel()
		}
		return ErrGuard
	}
	if boot < c.lastBoot || now.Before(c.last) || boot-c.lastBoot-now.Sub(c.last) > time.Second || (!c.lease.IsZero() && !now.Before(c.lease)) {
		if e := c.invalidateLocked(); e != nil {
			return e
		}
	}
	c.last = now
	c.lastBoot = boot
	if c.ended || boot-c.bootStart >= 4*time.Hour || now.Sub(c.started) >= 4*time.Hour || now.Before(c.started) {
		c.ended = true
		for _, cancel := range c.executions {
			cancel()
		}
		return ErrGuard
	}
	if browser && !now.Before(c.lease) {
		return ErrGuard
	}
	return nil
}

// Heartbeat is called ONLY by an authenticated browser session. No routing ID
// can authenticate it. Disconnect revokes even an otherwise unexpired lease.
func (c *Controller) Heartbeat() error {
	return c.HeartbeatUntil(c.cfg.Clock.Now().Add(c.cfg.BrowserLease))
}
func (c *Controller) HeartbeatUntil(deadline time.Time) error {
	return c.store.withLock(func() error {
		if e := c.guard(false); e != nil {
			return e
		}
		c.lease = c.cfg.Clock.Now().Add(c.cfg.BrowserLease)
		if deadline.Before(c.lease) {
			c.lease = deadline
		}
		return nil
	})
}
func (c *Controller) Disconnect() error { return c.Invalidate() }
func (c *Controller) Invalidate() error {
	return c.store.withLock(func() error { _ = c.guard(false); return c.invalidateLocked() })
}
func (c *Controller) invalidateLocked() error {
	c.epoch++
	c.lease = time.Time{}
	rs, e := c.records()
	if e != nil {
		return e
	}
	for _, r := range rs {
		if r.Snapshot.SessionID == c.session && r.Phase == contracts.Approved {
			r.Phase = contracts.Reviewing
			r.Approval = nil
			if e = c.save(r); e != nil {
				return e
			}
		}
	}
	return nil
}

// Import receives a parsed value, never a guest filename. Retry equality is the
// original validated proposal, not the editable spec. Session IDs never revive.
func (c *Controller) Import(req contracts.Request) (out Record, err error) {
	err = c.store.withLock(func() error {
		if req.Validate() != nil || req.Operation != contracts.Submit {
			return ErrGuard
		}
		if req.SessionID != c.session || c.guard(false) != nil {
			return ErrGuard
		}
		rs, e := c.records()
		if e != nil {
			return e
		}
		for _, r := range rs {
			if r.Snapshot.SessionID == req.SessionID && r.Snapshot.RequestID == req.RequestID {
				if r.Snapshot.Proposal != *req.Proposal {
					return ErrConflict
				}
				out = r
				return nil
			}
		}
		pending := 0
		for _, r := range rs {
			if r.Snapshot.SessionID != c.session {
				continue
			}
			switch r.Phase {
			case contracts.Reviewing, contracts.Approved, contracts.Starting, contracts.Running, contracts.OutputHeld:
				pending++
			}
		}
		if pending >= c.cfg.MaxPendingPerSession {
			return ErrFull
		}
		spec, e := contracts.Freeze(*req.Proposal, c.cfg.Shell)
		if e != nil {
			return e
		}
		d, _ := spec.Digest()
		snap := contracts.Snapshot{Version: 1, SessionID: c.session, SnapshotID: randomID(), RequestID: req.RequestID, ImportedUnix: c.cfg.Clock.Now().Unix(), Proposal: *req.Proposal}
		if e = c.store.put("snapshots", snap.SnapshotID, snap, true); e != nil {
			return e
		}
		out = Record{Version: 1, Snapshot: snap, Revision: 1, Phase: contracts.Reviewing, Spec: spec, Digest: d, Spent: []contracts.ApprovalBinding{}}
		return c.save(out)
	})
	return
}
func (c *Controller) View(id string) (out Record, err error) {
	err = c.store.withLock(func() error { var e error; out, e = c.load(id); return e })
	return
}
func (c *Controller) editGuard(r Record, revision uint64) error {
	if c.guard(true) != nil || r.Snapshot.SessionID != c.session || r.Revision != revision {
		return ErrGuard
	}
	return nil
}
func (c *Controller) Edit(id string, revision uint64, p contracts.Proposal) error {
	return c.store.withLock(func() error {
		r, e := c.load(id)
		if e != nil {
			return e
		}
		if p.Reason != r.Snapshot.Proposal.Reason || c.editGuard(r, revision) != nil || (r.Phase != contracts.Reviewing && r.Phase != contracts.Approved) {
			return ErrGuard
		}
		if r.Revision == ^uint64(0) {
			return ErrGuard
		}
		s, e := contracts.Freeze(p, c.cfg.Shell)
		if e != nil {
			return e
		}
		r.Spec = s
		r.Digest, _ = s.Digest()
		r.Revision++
		r.Phase = contracts.Reviewing
		r.Approval = nil
		r.CancelRequested = false
		return c.save(r)
	})
}
func (c *Controller) Approve(id string, revision uint64, digest string) (a contracts.ApprovalBinding, err error) {
	err = c.store.withLock(func() error {
		r, e := c.load(id)
		if e != nil {
			return e
		}
		if c.editGuard(r, revision) != nil || r.Phase != contracts.Reviewing || digest != r.Digest || len(r.Spent) >= 64 {
			return ErrGuard
		}
		a = contracts.ApprovalBinding{Version: 1, SessionID: c.session, SnapshotID: id, Revision: r.Revision, Digest: r.Digest, Nonce: randomID(), Epoch: c.epoch, ExpiresUnix: c.cfg.Clock.Now().Add(c.cfg.ApprovalTTL).Unix()}
		r.Approval = &a
		r.CancelRequested = false
		r.Phase = contracts.Approved
		return c.save(r)
	})
	return
}

// Consume's returned spec is the ONLY execution input. No I/O outside the ledger
// may start until this method returns success, including directory fsync.
func (c *Controller) Consume(a contracts.ApprovalBinding) (spec contracts.ExecutionSpec, err error) {
	err = c.store.withLock(func() error {
		rs, e := c.records()
		if e != nil {
			return e
		}
		for _, r := range rs {
			if r.Phase == contracts.Starting || r.Phase == contracts.Running {
				return ErrGuard
			}
		}
		r, e := c.load(a.SnapshotID)
		if e != nil {
			return e
		}
		if c.guard(true) != nil || a.Validate() != nil || a.SessionID != c.session || a.Epoch != c.epoch || a.ExpiresUnix <= c.cfg.Clock.Now().Unix() || r.Phase != contracts.Approved || r.Approval == nil || *r.Approval != a {
			return ErrGuard
		}
		d, e := r.Spec.Digest()
		if e != nil || d != a.Digest {
			return ErrGuard
		}
		r.Phase = contracts.Starting
		r.Spent = append(r.Spent, a)
		r.Approval = nil
		if e = c.save(r); e != nil {
			return e
		}
		spec = r.Spec
		return nil
	})
	return
}

// Cancel serializes against Consume. true means a supervisor stop is REQUIRED;
// it is not confirmation of cancellation. Submit/stop races can be uncertain.
func (c *Controller) Cancel(session, request string) (stop bool, err error) {
	err = c.store.withLock(func() error {
		rs, e := c.records()
		if e != nil {
			return e
		}
		for _, r := range rs {
			if r.Snapshot.SessionID != session || r.Snapshot.RequestID != request {
				continue
			}
			switch r.Phase {
			case contracts.Reviewing, contracts.Approved:
				r.Phase = contracts.Stopped
				r.Approval = nil
			case contracts.Starting, contracts.Running:
				r.CancelRequested = true
				stop = true
			default:
				return nil
			}
			e = c.save(r)
			if stop {
				if cancel := c.executions[r.Snapshot.SnapshotID]; cancel != nil {
					cancel()
				}
			}
			return e
		}
		return os.ErrNotExist
	})
	return
}
func (c *Controller) Reject(id string, revision uint64) error {
	return c.store.withLock(func() error {
		r, e := c.load(id)
		if e != nil {
			return e
		}
		if c.editGuard(r, revision) != nil || r.Phase != contracts.Reviewing {
			return ErrGuard
		}
		r.Phase = contracts.Denied
		return c.save(r)
	})
}

// Reconcile is trusted runner-adapter input, NEVER guest/UI input. Only a framed
// helper outcome after confirmed cgroup deactivation may supply an artifact.
func (c *Controller) Reconcile(id string, event contracts.Event, artifact *contracts.Artifact) error {
	return c.store.withLock(func() error {
		r, e := c.load(id)
		if e != nil {
			return e
		}
		switch event {
		case contracts.Started, contracts.AuthDenied, contracts.LoseExecution:
			if artifact != nil {
				return ErrGuard
			}
		case contracts.Finish:
			if artifact == nil {
				return ErrGuard
			}
		default:
			return ErrGuard
		}
		next, e := contracts.Next(r.Phase, event)
		if e != nil {
			return e
		}
		if artifact != nil {
			a := *artifact
			if a.Validate() != nil || a.SnapshotID != id || a.Revision != r.Revision || a.ExecutionDigest != r.Digest || a.Executed != r.Spec || len(a.RedactedFields) != 0 {
				return ErrGuard
			}
			if e = c.store.put("raw", id, a, true); e != nil {
				return e
			}
		}
		r.Phase = next
		return c.save(r)
	})
}

// Release requires a complete reviewed artifact. No silent metadata replacement:
// every changed allowed field must be explicitly marked, all other fields fixed.
func (c *Controller) Release(id string, revision uint64, a contracts.Artifact) error {
	return c.store.withLock(func() error {
		r, e := c.load(id)
		if e != nil {
			return e
		}
		if c.editGuard(r, revision) != nil || r.Phase != contracts.OutputHeld {
			return ErrGuard
		}
		b, e := c.store.read("raw", id)
		if e != nil {
			return e
		}
		var original contracts.Artifact
		if e = decode(b, &original); e != nil {
			return e
		}
		if original.Validate() != nil || a.Validate() != nil {
			return ErrGuard
		}
		expected := original
		expected.ArtifactID = a.ArtifactID
		expected.RedactedFields = a.RedactedFields
		for _, f := range a.RedactedFields {
			switch f {
			case "executed.script":
				expected.Executed.Script = a.Executed.Script
			case "executed.cwd":
				expected.Executed.Cwd = a.Executed.Cwd
			case "stdout":
				expected.Stdout = a.Stdout
			case "stderr":
				expected.Stderr = a.Stderr
			}
		}
		x, _ := json.Marshal(expected)
		y, _ := json.Marshal(a)
		if !bytes.Equal(x, y) {
			return ErrGuard
		}
		if e = c.store.put("releases", a.ArtifactID, a, true); e != nil {
			return e
		}
		r.ReleaseID = a.ArtifactID
		r.Phase = contracts.OutputReleased
		return c.save(r)
	})
}

// StatusBytes is the sole guest disclosure path. Identical releases return the
// same stored artifact encoding. No raw directory listing is an allowlist.
func (c *Controller) StatusBytes(session, request string) (out []byte, err error) {
	err = c.store.withLock(func() error {
		rs, e := c.records()
		if e != nil {
			return e
		}
		reply := contracts.Reply{Version: 1, SessionID: session, RequestID: request, Status: contracts.Rejected, Error: "not_found"}
		for _, r := range rs {
			if r.Snapshot.SessionID != session || r.Snapshot.RequestID != request {
				continue
			}
			reply.Error = ""
			reply.Status, _ = r.Phase.Coarse()
			if r.Phase == contracts.OutputReleased {
				b, e := c.store.read("releases", r.ReleaseID)
				if e != nil {
					return e
				}
				var a contracts.Artifact
				if e = decode(b, &a); e != nil {
					return e
				}
				if a.Validate() != nil || a.SnapshotID != r.Snapshot.SnapshotID || a.ExecutionDigest != r.Digest {
					return ErrGuard
				}
				reply.Artifact = &a
			}
			break
		}
		if e = reply.Validate(); e != nil {
			return e
		}
		out, e = json.Marshal(reply)
		return e
	})
	return
}

// Sweep does not extend ages on polling. Backward wall-clock movement expires
// private data conservatively. Active intents are never garbage-collected.
func (c *Controller) Sweep(cleanEnd bool) error {
	return c.store.withLock(func() error {
		rs, e := c.records()
		if e != nil {
			return e
		}
		now := c.cfg.Clock.Now().Unix()
		live := map[string]bool{}
		released := map[string]bool{}
		for _, r := range rs {
			age := now - r.Snapshot.ImportedUnix
			active := r.Phase == contracts.Starting || r.Phase == contracts.Running
			if cleanEnd && active {
				return fmt.Errorf("stop and reconcile execution before clean end")
			}
			// A clean end (including idle cleanup after recovery) has no route
			// capable of reviewing a dead session's private capture. Remove those
			// leftovers too, rather than retaining secrets until the age limit.
			if !active && (cleanEnd || age < 0 || age >= contracts.CrashRawRetentionSeconds) {
				if e = c.store.remove("raw", r.Snapshot.SnapshotID); e != nil {
					return e
				}
			}
			if !active && (age < 0 || age >= contracts.ReleasedRetentionSeconds) {
				if e = c.store.remove("records", r.Snapshot.SnapshotID); e != nil {
					return e
				}
				continue
			}
			live[r.Snapshot.SnapshotID] = true
			if r.ReleaseID != "" {
				released[r.ReleaseID] = true
			}
		}
		for _, dir := range []string{"snapshots", "releases", "raw"} {
			ids, e := c.store.names(dir)
			if e != nil {
				return e
			}
			for _, id := range ids {
				keep := live[id]
				if dir == "releases" {
					keep = released[id]
				}
				if !keep {
					if e = c.store.remove(dir, id); e != nil {
						return e
					}
				}
			}
		}
		return nil
	})
}

// BeginEnd permanently revokes admission and cancels managed work without
// deleting capture or releasing the singleton. Call again after joining runs to
// reconcile auth-cancelled reviewing records before final publication.
func (c *Controller) BeginEnd() error {
	c.store.mu.Lock()
	c.ended = true
	for _, cancel := range c.executions {
		cancel()
	}
	c.store.mu.Unlock()
	if e := c.Invalidate(); e != nil {
		return e
	}
	if e := c.store.withLock(func() error {
		rs, e := c.records()
		if e != nil {
			return e
		}
		for _, r := range rs {
			if r.Snapshot.SessionID == c.session && r.Phase == contracts.Reviewing {
				r.Phase = contracts.Stopped
				if e = c.save(r); e != nil {
					return e
				}
			}
		}
		return nil
	}); e != nil {
		return e
	}
	return nil
}

// End must be called after stop/reap/reconciliation. If any intent is still live
// it fails without claiming cleanup; the supervisor's leases remain mandatory.
func (c *Controller) End() error {
	if err := c.BeginEnd(); err != nil {
		return err
	}
	if e := c.Sweep(true); e != nil {
		return e
	}
	return c.store.withLock(func() error {
		c.ended = true
		if c.reviewer != nil {
			e := c.reviewer.Close()
			c.reviewer = nil
			return e
		}
		return nil
	})
}

// CloseUnclean releases only the singleton lock, for fatal shutdown/recovery.
func (c *Controller) CloseUnclean() error {
	c.store.mu.Lock()
	defer c.store.mu.Unlock()
	c.ended = true
	c.lease = time.Time{}
	for _, cancel := range c.executions {
		cancel()
	}
	if c.reviewer != nil {
		e := c.reviewer.Close()
		c.reviewer = nil
		return e
	}
	return nil
}

// List is host-private, not a guest status endpoint.
func (c *Controller) List() (out []Record, err error) {
	err = c.store.withLock(func() error { var e error; out, e = c.records(); return e })
	return
}
func (c *Controller) Held(id string) (out contracts.Artifact, err error) {
	err = c.store.withLock(func() error {
		r, e := c.load(id)
		if e != nil {
			return e
		}
		if r.Phase != contracts.OutputHeld {
			return ErrGuard
		}
		b, e := c.store.read("raw", id)
		if e != nil {
			return e
		}
		if e = decode(b, &out); e != nil {
			return e
		}
		return out.Validate()
	})
	return
}

// Run is the optional synchronous execution adapter. Transport/UI adapters may
// run it in a goroutine, but must not retry errors automatically. Every path
// after Consume preserves spent intent; persistence errors require recovery.
func (c *Controller) Run(ctx context.Context, a contracts.ApprovalBinding, cfg execution.Config) error {
	spec, e := c.Consume(a)
	if e != nil {
		return e
	}
	return c.RunConsumed(ctx, a, spec, cfg)
}

// RunConsumed reconciles a spec already durably consumed by the trusted UI adapter.
// It is not a guest API. Call exactly once, even when the context is cancelled.
func (c *Controller) RunConsumed(ctx context.Context, a contracts.ApprovalBinding, spec contracts.ExecutionSpec, cfg execution.Config) error {
	return c.RunConsumedWithObservedExecutor(ctx, a, spec, cfg, execution.ExecuteObserved)
}

// RunConsumedWithExecutor is a trusted in-process test seam, never wire input.
func (c *Controller) RunConsumedWithExecutor(ctx context.Context, a contracts.ApprovalBinding, spec contracts.ExecutionSpec, cfg execution.Config, execute func(context.Context, execution.Config, contracts.ExecutionSpec) execution.Report) error {
	return c.RunConsumedWithObservedExecutor(ctx, a, spec, cfg, func(ctx context.Context, cfg execution.Config, spec contracts.ExecutionSpec, _ execution.Observer) execution.Report {
		return execute(ctx, cfg, spec)
	})
}

// RunConsumedWithObservedExecutor is a trusted in-process seam, never wire input.
func (c *Controller) RunConsumedWithObservedExecutor(ctx context.Context, a contracts.ApprovalBinding, spec contracts.ExecutionSpec, cfg execution.Config, execute func(context.Context, execution.Config, contracts.ExecutionSpec, execution.Observer) execution.Report) error {
	var e error
	token := randomID()
	runCtx, stop := context.WithTimeout(ctx, time.Duration(spec.TimeoutSeconds)*time.Second+execution.AuthTimeout+30*time.Second)
	cancel := func() { c.clearLive(a.SnapshotID, token); stop() }
	defer cancel()
	var sessionTimer *time.Timer
	defer func() {
		if sessionTimer != nil {
			sessionTimer.Stop()
		}
	}()
	e = c.store.withLock(func() error {
		r, e := c.load(a.SnapshotID)
		if e != nil {
			return e
		}
		if r.Phase != contracts.Starting || r.Spec != spec || len(r.Spent) == 0 || r.Spent[len(r.Spent)-1] != a || c.executions[a.SnapshotID] != nil {
			return ErrGuard
		}
		c.executions[a.SnapshotID] = cancel
		c.liveMu.Lock()
		if c.live == nil {
			c.live = map[string]*LiveCapture{}
		}
		c.live[a.SnapshotID] = &LiveCapture{RunID: token, Stdout: []byte{}, Stderr: []byte{}}
		c.liveMu.Unlock()
		boot, e := c.cfg.Clock.BootElapsed()
		if e != nil {
			cancel()
			delete(c.executions, a.SnapshotID)
			return e
		}
		remaining := 4*time.Hour - (boot - c.bootStart)
		wallRemaining := 4*time.Hour - c.cfg.Clock.Now().Sub(c.started)
		if wallRemaining < remaining {
			remaining = wallRemaining
		}
		if remaining <= 0 {
			cancel()
		} else {
			sessionTimer = time.AfterFunc(remaining, cancel)
		}
		if r.CancelRequested || c.ended {
			cancel()
		}
		return nil
	})
	if e != nil {
		return e
	}
	defer func() { c.store.mu.Lock(); delete(c.executions, a.SnapshotID); c.store.mu.Unlock() }()
	defer c.clearLive(a.SnapshotID, token)
	stopCleanup := context.AfterFunc(runCtx, func() { c.clearLive(a.SnapshotID, token) })
	defer stopCleanup()
	report := execute(runCtx, cfg, spec, func(p execution.Progress) {
		if runCtx.Err() != nil {
			c.clearLive(a.SnapshotID, token)
			return
		}
		c.observeLive(a.SnapshotID, token, p)
	})
	c.clearLive(a.SnapshotID, token)
	if report.AuthCancelled {
		return c.Reconcile(a.SnapshotID, contracts.AuthDenied, nil)
	}
	if report.Result.Uncertain {
		return c.Reconcile(a.SnapshotID, contracts.LoseExecution, nil)
	}
	if report.Result.Started {
		if e = c.Reconcile(a.SnapshotID, contracts.Started, nil); e != nil {
			return e
		}
	}
	r := report.Result
	artifact := contracts.Artifact{Version: 1, ArtifactID: randomID(), SnapshotID: a.SnapshotID, Revision: a.Revision, ExecutionDigest: a.Digest, Executed: spec, Stdout: r.Stdout, Stderr: r.Stderr, Exit: r.Exit, Truncated: r.Truncated, RedactedFields: []string{}}
	return c.Reconcile(a.SnapshotID, contracts.Finish, &artifact)
}
