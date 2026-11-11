package host

import (
	"bytes"
	"encoding/json"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"

	"host-command-review/contracts"
)

const shell = "/nix/store/0123456789abcdfghijklmnpqrsvwxyz-bash-5.3/bin/bash"

type fakeClock struct {
	now  time.Time
	boot time.Duration
}

func (f *fakeClock) Now() time.Time                      { return f.now }
func (f *fakeClock) BootElapsed() (time.Duration, error) { return f.boot, nil }
func privateDir(t *testing.T) string {
	t.Helper()
	p, e := os.MkdirTemp(".", ".host-test-")
	if e != nil {
		t.Fatal(e)
	}
	p, e = filepath.Abs(p)
	if e != nil {
		t.Fatal(e)
	}
	t.Cleanup(func() { os.RemoveAll(p) })
	return p
}
func setup(t *testing.T) (*Controller, *fakeClock) {
	t.Helper()
	s, e := OpenStore(privateDir(t), nil)
	if e != nil {
		t.Fatal(e)
	}
	clock := &fakeClock{time.Unix(1700000000, 0), time.Hour}
	c, e := NewController(s, Config{Shell: shell, Clock: clock})
	if e != nil {
		t.Fatal(e)
	}
	t.Cleanup(func() { c.CloseUnclean(); s.Close() })
	if e = c.Heartbeat(); e != nil {
		t.Fatal(e)
	}
	return c, clock
}
func proposal() contracts.Proposal {
	return contracts.Proposal{Script: "printf secret", Cwd: "/", User: contracts.Dog, TimeoutSeconds: 120}
}
func imported(t *testing.T, c *Controller, n string) Record {
	t.Helper()
	p := proposal()
	r, e := c.Import(contracts.Request{Version: 1, Operation: contracts.Submit, SessionID: c.SessionID(), RequestID: strings.Repeat(n, 64), Proposal: &p})
	if e != nil {
		t.Fatal(e)
	}
	return r
}
func approved(t *testing.T, c *Controller, r Record) contracts.ApprovalBinding {
	t.Helper()
	a, e := c.Approve(r.Snapshot.SnapshotID, r.Revision, r.Digest)
	if e != nil {
		t.Fatal(e)
	}
	return a
}
func TestImportDedupeEditAndDisclosure(t *testing.T) {
	c, _ := setup(t)
	r := imported(t, c, "1")
	same := imported(t, c, "1")
	if same.Snapshot != r.Snapshot {
		t.Fatal("retry changed snapshot")
	}
	p := proposal()
	p.Script = "other"
	_, e := c.Import(contracts.Request{Version: 1, Operation: contracts.Submit, SessionID: c.SessionID(), RequestID: strings.Repeat("1", 64), Proposal: &p})
	if !errors.Is(e, ErrConflict) {
		t.Fatal(e)
	}
	a := approved(t, c, r)
	if e = c.Edit(r.Snapshot.SnapshotID, 1, p); e != nil {
		t.Fatal(e)
	}
	if _, e = c.Consume(a); e == nil {
		t.Fatal("stale consumed")
	}
	r, e = c.View(r.Snapshot.SnapshotID)
	if e != nil || r.Revision != 2 || r.Snapshot.Proposal == p {
		t.Fatal("snapshot mutable", e)
	}
	a = approved(t, c, r)
	if _, e = c.Consume(a); e != nil {
		t.Fatal(e)
	}
	if _, e = c.Consume(a); e == nil {
		t.Fatal("nonce reused")
	}
	b, e := c.StatusBytes(c.session, r.Snapshot.RequestID)
	if e != nil || bytes.Contains(b, []byte("secret")) || bytes.Contains(b, []byte("other")) {
		t.Fatal(string(b), e)
	}
	if e = c.Reconcile(a.SnapshotID, contracts.Started, nil); e != nil {
		t.Fatal(e)
	}
	artifact := contracts.Artifact{Version: 1, ArtifactID: randomID(), SnapshotID: a.SnapshotID, Revision: 2, ExecutionDigest: r.Digest, Executed: r.Spec, Stdout: []byte("secret output"), Stderr: []byte{}, Exit: contracts.ExitInfo{Kind: "exited", Code: 127}, RedactedFields: []string{}}
	if e = c.Reconcile(a.SnapshotID, contracts.Finish, &artifact); e != nil {
		t.Fatal(e)
	}
	b, _ = c.StatusBytes(c.session, r.Snapshot.RequestID)
	if bytes.Contains(b, []byte("secret")) {
		t.Fatal("held leaked")
	}
	bad := artifact
	bad.Executed.Script = "unmarked"
	if e = c.Release(a.SnapshotID, 2, bad); e == nil {
		t.Fatal("unmarked redaction")
	}
	artifact.Stdout = []byte("redacted")
	artifact.RedactedFields = []string{"stdout"}
	if e = c.Release(a.SnapshotID, 2, artifact); e != nil {
		t.Fatal(e)
	}
	b, e = c.StatusBytes(c.session, r.Snapshot.RequestID)
	if e != nil {
		t.Fatal(e)
	}
	again, _ := c.StatusBytes(c.session, r.Snapshot.RequestID)
	if !bytes.Equal(b, again) {
		t.Fatal("release unstable")
	}
	if _, e = contracts.ParseReply(bytes.NewReader(b)); e != nil {
		t.Fatal(e)
	}
	if e = c.End(); e != nil {
		t.Fatal(e)
	}
	if _, e = c.store.read("raw", a.SnapshotID); !os.IsNotExist(e) {
		t.Fatal("raw retained", e)
	}
	if _, e = c.store.read("releases", artifact.ArtifactID); e != nil {
		t.Fatal("release removed", e)
	}
}
func TestCancelConsumeRace(t *testing.T) {
	for i := 0; i < 30; i++ {
		c, _ := setup(t)
		r := imported(t, c, "1")
		a := approved(t, c, r)
		var wg sync.WaitGroup
		wg.Add(2)
		var consume error
		var stop bool
		var cancelErr error
		go func() { defer wg.Done(); _, consume = c.Consume(a) }()
		go func() { defer wg.Done(); stop, cancelErr = c.Cancel(c.session, r.Snapshot.RequestID) }()
		wg.Wait()
		if cancelErr != nil {
			t.Fatal(cancelErr)
		}
		out, e := c.View(a.SnapshotID)
		if e != nil {
			t.Fatal(e)
		}
		if consume == nil {
			if !stop || out.Phase != contracts.Starting || !out.CancelRequested {
				t.Fatal("consumed cancellation mislabeled")
			}
		} else if stop || out.Phase != contracts.Stopped {
			t.Fatal("prestart cancel not serialized")
		}
	}
}
func TestOneCommandIncludesAuth(t *testing.T) {
	c, _ := setup(t)
	r := imported(t, c, "1")
	s := imported(t, c, "2")
	a := approved(t, c, r)
	b := approved(t, c, s)
	if _, e := c.Consume(a); e != nil {
		t.Fatal(e)
	}
	if _, e := c.Consume(b); e == nil {
		t.Fatal("concurrent auth")
	}
	if e := c.Reconcile(a.SnapshotID, contracts.AuthDenied, nil); e != nil {
		t.Fatal(e)
	}
	if _, e := c.Consume(a); e == nil {
		t.Fatal("denial unspent nonce")
	}
	if _, e := c.Consume(b); e != nil {
		t.Fatal(e)
	}
}
func TestLeasesSuspendSession(t *testing.T) {
	for _, kind := range []string{"disconnect", "lease", "approval", "suspend", "explicit", "session", "backwards"} {
		t.Run(kind, func(t *testing.T) {
			c, clock := setup(t)
			r := imported(t, c, "1")
			a := approved(t, c, r)
			switch kind {
			case "disconnect":
				c.Disconnect()
			case "lease":
				clock.now = clock.now.Add(3 * time.Second)
				clock.boot += 3 * time.Second
			case "approval":
				clock.now = clock.now.Add(31 * time.Second)
				clock.boot += 31 * time.Second
				c.Heartbeat()
			case "suspend":
				clock.boot += time.Hour
				c.Heartbeat()
			case "explicit":
				c.Invalidate()
				c.Heartbeat()
			case "session":
				clock.boot += 4 * time.Hour
				c.Heartbeat()
			case "backwards":
				clock.now = clock.now.Add(-time.Minute)
				c.Heartbeat()
			}
			if _, e := c.Consume(a); e == nil {
				t.Fatal("stale authority consumed")
			}
		})
	}
}
func TestConsumeFaultsNeverReturnSpec(t *testing.T) {
	for _, stage := range []string{"write", "file_sync", "close", "rename", "dir_sync"} {
		t.Run(stage, func(t *testing.T) {
			c, _ := setup(t)
			r := imported(t, c, "1")
			a := approved(t, c, r)
			c.store.fault = func(s string) error {
				if s == stage {
					return errors.New("injected")
				}
				return nil
			}
			spec, e := c.Consume(a)
			if e == nil || spec.Script != "" {
				t.Fatal("execution permitted on failure")
			}
			if _, e = c.Consume(a); e == nil {
				t.Fatal("poisoned handle reused")
			}
			path := c.store.root.Name()
			c.CloseUnclean()
			c.store.Close()
			s, e := OpenStore(path, nil)
			if e != nil {
				t.Fatal(e)
			}
			defer s.Close()
			next, e := NewController(s, Config{Shell: shell, Clock: c.cfg.Clock})
			if e != nil {
				t.Fatal(e)
			}
			defer next.CloseUnclean()
			out, e := next.View(a.SnapshotID)
			if e != nil {
				t.Fatal(e)
			}
			if stage == "dir_sync" {
				if out.Phase != contracts.Ambiguous {
					t.Fatal(out.Phase)
				}
			} else if out.Phase != contracts.Reviewing {
				t.Fatal(out.Phase)
			}
			if _, e = next.Consume(a); e == nil {
				t.Fatal("recovery replayed")
			}
		})
	}
}
func TestImmutableFaultsAndNoClobber(t *testing.T) {
	for _, stage := range []string{"write", "file_sync", "close", "link", "dir_sync", "unlink_temp", "cleanup_sync"} {
		t.Run(stage, func(t *testing.T) {
			dir := privateDir(t)
			s, e := OpenStore(dir, func(v string) error {
				if v == stage {
					return errors.New("injected")
				}
				return nil
			})
			if e != nil {
				t.Fatal(e)
			}
			defer s.Close()
			e = s.withLock(func() error {
				return s.put("snapshots", randomID(), struct {
					V int `json:"v"`
				}{1}, true)
			})
			if e == nil {
				t.Fatal("fault ignored")
			}
		})
	}
	s, e := OpenStore(privateDir(t), nil)
	if e != nil {
		t.Fatal(e)
	}
	defer s.Close()
	id := randomID()
	if e = s.withLock(func() error { return s.put("releases", id, 1, true) }); e != nil {
		t.Fatal(e)
	}
	if e = s.withLock(func() error { return s.put("releases", id, 2, true) }); e == nil {
		t.Fatal("clobbered immutable")
	}
}
func TestUnsafePathsCorruptionAndSingleton(t *testing.T) {
	c, _ := setup(t)
	if _, e := NewController(c.store, c.cfg); e == nil {
		t.Fatal("two reviewers")
	}
	r := imported(t, c, "1")
	p := filepath.Join(c.store.root.Name(), "records", r.Snapshot.SnapshotID+".json")
	b, _ := os.ReadFile(p)
	var v map[string]any
	json.Unmarshal(b, &v)
	b = append(b, ' ')
	os.WriteFile(p, b, 0600)
	if _, e := c.Consume(approvedDummy(r)); e == nil {
		t.Fatal("corruption ignored")
	}
	if _, e := OpenStore("/tmp/host-review-must-reject", nil); e == nil {
		t.Fatal("world writable ancestor")
	}
	path := privateDir(t)
	link := filepath.Join(path, "symlink")
	os.Symlink(c.store.root.Name(), link)
	if _, e := OpenStore(link, nil); e == nil {
		t.Fatal("symlink followed")
	}
}
func approvedDummy(r Record) contracts.ApprovalBinding {
	return contracts.ApprovalBinding{SnapshotID: r.Snapshot.SnapshotID}
}
func TestRetention(t *testing.T) {
	c, clock := setup(t)
	r := imported(t, c, "1")
	a := approved(t, c, r)
	c.Consume(a)
	if e := c.End(); e == nil {
		t.Fatal("clean end with active intent")
	}
	c.Reconcile(a.SnapshotID, contracts.LoseExecution, nil)
	clock.now = clock.now.Add(8 * 24 * time.Hour)
	clock.boot += 8 * 24 * time.Hour
	if e := c.Sweep(false); e != nil {
		t.Fatal(e)
	}
	if _, e := c.View(a.SnapshotID); !os.IsNotExist(e) {
		t.Fatal("ledger not expired", e)
	}
}
