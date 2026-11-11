package host

import (
	"bytes"
	"errors"
	"os"
	"path/filepath"
	"testing"
	"time"

	"host-command-review/contracts"
)

func held(t *testing.T, c *Controller) (Record, contracts.Artifact) {
	t.Helper()
	r := imported(t, c, "1")
	a := approved(t, c, r)
	if _, e := c.Consume(a); e != nil {
		t.Fatal(e)
	}
	if e := c.Reconcile(a.SnapshotID, contracts.Started, nil); e != nil {
		t.Fatal(e)
	}
	artifact := contracts.Artifact{Version: 1, ArtifactID: randomID(), SnapshotID: a.SnapshotID, Revision: r.Revision, ExecutionDigest: r.Digest, Executed: r.Spec, Stdout: []byte("secret"), Stderr: []byte{}, Exit: contracts.ExitInfo{Kind: "exited", Code: 0}, RedactedFields: []string{}}
	if e := c.Reconcile(a.SnapshotID, contracts.Finish, &artifact); e != nil {
		t.Fatal(e)
	}
	return r, artifact
}
func TestReleasePublicationFaultsWithholdOrComplete(t *testing.T) {
	for _, stage := range []string{"link", "dir_sync", "unlink_temp", "cleanup_sync", "rename", "second_dir_sync"} {
		t.Run(stage, func(t *testing.T) {
			c, _ := setup(t)
			r, a := held(t, c)
			count := 0
			c.store.fault = func(s string) error {
				if s == "dir_sync" {
					count++
				}
				if s == stage || stage == "second_dir_sync" && s == "dir_sync" && count == 2 {
					return errors.New("injected")
				}
				return nil
			}
			if e := c.Release(r.Snapshot.SnapshotID, 1, a); e == nil {
				t.Fatal("fault ignored")
			}
			path := c.store.root.Name()
			c.CloseUnclean()
			c.store.Close()
			s, e := OpenStore(path, nil)
			if e != nil {
				t.Fatal(e)
			}
			defer s.Close()
			next, e := NewController(s, c.cfg)
			if e != nil {
				t.Fatal(e)
			}
			defer next.CloseUnclean()
			b, e := next.StatusBytes(r.Snapshot.SessionID, r.Snapshot.RequestID)
			if e != nil {
				t.Fatal(e)
			}
			reply, e := contracts.ParseReply(bytes.NewReader(b))
			if e != nil {
				t.Fatal(e)
			}
			if stage == "second_dir_sync" {
				if reply.Status != contracts.Released || reply.Artifact == nil {
					t.Fatal("committed reference missing complete artifact")
				}
			} else if reply.Artifact != nil || reply.Status != contracts.Held {
				t.Fatal("orphan publication disclosed")
			}
		})
	}
}
func TestRecoverInterruptedHardLink(t *testing.T) {
	c, _ := setup(t)
	r := imported(t, c, "1")
	path := c.store.root.Name()
	snap := filepath.Join(path, "snapshots", r.Snapshot.SnapshotID+".json")
	if e := os.Link(snap, filepath.Join(path, "snapshots", ".tmp-interrupted")); e != nil {
		t.Fatal(e)
	}
	c.CloseUnclean()
	c.store.Close()
	s, e := OpenStore(path, nil)
	if e != nil {
		t.Fatal(e)
	}
	defer s.Close()
	next, e := NewController(s, c.cfg)
	if e != nil {
		t.Fatal(e)
	}
	defer next.CloseUnclean()
	if _, e = next.View(r.Snapshot.SnapshotID); e != nil {
		t.Fatal("recovery checked nlink before temp cleanup", e)
	}
}
func TestRawCrashRetentionAndReleaseSurvival(t *testing.T) {
	c, clock := setup(t)
	r, a := held(t, c)
	if e := c.Release(r.Snapshot.SnapshotID, 1, a); e != nil {
		t.Fatal(e)
	}
	clock.now = clock.now.Add(25 * time.Hour)
	clock.boot += 25 * time.Hour
	if e := c.Sweep(false); e != nil {
		t.Fatal(e)
	}
	if _, e := c.store.read("raw", r.Snapshot.SnapshotID); !os.IsNotExist(e) {
		t.Fatal("raw not expired")
	}
	if _, e := c.store.read("releases", a.ArtifactID); e != nil {
		t.Fatal("release expired with raw")
	}
	clock.now = clock.now.Add(7 * 24 * time.Hour)
	clock.boot += 7 * 24 * time.Hour
	if e := c.Sweep(false); e != nil {
		t.Fatal(e)
	}
	if _, e := c.store.read("releases", a.ArtifactID); !os.IsNotExist(e) {
		t.Fatal("release not expired")
	}
}
