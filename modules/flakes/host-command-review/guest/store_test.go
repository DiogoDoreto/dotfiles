package guest

import (
	"bytes"
	"context"
	"encoding/json"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"sync"
	"syscall"
	"testing"
	"time"

	"host-command-review/contracts"
	"host-command-review/transport"
)

var sid = strings.Repeat("a", 64)
var rid = strings.Repeat("b", 64)

func proposal(id string) contracts.Request {
	return contracts.Request{Version: 1, Operation: contracts.Submit, SessionID: sid, RequestID: id, Proposal: &contracts.Proposal{Script: "printf '<script>secret</script>\\033[31m'", Cwd: "/home/dog", User: contracts.Dog, TimeoutSeconds: 120}}
}
func store(t *testing.T) *Store {
	t.Helper()
	dir := t.TempDir()
	if err := os.Chmod(dir, 0700); err != nil {
		t.Fatal(err)
	}
	return &Store{Dir: dir}
}
func exchange(t *testing.T, s *Store, op string, r *contracts.Reply) transport.Response {
	t.Helper()
	q := transport.Request{Version: 1, Operation: op, SessionID: sid, Reply: r}
	if op == "announce" {
		q.Label = "lapdog"
	}
	out, err := s.Exchange(context.Background(), q)
	if err != nil {
		t.Fatal(err)
	}
	return out
}
func announce(t *testing.T, s *Store) {
	t.Helper()
	if out := exchange(t, s, "announce", nil); out.Error != "" {
		t.Fatal(out)
	}
}
func handle(t *testing.T, s *Store, q contracts.Request) contracts.Reply {
	t.Helper()
	out, err := s.Handle(context.Background(), q)
	if err != nil {
		t.Fatal(err)
	}
	return out
}
func TestLeaseRoutingAndDedupe(t *testing.T) {
	s := store(t)
	clock := int64(time.Second)
	s.now = func() (string, int64, error) { return "boot", clock, nil }
	q := proposal(rid)
	if out := handle(t, s, q); out.Error != "no_reviewer" {
		t.Fatal(out)
	}
	announce(t, s)
	if out := handle(t, s, q); out.Status != contracts.Pending {
		t.Fatal(out)
	}
	if out := handle(t, s, q); out.Status != contracts.Pending {
		t.Fatal(out)
	}
	changed := proposal(rid)
	changed.Proposal.Script = "different"
	if out := handle(t, s, changed); out.Error != "conflict" {
		t.Fatal(out)
	}
	if out := exchange(t, s, "pull", nil); len(out.Items) != 1 || out.Items[0].Acknowledged {
		t.Fatal(out)
	}
	q.Operation = contracts.Cancel
	q.Proposal = nil
	if out := handle(t, s, q); out.Status != contracts.Pending {
		t.Fatal(out)
	}
	if out := exchange(t, s, "pull", nil); !out.Items[0].CancelRequested {
		t.Fatal(out)
	}
	clock += 14 * int64(time.Second)
	if out := exchange(t, s, "renew", nil); out.LeaseSeconds != 15 {
		t.Fatal(out)
	}
	clock += 15 * int64(time.Second)
	if out := exchange(t, s, "renew", nil); out.Error != "session_expired" {
		t.Fatal(out)
	}
	if out := exchange(t, s, "announce", nil); out.Error != "session_expired" {
		t.Fatal(out)
	}
	q.Operation = contracts.Check
	if out := handle(t, s, q); out.Error != "session_expired" {
		t.Fatal(out)
	}
	if out := handle(t, s, proposal(strings.Repeat("c", 64))); out.Error != "no_reviewer" {
		t.Fatal(out)
	}
}
func TestBootAndHardExpiry(t *testing.T) {
	for _, kind := range []string{"boot", "hard", "backwards", "close"} {
		t.Run(kind, func(t *testing.T) {
			s := store(t)
			now := int64(time.Second)
			boot := "one"
			s.now = func() (string, int64, error) { return boot, now, nil }
			announce(t, s)
			switch kind {
			case "boot":
				boot = "two"
			case "hard":
				now += int64(contracts.MaxSessionSeconds) * int64(time.Second)
			case "backwards":
				now = 0
			case "close":
				exchange(t, s, "close", nil)
			}
			if out := exchange(t, s, "renew", nil); out.Error != "session_expired" {
				t.Fatal(out)
			}
		})
	}
}
func TestBoundedAdmissionAndImmutablePublication(t *testing.T) {
	s := store(t)
	announce(t, s)
	for i := 0; i < transport.MaxRecords; i++ {
		q := proposal(strings.Repeat(string(rune('0'+i)), 64))
		if out := handle(t, s, q); out.Error != "" {
			t.Fatal(out)
		}
	}
	if out := handle(t, s, proposal(rid)); out.Error != "queue_full" {
		t.Fatal(out)
	}
	q := proposal(strings.Repeat("0", 64))
	out := reply(q, "")
	out.Status = contracts.Held
	if r := exchange(t, s, "publish", &out); r.Error != "" {
		t.Fatal(r)
	}
	out.Status = contracts.Pending
	if r := exchange(t, s, "publish", &out); r.Error != "conflict" {
		t.Fatal(r)
	}
	out.Status = contracts.Released
	out.Artifact = artifact(t)
	if r := exchange(t, s, "publish", &out); r.Error != "" {
		t.Fatal(r)
	}
	if r := exchange(t, s, "publish", &out); r.Error != "" {
		t.Fatal(r)
	}
	out.Artifact.Stdout = []byte("mutated")
	if r := exchange(t, s, "publish", &out); r.Error != "conflict" {
		t.Fatal(r)
	}
	q.Operation = contracts.Check
	q.Proposal = nil
	if r := handle(t, s, q); !bytes.Equal(r.Artifact.Stdout, []byte("hostile\x00\x1b[31m<script>secret</script>")) {
		t.Fatal("release mutated")
	}
}
func artifact(t *testing.T) *contracts.Artifact {
	t.Helper()
	spec, err := contracts.Freeze(*proposal(rid).Proposal, "/nix/store/"+strings.Repeat("a", 32)+"-bash-5.3/bin/bash")
	if err != nil {
		t.Fatal(err)
	}
	digest, _ := spec.Digest()
	return &contracts.Artifact{Version: 1, ArtifactID: strings.Repeat("d", 64), SnapshotID: strings.Repeat("e", 64), Revision: 1, ExecutionDigest: digest, Executed: spec, Stdout: []byte("hostile\x00\x1b[31m<script>secret</script>"), Stderr: []byte{}, Exit: contracts.ExitInfo{Kind: "exited", Code: 0}, RedactedFields: []string{}}
}
func TestHeldDataNeverAppears(t *testing.T) {
	s := store(t)
	announce(t, s)
	q := proposal(rid)
	handle(t, s, q)
	r := reply(q, "")
	r.Status = contracts.Held
	exchange(t, s, "publish", &r)
	q.Operation = contracts.Check
	q.Proposal = nil
	out := handle(t, s, q)
	b, _ := json.Marshal(out)
	for _, secret := range []string{"script", "stdout", "stderr", "exit", "secret", "artifact", "digest", "executed"} {
		if bytes.Contains(b, []byte(secret)) {
			t.Fatalf("leaked %s", secret)
		}
	}
	bad := r
	bad.Artifact = artifact(t)
	if _, err := s.Exchange(context.Background(), transport.Request{Version: 1, Operation: "publish", SessionID: sid, Reply: &bad}); err == nil {
		t.Fatal("held artifact accepted")
	}
}
func TestStoreRejectsCorruptionAndUnsafeFiles(t *testing.T) {
	for _, kind := range []string{"directory_symlink", "state_symlink", "hardlink", "mode", "lock_symlink", "fifo", "corrupt", "oversized"} {
		t.Run(kind, func(t *testing.T) {
			s := store(t)
			announce(t, s)
			p := filepath.Join(s.Dir, "state.json")
			switch kind {
			case "directory_symlink":
				p = s.Dir + "-link"
				if err := os.Symlink(s.Dir, p); err != nil {
					t.Fatal(err)
				}
				defer os.Remove(p)
				s.Dir = p
			case "state_symlink":
				os.Rename(p, p+"-real")
				os.Symlink(p+"-real", p)
			case "hardlink":
				os.Link(p, p+"-link")
			case "mode":
				os.Chmod(p, 0644)
			case "lock_symlink":
				p = filepath.Join(s.Dir, "store.lock")
				os.Remove(p)
				os.Symlink("state.json", p)
			case "fifo":
				os.Remove(p)
				if err := syscall.Mkfifo(p, 0600); err != nil {
					t.Fatal(err)
				}
			case "corrupt":
				os.WriteFile(p, []byte(`{"version":1,"version":1}`), 0600)
			case "oversized":
				f, err := os.OpenFile(p, os.O_WRONLY, 0600)
				if err != nil {
					t.Fatal(err)
				}
				f.Truncate(MaxStateBytes + 1)
				f.Close()
			}
			if _, err := s.Handle(context.Background(), proposal(rid)); err == nil {
				t.Fatal("unsafe state accepted")
			}
		})
	}
}
func TestCrashScratchAndLockDeadline(t *testing.T) {
	s := store(t)
	announce(t, s)
	if err := os.WriteFile(filepath.Join(s.Dir, "state.tmp"), []byte("partial write"), 0600); err != nil {
		t.Fatal(err)
	}
	handle(t, s, proposal(rid))
	f, err := os.OpenFile(filepath.Join(s.Dir, "store.lock"), os.O_RDWR, 0600)
	if err != nil {
		t.Fatal(err)
	}
	defer f.Close()
	if err = syscall.Flock(int(f.Fd()), syscall.LOCK_EX); err != nil {
		t.Fatal(err)
	}
	defer syscall.Flock(int(f.Fd()), syscall.LOCK_UN)
	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Millisecond)
	defer cancel()
	if _, err = s.Handle(ctx, proposal(rid)); err == nil {
		t.Fatal("lock ignored deadline")
	}
}
func TestProcessHelper(t *testing.T) {
	mode := os.Getenv("HOST_REVIEW_TEST_HELPER")
	if mode == "" {
		return
	}
	s := &Store{Dir: os.Getenv("HOST_REVIEW_TEST_STATE")}
	switch mode {
	case "submit":
		if _, err := s.Handle(context.Background(), proposal(rid)); err != nil {
			os.Exit(2)
		}
	}
	os.Exit(0)
}
func helper(s *Store, mode string) *exec.Cmd {
	c := exec.Command(os.Args[0], "-test.run=^TestProcessHelper$")
	c.Env = append(os.Environ(), "HOST_REVIEW_TEST_HELPER="+mode, "HOST_REVIEW_TEST_STATE="+s.Dir)
	return c
}
func TestConcurrentProcesses(t *testing.T) {
	s := store(t)
	announce(t, s)
	var wg sync.WaitGroup
	for i := 0; i < 8; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			if b, err := helper(s, "submit").CombinedOutput(); err != nil {
				t.Errorf("%v: %s", err, b)
			}
		}()
	}
	wg.Wait()
	out := exchange(t, s, "pull", nil)
	if len(out.Items) != 1 {
		t.Fatal(out)
	}
}
