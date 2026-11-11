package mcpstdio

import (
	"context"
	"host-command-review/contracts"
	"host-command-review/guest"
	"host-command-review/transport"
	"os"
	"os/exec"
	"strings"
	"testing"
)

var sid = strings.Repeat("a", 64)
var rid = strings.Repeat("b", 64)

func store(t *testing.T) *guest.Store {
	t.Helper()
	dir := t.TempDir()
	if err := os.Chmod(dir, 0700); err != nil {
		t.Fatal(err)
	}
	return &guest.Store{Dir: dir}
}
func proposal(id string) contracts.Request {
	return contracts.Request{Version: 1, Operation: contracts.Submit, SessionID: sid, RequestID: id, Proposal: &contracts.Proposal{Script: "echo hello", Cwd: "/", User: contracts.Dog, TimeoutSeconds: 120}}
}
func reply(q contracts.Request, code string) contracts.Reply {
	return contracts.Reply{Version: 1, SessionID: q.SessionID, RequestID: q.RequestID, Status: contracts.Pending, Error: code}
}
func exchange(t *testing.T, s *guest.Store, op string, r *contracts.Reply) transport.Response {
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
func announce(t *testing.T, s *guest.Store) {
	t.Helper()
	if out := exchange(t, s, "announce", nil); out.Error != "" {
		t.Fatal(out)
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
func TestProcessHelper(t *testing.T) {
	if os.Getenv("HOST_REVIEW_TEST_HELPER") != "mcp" {
		return
	}
	if err := RunStdio(context.Background(), &guest.Store{Dir: os.Getenv("HOST_REVIEW_TEST_STATE")}, os.Stdin, os.Stdout); err != nil {
		os.Exit(2)
	}
	os.Exit(0)
}
func helper(s *guest.Store, mode string) *exec.Cmd {
	c := exec.Command(os.Args[0], "-test.run=^TestProcessHelper$")
	c.Env = append(os.Environ(), "HOST_REVIEW_TEST_HELPER="+mode, "HOST_REVIEW_TEST_STATE="+s.Dir)
	return c
}
