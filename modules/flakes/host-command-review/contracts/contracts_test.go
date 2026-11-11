package contracts

import (
	"bytes"
	"encoding/json"
	"io"
	"os"
	"strings"
	"testing"
)

const testShell = "/nix/store/0123456789abcdfghijklmnpqrsvwxyz-bash-5.3/bin/bash"

func fixture(t *testing.T, name string) []byte {
	t.Helper()
	b, err := os.ReadFile("testdata/" + name + ".v1.json")
	if err != nil {
		t.Fatal(err)
	}
	return b
}
func spec(t *testing.T) ExecutionSpec {
	t.Helper()
	req, err := ParseRequest(bytes.NewReader(fixture(t, "submit")))
	if err != nil {
		t.Fatal(err)
	}
	s, err := Freeze(*req.Proposal, testShell)
	if err != nil {
		t.Fatal(err)
	}
	return s
}
func encoded(t *testing.T, v any) []byte {
	t.Helper()
	b, err := json.Marshal(v)
	if err != nil {
		t.Fatal(err)
	}
	return b
}
func TestFixtures(t *testing.T) {
	req, err := ParseRequest(bytes.NewReader(fixture(t, "submit")))
	if err != nil || req.Proposal.TimeoutSeconds != DefaultTimeoutSeconds {
		t.Fatalf("%+v %v", req, err)
	}
	reply, err := ParseReply(bytes.NewReader(fixture(t, "no-reviewer")))
	if err != nil || reply.Error != "no_reviewer" {
		t.Fatalf("%+v %v", reply, err)
	}
}
func TestStrictRequest(t *testing.T) {
	b := string(fixture(t, "submit"))
	cases := map[string]string{
		"unknown":              strings.Replace(b, `"version": 1`, `"version": 1, "extra": 0`, 1),
		"case alias":           strings.Replace(b, `"version"`, `"Version"`, 1),
		"duplicate":            strings.Replace(b, `"version": 1`, `"version": 1, "version": 1`, 1),
		"escaped duplicate":    strings.Replace(b, `"version": 1`, `"version": 1, "vers\u0069on": 1`, 1),
		"nested duplicate":     strings.Replace(b, `"user": "dog"`, `"user": "dog", "user": "root"`, 1),
		"nested unknown":       strings.Replace(b, `"user": "dog"`, `"user": "dog", "env": {}`, 1),
		"missing":              strings.Replace(b, `"version": 1,`, ``, 1),
		"null":                 strings.Replace(b, `"version": 1`, `"version": null`, 1),
		"null proposal":        strings.Replace(b, `"proposal": {`, `"proposal": null, "ignored": {`, 1),
		"float":                strings.Replace(b, `"version": 1`, `"version": 1.0`, 1),
		"exponent":             strings.Replace(b, `"version": 1`, `"version": 1e0`, 1),
		"future version":       strings.Replace(b, `"version": 1`, `"version": 2`, 1),
		"unknown user":         strings.Replace(b, `"user": "dog"`, `"user": "nobody"`, 1),
		"timeout zero":         strings.Replace(b, `"timeout_seconds": 120`, `"timeout_seconds": 0`, 1),
		"timeout huge":         strings.Replace(b, `"timeout_seconds": 120`, `"timeout_seconds": 1801`, 1),
		"relative cwd":         strings.Replace(b, `/home/dog/projects/example`, `../example`, 1),
		"unclean cwd":          strings.Replace(b, `/home/dog/projects/example`, `/home/dog/../root`, 1),
		"lone high surrogate":  strings.Replace(b, `review me`, `\ud800`, 1),
		"lone low surrogate":   strings.Replace(b, `review me`, `\udc00`, 1),
		"nul":                  strings.Replace(b, `review me`, `\u0000`, 1),
		"invalid utf8":         strings.Replace(b, `review me`, string([]byte{0xff}), 1),
		"trailing value":       b + ` {}`,
		"trailing garbage":     b + ` nope`,
		"wrong operation body": strings.Replace(b, `"submit"`, `"check"`, 1),
		"overflow":             strings.Replace(b, `"timeout_seconds": 120`, `"timeout_seconds": 99999999999999999999999`, 1),
	}
	for name, input := range cases {
		t.Run(name, func(t *testing.T) {
			got, err := ParseRequest(strings.NewReader(input))
			if err == nil {
				t.Fatal("accepted invalid request")
			}
			if got.Version != 0 || got.Proposal != nil {
				t.Fatal("returned partial input")
			}
		})
	}
	good := strings.Replace(b, `review me`, `\ud83d\ude00`, 1)
	if _, err := ParseRequest(strings.NewReader(good)); err != nil {
		t.Fatal(err)
	}
}
func TestBounds(t *testing.T) {
	if _, err := ParseRequest(strings.NewReader(strings.Repeat(" ", MaxRequestBytes+1))); err == nil {
		t.Fatal("unbounded")
	}
	r := &countingReader{remaining: MaxRequestBytes * 2}
	_, _ = ParseRequest(r)
	if r.read != MaxRequestBytes+1 {
		t.Fatalf("read %d", r.read)
	}
	p := Proposal{Script: strings.Repeat("x", MaxScriptBytes), Cwd: "/", User: Dog, TimeoutSeconds: MaxTimeoutSeconds}
	if err := p.Validate(); err != nil {
		t.Fatal(err)
	}
	p.Script += "x"
	if err := p.Validate(); err == nil {
		t.Fatal("oversize script")
	}
	// JSON HTML escaping must not make a valid maximum spec unparseable.
	p.Script = strings.Repeat("<", MaxScriptBytes)
	p.Cwd = "/" + strings.Repeat("<", 4095)
	s, err := Freeze(p, testShell)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := ParseExecution(bytes.NewReader(encoded(t, s))); err != nil {
		t.Fatal(err)
	}
}

type countingReader struct{ remaining, read int }

func (r *countingReader) Read(p []byte) (int, error) {
	if r.remaining == 0 {
		return 0, io.EOF
	}
	n := min(len(p), r.remaining)
	for i := 0; i < n; i++ {
		p[i] = ' '
	}
	r.remaining -= n
	r.read += n
	return n, nil
}
func TestFreezeAndDigest(t *testing.T) {
	s := spec(t)
	digest, err := s.Digest()
	if err != nil {
		t.Fatal(err)
	}
	// Golden protects field order, escaping and absence of trailing newline.
	const golden = "f1a4b32dd1b32057389f180e256b440baed6c85079d3adafe72e6c59da9d9317"
	if digest != golden {
		t.Fatalf("digest = %s", digest)
	}
	parsed, err := ParseExecution(bytes.NewReader(encoded(t, s)))
	if err != nil || parsed != s {
		t.Fatalf("round trip: %v", err)
	}
	edits := []func(*ExecutionSpec){
		func(s *ExecutionSpec) { s.Script += "\ntrue" }, func(s *ExecutionSpec) { s.Cwd = "/" },
		func(s *ExecutionSpec) { s.User = Root; s.Environment = CleanEnvironment(Root) },
		func(s *ExecutionSpec) { s.TimeoutSeconds++ },
		func(s *ExecutionSpec) { s.Shell = strings.Replace(s.Shell, "5.3", "5.4", 1) },
	}
	for _, edit := range edits {
		n := s
		edit(&n)
		d, err := n.Digest()
		if err != nil || d == digest {
			t.Fatalf("unbound edit: %v", err)
		}
	}
	invalid := []func(*ExecutionSpec){
		func(s *ExecutionSpec) { s.Environment.PATH = "/tmp" }, func(s *ExecutionSpec) { s.StopGraceSeconds++ },
		func(s *ExecutionSpec) { s.CaptureBytes++ }, func(s *ExecutionSpec) { s.Shell = "/bin/bash" },
		func(s *ExecutionSpec) { s.Script = string([]byte{0xff}) },
	}
	for _, edit := range invalid {
		n := s
		edit(&n)
		if _, err := n.Digest(); err == nil {
			t.Fatal("invalid spec digested")
		}
	}
}
func TestApproval(t *testing.T) {
	a := ApprovalBinding{Version: Version, SessionID: strings.Repeat("1", 64), SnapshotID: strings.Repeat("2", 64), Revision: 1, Digest: strings.Repeat("3", 64), Nonce: strings.Repeat("4", 64), Epoch: 1, ExpiresUnix: 100}
	if _, err := ParseApproval(bytes.NewReader(encoded(t, a))); err != nil {
		t.Fatal(err)
	}
	a.Nonce = ""
	if _, err := ParseApproval(bytes.NewReader(encoded(t, a))); err == nil {
		t.Fatal("empty nonce")
	}
}
func TestReleaseBoundary(t *testing.T) {
	s := spec(t)
	d, _ := s.Digest()
	a := Artifact{Version: Version, ArtifactID: strings.Repeat("3", 64), SnapshotID: strings.Repeat("4", 64), Revision: 1, ExecutionDigest: d, Executed: s, Stdout: []byte{0, 255}, Stderr: []byte{}, Exit: ExitInfo{Kind: "exited", Code: 0}, RedactedFields: []string{}}
	r := Reply{Version: Version, SessionID: strings.Repeat("1", 64), RequestID: strings.Repeat("2", 64), Status: Released, Artifact: &a}
	got, err := ParseReply(bytes.NewReader(encoded(t, r)))
	if err != nil || !bytes.Equal(got.Artifact.Stdout, a.Stdout) {
		t.Fatalf("%v", err)
	}
	for _, status := range []CoarseStatus{Pending, Active, Held, Rejected, Cancelled, Uncertain} {
		r.Status = status
		if _, err := ParseReply(bytes.NewReader(encoded(t, r))); err == nil {
			t.Fatalf("leak in %s", status)
		}
	}
	r.Status = Released
	r.Artifact = nil
	if r.Validate() == nil {
		t.Fatal("release without artifact")
	}
	a.Executed.Script = "[REDACTED]"
	if a.Validate() == nil {
		t.Fatal("unmarked changed execution")
	}
	a.RedactedFields = []string{"executed.script", "stdout"}
	if err := a.Validate(); err != nil {
		t.Fatal(err)
	}
	a.Stdout = make([]byte, CaptureBytes+1)
	if a.Validate() == nil {
		t.Fatal("oversized output")
	}
}
func FuzzRequest(f *testing.F) {
	b, err := os.ReadFile("testdata/submit.v1.json")
	if err != nil {
		f.Fatal(err)
	}
	f.Add(b)
	f.Add([]byte(`{}`))
	f.Fuzz(func(t *testing.T, b []byte) {
		r, err := ParseRequest(bytes.NewReader(b))
		if err == nil {
			again, err := ParseRequest(bytes.NewReader(encoded(t, r)))
			if err != nil || again.Validate() != nil {
				t.Fatal("unstable accepted request")
			}
		}
	})
}
