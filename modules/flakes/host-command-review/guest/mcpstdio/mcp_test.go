package mcpstdio

import (
	"bytes"
	"context"
	"encoding/json"
	"io"
	"strings"
	"testing"
	"time"

	"github.com/modelcontextprotocol/go-sdk/mcp"
	"host-command-review/contracts"
	"host-command-review/guest"
	"host-command-review/transport"
)

func TestToolArgumentsClosedAndDefaults(t *testing.T) {
	valid := `{"session_id":"` + sid + `","request_id":"` + rid + `","proposal":{"script":"echo hello","cwd":"/","user":"dog"}}`
	q, err := toolRequest("submit", []byte(valid))
	if err != nil || q.Proposal.TimeoutSeconds != 120 {
		t.Fatalf("%+v %v", q, err)
	}
	for _, s := range []string{
		strings.Replace(valid, `"script":"echo hello"`, `"script":"echo hello","scrip\u0074":"other"`, 1),
		strings.Replace(valid, `"cwd":"/"`, `"Cwd":"/"`, 1),
		strings.Replace(valid, `"user":"dog"`, `"user":"dog","timeout_seconds":null`, 1),
		strings.Replace(valid, `"user":"dog"`, `"user":"dog","timeout_seconds":1e2`, 1),
		strings.Replace(valid, `"user":"dog"`, `"user":"dog","timeout_seconds":9223372036854775808`, 1),
		strings.Replace(valid, `echo hello`, `\ud800`, 1),
		strings.Replace(valid, `echo hello`, strings.Repeat("x", contracts.MaxScriptBytes+1), 1),
		valid + `{}`, `null`,
	} {
		if _, err := toolRequest("submit", []byte(s)); err == nil {
			t.Fatalf("accepted %s", s[:min(80, len(s))])
		}
	}
}
func TestMCPNativeStdioReleasedDataIdentical(t *testing.T) {
	s := store(t)
	if r, e := s.Exchange(context.Background(), transport.Request{Version: 1, Operation: "announce", SessionID: sid, Label: "lapdog", Alias: "ABCD-EFGH"}); e != nil || r.Error != "" {
		t.Fatal(r, e)
	}
	ctx, cancel := context.WithTimeout(context.Background(), 45*time.Second)
	defer cancel()
	client := mcp.NewClient(&mcp.Implementation{Name: "guest-test", Version: "1"}, nil)
	cs, err := client.Connect(ctx, &mcp.CommandTransport{Command: helper(s, "mcp")}, nil)
	if err != nil {
		t.Fatal(err)
	}
	defer cs.Close()
	tools, err := cs.ListTools(ctx, nil)
	if err != nil || len(tools.Tools) != 3 {
		t.Fatalf("tools %v %v", tools, err)
	}
	args := map[string]any{"session_id": "abcdefgh", "request_id": rid, "proposal": map[string]any{"script": "echo hello", "cwd": "/", "user": "dog"}}
	result, err := cs.CallTool(ctx, &mcp.CallToolParams{Name: "submit", Arguments: args})
	if err != nil || result.IsError {
		t.Fatalf("submit %v %v", result, err)
	}
	if got := checkResult(t, result, contracts.Pending); got.SessionID != sid {
		t.Fatal("noncanonical route", got)
	}
	// Hold reveals no original proposal, reviewed metadata, output or exit.
	r := reply(proposal(rid), "")
	r.Status = contracts.Held
	exchange(t, s, "publish", &r)
	route := map[string]any{"session_id": sid, "request_id": rid}
	result, err = cs.CallTool(ctx, &mcp.CallToolParams{Name: "get", Arguments: route})
	if err != nil {
		t.Fatal(err)
	}
	checkResult(t, result, contracts.Held)
	// Four MiB arbitrary capture survives both MCP representations, with worst
	// case escaped metadata. Inbound cap must not truncate outbound disclosure.
	a := artifact(t)
	a.Stdout = bytes.Repeat([]byte{0xff}, contracts.CaptureBytes)
	a.Executed.Script = strings.Repeat("\x01", contracts.MaxScriptBytes)
	a.ExecutionDigest, _ = a.Executed.Digest()
	r.Status = contracts.Released
	r.Artifact = a
	if out := exchange(t, s, "publish", &r); out.Error != "" {
		t.Fatal(out)
	}
	result, err = cs.CallTool(ctx, &mcp.CallToolParams{Name: "get", Arguments: route})
	if err != nil {
		t.Fatal(err)
	}
	got := checkResult(t, result, contracts.Released)
	if !bytes.Equal(got.Artifact.Stdout, a.Stdout) || got.Artifact.Executed.Script != a.Executed.Script {
		t.Fatal("release changed")
	}
	first, _ := json.Marshal(result)
	result, err = cs.CallTool(ctx, &mcp.CallToolParams{Name: "cancel", Arguments: route})
	if err != nil {
		t.Fatal(err)
	}
	again, _ := json.Marshal(result)
	if !bytes.Equal(first, again) {
		t.Fatal("long-ID cancel changed release")
	}
	route["session_id"] = "abcd-efgh"
	result, err = cs.CallTool(ctx, &mcp.CallToolParams{Name: "get", Arguments: route})
	if err != nil {
		t.Fatal(err)
	}
	again, _ = json.Marshal(result)
	if !bytes.Equal(first, again) {
		t.Fatal("cancel changed immutable release")
	}
}
func checkResult(t *testing.T, result *mcp.CallToolResult, status contracts.CoarseStatus) contracts.Reply {
	t.Helper()
	if result.IsError || len(result.Content) != 1 {
		t.Fatalf("unexpected content %+v", result)
	}
	text, ok := result.Content[0].(*mcp.TextContent)
	if !ok {
		t.Fatal("not text")
	}
	r, err := contracts.ParseReply(strings.NewReader(text.Text))
	if err != nil || r.Status != status {
		t.Fatalf("reply %v %v", r, err)
	}
	var want, got any
	if err = json.Unmarshal([]byte(text.Text), &want); err != nil {
		t.Fatal(err)
	}
	b, err := json.Marshal(result.StructuredContent)
	if err != nil {
		t.Fatal(err)
	}
	if err = json.Unmarshal(b, &got); err != nil {
		t.Fatal(err)
	}
	wb, _ := json.Marshal(want)
	gb, _ := json.Marshal(got)
	if !bytes.Equal(wb, gb) {
		t.Fatal("structured/text mismatch")
	}
	if status != contracts.Released && (r.Artifact != nil || strings.Contains(text.Text, "executed")) {
		t.Fatal("unreleased content")
	}
	return r
}
func TestFramesStrictAndBounded(t *testing.T) {
	for _, s := range []string{
		`{"jsonrpc":"2.0","id":1,"method":"ping","meth\u006fd":"bad"}` + "\n",
		`{"jsonrpc":"2.0","id":1,"method":"\ud800"}` + "\n",
		`{"jsonrpc":"2.0","id":1,"method":"ping"}`, // no newline
		`{} {}` + "\n", strings.Repeat(" ", MaxMCPFrameBytes) + "\n", string([]byte{0xff, '\n'}),
	} {
		r := NewFrameReader(io.NopCloser(strings.NewReader(s)))
		if _, err := io.ReadAll(r); err == nil {
			t.Fatal("accepted invalid frame")
		}
	}
	good := "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}\n"
	r := NewFrameReader(io.NopCloser(strings.NewReader(good + good)))
	b, err := io.ReadAll(r)
	if err != nil || string(b) != good+good {
		t.Fatalf("%s %v", b, err)
	}
}
func TestExchangeIOFraming(t *testing.T) {
	s := store(t)
	valid := `{"version":1,"operation":"announce","session_id":"` + sid + `","label":"lapdog"}`
	for _, input := range []string{valid + `{}`, strings.Replace(valid, `"version":1`, `"Version":1`, 1), strings.Replace(valid, `"version":1`, `"version":1,"version":1`, 1), strings.Repeat("x", 9<<20)} {
		var out bytes.Buffer
		if err := guest.ExchangeIO(context.Background(), s, strings.NewReader(input), &out); err == nil || out.Len() != 0 {
			t.Fatal("invalid exchange produced response")
		}
	}
	var out bytes.Buffer
	if err := guest.ExchangeIO(context.Background(), s, strings.NewReader(valid), &out); err != nil {
		t.Fatal(err)
	}
	if out.String()[out.Len()-1] != '\n' {
		t.Fatal("no newline")
	}
}
