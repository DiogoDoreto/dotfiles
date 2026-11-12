package mcpstdio

import (
	"bytes"
	"context"
	"crypto/rand"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"time"

	"github.com/modelcontextprotocol/go-sdk/mcp"
	"host-command-review/contracts"
	"host-command-review/guest"
	"host-command-review/transport"
)

const MaxMCPFrameBytes = contracts.MaxRequestBytes + (64 << 10)

// A release is represented twice by MCP (structured + JSON text); the 8 MiB
// SSH reply bound must NOT be reused for the resulting MCP response frame.
const MaxMCPResponseBytes = 24 << 20

type submitArgs struct {
	SessionID string         `json:"session_id"`
	Reason    string         `json:"reason"`
	Script    string         `json:"script"`
	Cwd       string         `json:"cwd"`
	User      contracts.User `json:"user"`
	Timeout   *int           `json:"timeout_seconds,omitempty"`
}
type routeArgs struct {
	SessionID string `json:"session_id"`
	RequestID string `json:"request_id"`
}

func toolRequest(name string, b []byte) (contracts.Request, error) {
	q := contracts.Request{Version: 1}
	switch name {
	case "propose_command":
		var a submitArgs
		if err := transport.DecodeStrict(bytes.NewReader(b), contracts.MaxRequestBytes, &a); err != nil {
			return q, err
		}
		timeout := contracts.DefaultTimeoutSeconds
		if a.Timeout != nil {
			timeout = *a.Timeout
		}
		q.Operation = contracts.Submit
		q.SessionID = a.SessionID
		// One identity per invocation, retained in q for any internal retry.
		// JSON-RPC IDs/_meta are not durable cross-call idempotency keys;
		// independent identical proposals must remain distinct commands.
		var id [32]byte
		if _, err := rand.Read(id[:]); err != nil {
			return q, err
		}
		q.RequestID = hex.EncodeToString(id[:])
		q.Proposal = &contracts.Proposal{Reason: a.Reason, Script: a.Script, Cwd: a.Cwd, User: a.User, TimeoutSeconds: timeout}
	case "get_command_status", "request_cancellation":
		var a routeArgs
		if err := transport.DecodeStrict(bytes.NewReader(b), contracts.MaxRequestBytes, &a); err != nil {
			return q, err
		}
		q.Operation = contracts.Check
		if name == "request_cancellation" {
			q.Operation = contracts.Cancel
		}
		q.SessionID = a.SessionID
		q.RequestID = a.RequestID
	default:
		return q, fmt.Errorf("invalid tool")
	}
	check := q
	if !transport.ValidID(q.SessionID) {
		if _, err := transport.CanonicalAlias(q.SessionID); err != nil {
			return q, err
		}
		check.SessionID = fmt.Sprintf("%064x", 0)
	}
	return q, check.Validate()
}
func toolError(code string) *mcp.CallToolResult {
	b, _ := json.Marshal(struct {
		Error string `json:"error"`
	}{code})
	return &mcp.CallToolResult{IsError: true, Content: []mcp.Content{&mcp.TextContent{Text: string(b)}}, StructuredContent: json.RawMessage(b)}
}
func replyResult(r contracts.Reply) (*mcp.CallToolResult, error) {
	// Re-parse the only outward payload. No alternate metadata, annotations,
	// images, resource links or hidden raw-output content blocks are generated.
	b, err := json.Marshal(r)
	if err != nil {
		return nil, err
	}
	if _, err = contracts.ParseReply(bytes.NewReader(b)); err != nil {
		return nil, err
	}
	result := &mcp.CallToolResult{Content: []mcp.Content{&mcp.TextContent{Text: string(b)}}, StructuredContent: json.RawMessage(b)}
	encoded, err := json.Marshal(result)
	if err != nil {
		return nil, err
	}
	if len(encoded) > MaxMCPResponseBytes {
		return nil, fmt.Errorf("response exceeds bound")
	}
	return result, nil
}
func NewMCPServer(s *guest.Store) *mcp.Server {
	server := mcp.NewServer(&mcp.Implementation{Name: "host-review", Version: "1.0.0"}, &mcp.ServerOptions{Capabilities: &mcp.ServerCapabilities{}})
	for _, name := range []string{"propose_command", "get_command_status", "request_cancellation"} {
		properties := map[string]any{
			"session_id": map[string]any{"type": "string", "description": "Host review routing code for a proposal; for status/cancellation use the canonical session_id returned by propose_command. Routing grants no authority.", "pattern": "^([0-9a-f]{64}|[2-9A-HJ-NP-Za-hj-np-z]{4}-?[2-9A-HJ-NP-Za-hj-np-z]{4})$"},
			"request_id": map[string]any{"type": "string", "description": "Exact server-generated request_id returned by propose_command. Never invent or replace it.", "pattern": "^[0-9a-f]{64}$"},
		}
		schema := map[string]any{"type": "object", "additionalProperties": false, "required": []string{"session_id", "request_id"}, "properties": properties}
		description := "Read the status of an existing host command using its returned session_id/request_id. Pending is not host acceptance or execution approval. Output stays held until separate explicit host release; released stdout/stderr are base64, may be redacted, and executed fields may reflect host edits. An uncertain outcome is not permission to submit a replacement: reconcile with the human reviewer first."
		if name == "propose_command" {
			description = "Propose a command for human review on the host; this does not approve execution or release output. Supply a concise reason explaining intent and expected effects. The host may edit execution fields. Returns canonical session_id and a new server-generated request_id: retain both for status/cancellation. Pending is not host acceptance. Execution approval and output release are separate; released results may be redacted and streams are base64. Every independent call creates a new request, even for identical commands. If the submission response is lost or ambiguous, do NOT blindly repeat or submit a replacement; reconcile with the human reviewer first. No exactly-once guarantee across calls."
			delete(properties, "request_id")
			schema["required"] = []string{"session_id", "reason", "script", "cwd", "user"}
			properties["reason"] = map[string]any{"type": "string", "description": "Agent-provided intent and expected effects for the human reviewer; nonblank UTF-8, at most 2048 bytes. Immutable original, not executable and not automatically included in released results.", "minLength": 1, "maxLength": contracts.MaxReasonBytes}
			properties["script"] = map[string]any{"type": "string", "description": "Exact bash script proposed for host execution; 1..65536 UTF-8 bytes without NUL. Human review may change it.", "minLength": 1, "maxLength": contracts.MaxScriptBytes}
			properties["cwd"] = map[string]any{"type": "string", "description": "Clean absolute working directory on the host (no dot components or trailing slash except /).", "maxLength": 4096}
			properties["user"] = map[string]any{"type": "string", "description": "Proposed host execution user; root requires separate host authentication and approval.", "enum": []string{"dog", "root"}}
			properties["timeout_seconds"] = map[string]any{"type": "integer", "description": "Host execution time limit in seconds, defaults to 120; timeout requests termination, not rollback of effects.", "minimum": 1, "maximum": contracts.MaxTimeoutSeconds, "default": contracts.DefaultTimeoutSeconds}
		} else if name == "request_cancellation" {
			description = "Request cancellation of an existing host command using its returned session_id/request_id. This is only a request, not confirmation that execution stopped; it cannot roll back side effects. Check get_command_status and reconcile uncertain outcomes with the human reviewer, never blindly submit a replacement. Cancellation does not release held output or change an already released immutable result."
		}
		server.AddTool(&mcp.Tool{Name: name, Description: description, InputSchema: schema}, func(ctx context.Context, req *mcp.CallToolRequest) (*mcp.CallToolResult, error) {
			q, err := toolRequest(name, req.Params.Arguments)
			if err != nil {
				return toolError("invalid_request"), nil
			}
			ctx, cancel := context.WithTimeout(ctx, 5*time.Second)
			defer cancel()
			q.SessionID, err = s.ResolveRoute(ctx, q.SessionID)
			if err != nil {
				return toolError("no_reviewer"), nil
			}
			out, err := s.Handle(ctx, q)
			if err != nil {
				return toolError("unavailable"), nil
			}
			result, err := replyResult(out)
			if err != nil {
				return toolError("unavailable"), nil
			}
			return result, nil
		})
	}
	return server
}
