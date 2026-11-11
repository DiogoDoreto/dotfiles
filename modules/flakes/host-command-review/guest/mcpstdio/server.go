package mcpstdio

import (
	"bytes"
	"context"
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
	SessionID string `json:"session_id"`
	RequestID string `json:"request_id"`
	Proposal  struct {
		Script  string         `json:"script"`
		Cwd     string         `json:"cwd"`
		User    contracts.User `json:"user"`
		Timeout *int           `json:"timeout_seconds,omitempty"`
	} `json:"proposal"`
}
type routeArgs struct {
	SessionID string `json:"session_id"`
	RequestID string `json:"request_id"`
}

func toolRequest(name string, b []byte) (contracts.Request, error) {
	q := contracts.Request{Version: 1}
	switch name {
	case "submit":
		var a submitArgs
		if err := transport.DecodeStrict(bytes.NewReader(b), contracts.MaxRequestBytes, &a); err != nil {
			return q, err
		}
		timeout := contracts.DefaultTimeoutSeconds
		if a.Proposal.Timeout != nil {
			timeout = *a.Proposal.Timeout
		}
		q.Operation = contracts.Submit
		q.SessionID = a.SessionID
		q.RequestID = a.RequestID
		q.Proposal = &contracts.Proposal{Script: a.Proposal.Script, Cwd: a.Proposal.Cwd, User: a.Proposal.User, TimeoutSeconds: timeout}
	case "get", "cancel":
		var a routeArgs
		if err := transport.DecodeStrict(bytes.NewReader(b), contracts.MaxRequestBytes, &a); err != nil {
			return q, err
		}
		q.Operation = contracts.Check
		if name == "cancel" {
			q.Operation = contracts.Cancel
		}
		q.SessionID = a.SessionID
		q.RequestID = a.RequestID
	default:
		return q, fmt.Errorf("invalid tool")
	}
	return q, q.Validate()
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
	for _, name := range []string{"submit", "get", "cancel"} {
		schema := map[string]any{"type": "object", "additionalProperties": false, "required": []string{"session_id", "request_id"}, "properties": map[string]any{
			"session_id": map[string]any{"type": "string", "pattern": "^[0-9a-f]{64}$"}, "request_id": map[string]any{"type": "string", "pattern": "^[0-9a-f]{64}$"},
		}}
		if name == "submit" {
			schema["required"] = []string{"session_id", "request_id", "proposal"}
			schema["properties"].(map[string]any)["proposal"] = map[string]any{"type": "object", "additionalProperties": false, "required": []string{"script", "cwd", "user"}, "properties": map[string]any{
				"script": map[string]any{"type": "string", "minLength": 1, "maxLength": contracts.MaxScriptBytes}, "cwd": map[string]any{"type": "string", "maxLength": 4096}, "user": map[string]any{"type": "string", "enum": []string{"dog", "root"}}, "timeout_seconds": map[string]any{"type": "integer", "minimum": 1, "maximum": contracts.MaxTimeoutSeconds, "default": contracts.DefaultTimeoutSeconds},
			}}
		}
		server.AddTool(&mcp.Tool{Name: name, Description: "Guest routing only; pending is not host acceptance. No execution or output without host review and explicit release.", InputSchema: schema}, func(ctx context.Context, req *mcp.CallToolRequest) (*mcp.CallToolResult, error) {
			q, err := toolRequest(name, req.Params.Arguments)
			if err != nil {
				return toolError("invalid_request"), nil
			}
			ctx, cancel := context.WithTimeout(ctx, 5*time.Second)
			defer cancel()
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
