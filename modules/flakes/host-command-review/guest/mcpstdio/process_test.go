package mcpstdio

import (
	"bytes"
	"context"
	"os/exec"
	"strings"
	"testing"
	"time"
)

func TestStdioMalformedFramesCloseProcess(t *testing.T) {
	for _, input := range []string{`{"jsonrpc":"2.0","id":1,"method":"ping","method":"bad"}` + "\n", strings.Repeat("x", MaxMCPFrameBytes+1) + "\n"} {
		s := store(t)
		ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
		template := helper(s, "mcp")
		cmd := exec.CommandContext(ctx, template.Path, template.Args[1:]...)
		cmd.Env = template.Env
		cmd.Stdin = strings.NewReader(input)
		var stdout bytes.Buffer
		cmd.Stdout = &stdout
		err := cmd.Run()
		cancel()
		if err == nil || stdout.Len() != 0 {
			t.Fatal("malformed frame didn't close cleanly")
		}
	}
}
