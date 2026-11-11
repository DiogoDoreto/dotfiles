package main

import (
	"os"
	"strings"
	"testing"
)

func TestUnpackagedEntrypointFailsClosed(t *testing.T) {
	saved := os.Args
	defer func() { os.Args = saved }()
	for _, args := range [][]string{{}, {"--approved-sha256", strings.Repeat("a", 64)}, {"--user", "--approved-sha256", strings.Repeat("a", 64)}, {"--approved-sha256", "bad", "--extra"}, {"--unit-payload"}} {
		os.Args = append([]string{"test-runner"}, args...)
		if run() != 70 {
			t.Fatal("unconfigured entrypoint did not fail closed")
		}
	}
}
