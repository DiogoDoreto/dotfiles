package main

import (
	"os"
	"strings"
	"testing"
)

func TestUnpackagedEntrypointFailsClosed(t *testing.T) {
	saved := os.Args
	defer func() { os.Args = saved }()
	for _, args := range [][]string{{}, {"--unit-payload", "--approved-sha256", strings.Repeat("a", 64)}, {"--unit-payload", "--approved-sha256", "bad", "--extra"}} {
		os.Args = append([]string{"test-helper"}, args...)
		if run() != 70 {
			t.Fatal("unconfigured entrypoint did not fail closed")
		}
	}
}
