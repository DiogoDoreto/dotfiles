package reviewer

import (
	"context"
	"os"
	"path/filepath"
	"testing"

	"host-command-review/web"
)

func TestQuitExitsAfterCleanupFailureAndRetainsError(t *testing.T) {
	a, _, path := application(t)
	quit := false
	a.Quit = func() { quit = true }
	if err := os.WriteFile(filepath.Join(path, "raw", "unexpected"), []byte("bad"), 0600); err != nil {
		t.Fatal(err)
	}
	err := a.Apply(context.Background(), web.Operation{Action: "quit"})
	if err == nil {
		t.Fatal("cleanup failure was not reported")
	}
	if !quit || a.adapter != nil {
		t.Fatal("quit did not finish the session and request exit on first attempt")
	}
	if err := a.Shutdown(); err == nil {
		t.Fatal("application exit lost cleanup error")
	}
	if err := a.Start(); err == nil {
		t.Fatal("cleanup failure permitted restart")
	}
}
