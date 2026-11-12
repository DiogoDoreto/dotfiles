package reviewer

import (
	"context"
	"testing"
	"time"
)

func TestBrowserLeaseDoesNotWaitForLifecycleTransition(t *testing.T) {
	a := &Application{}
	a.mu.Lock()
	done := make(chan error, 1)
	go func() { done <- a.Reviewer(context.Background(), time.Time{}) }()
	select {
	case err := <-done:
		if err == nil {
			t.Error("busy lifecycle should fail closed")
		}
	case <-time.After(time.Second):
		t.Error("browser lease blocked behind lifecycle transition")
	}
	a.mu.Unlock()
}
