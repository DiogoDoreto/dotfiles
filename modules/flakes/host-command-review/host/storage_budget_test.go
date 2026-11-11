package host

import (
	"bytes"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"testing"
)

func TestHistoryStorageBudgetFailsBeforeWriteWithoutEvictionOrPoison(t *testing.T) {
	s, err := OpenStore(privateDir(t), nil)
	if err != nil {
		t.Fatal(err)
	}
	defer s.Close()
	id := randomID()
	if err := s.put("raw", id, "retained", true); err != nil {
		t.Fatal(err)
	}
	original, err := s.read("raw", id)
	if err != nil {
		t.Fatal(err)
	}
	used, err := s.storageBytes()
	if err != nil {
		t.Fatal(err)
	}
	// JSON for "new" is five bytes, plus its entry name. Exercise peak
	// accounting, including the old file during atomic replacement.
	s.maxStorageBytes = used + int64(5+64+len(".json")) - 1
	if err := s.put("raw", id, "new", false); !errors.Is(err, ErrStorageFull) {
		t.Fatal(err)
	}
	if s.poisoned {
		t.Fatal("capacity rejection poisoned healthy store")
	}
	got, err := s.read("raw", id)
	if err != nil || !bytes.Equal(got, original) {
		t.Fatalf("history changed: %s %v", got, err)
	}
	if err := s.withLock(func() error { return nil }); err != nil {
		t.Fatal(err)
	}
	s.maxStorageBytes++
	if err := s.put("raw", id, "new", false); err != nil {
		t.Fatal(err)
	}
}

func TestDirectoryEnumerationBeyondOld4096EntryLimit(t *testing.T) {
	root := privateDir(t)
	s, err := OpenStore(root, nil)
	if err != nil {
		t.Fatal(err)
	}
	defer s.Close()
	for i := 0; i < 4097; i++ {
		name := fmt.Sprintf("%064x.json", i)
		if err := os.WriteFile(filepath.Join(root, "raw", name), []byte("{}"), 0600); err != nil {
			t.Fatal(err)
		}
	}
	ids, err := s.names("raw")
	if err != nil || len(ids) != 4097 {
		t.Fatalf("history enumeration: %d %v", len(ids), err)
	}
	if _, err := s.storageBytes(); err != nil {
		t.Fatal(err)
	}
}
