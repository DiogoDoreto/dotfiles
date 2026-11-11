// Package host owns host-private authority. Its APIs are local trusted adapter
// APIs, not transport handlers; browser authentication must precede mutations.
package host

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"syscall"
)

const maxFile = 8 << 20

// Fault is a test seam invoked immediately before durability boundaries. Any
// error poisons this handle: uncertain writes must never authorize a launch.
type Fault func(stage string) error

type Store struct {
	mu       sync.Mutex
	root     *os.File
	dirs     map[string]*os.File
	lock     *os.File
	fault    Fault
	poisoned bool
}

func checkFD(fd int, dir bool) error {
	var s syscall.Stat_t
	if err := syscall.Fstat(fd, &s); err != nil {
		return err
	}
	typ := uint32(syscall.S_IFREG)
	if dir {
		typ = syscall.S_IFDIR
	}
	if s.Mode&syscall.S_IFMT != typ || s.Uid != uint32(os.Geteuid()) || s.Mode&0077 != 0 || (!dir && s.Nlink != 1) {
		return errors.New("unsafe private store inode")
	}
	return nil
}

// OpenStore anchors every operation at directory FDs. All ancestors must be
// root/current-user owned and not group/world writable; no symlinks, including
// ancestors. This deliberately refuses /tmp and known VM-share roots. The deployment
// must additionally audit mounts/exports; ownership cannot detect a VM export.
func OpenStore(path string, fault Fault) (*Store, error) {
	if !filepath.IsAbs(path) || filepath.Clean(path) != path || path == "/" {
		return nil, errors.New("absolute clean private path required")
	}
	for _, share := range []string{"/home/dog/projects", "/var/lib/agent-vm"} {
		if path == share || strings.HasPrefix(path, share+"/") {
			return nil, errors.New("store must be outside VM-share roots")
		}
	}
	fd, err := syscall.Open("/", syscall.O_DIRECTORY|syscall.O_CLOEXEC|syscall.O_RDONLY, 0)
	if err != nil {
		return nil, err
	}
	for _, part := range strings.Split(strings.TrimPrefix(path, "/"), "/") {
		next, e := syscall.Openat(fd, part, syscall.O_RDONLY|syscall.O_DIRECTORY|syscall.O_NOFOLLOW|syscall.O_CLOEXEC, 0)
		if errors.Is(e, syscall.ENOENT) {
			if e = syscall.Mkdirat(fd, part, 0700); e == nil {
				e = syscall.Fsync(fd)
			}
			if e == nil {
				next, e = syscall.Openat(fd, part, syscall.O_RDONLY|syscall.O_DIRECTORY|syscall.O_NOFOLLOW|syscall.O_CLOEXEC, 0)
			}
		}
		syscall.Close(fd)
		if e != nil {
			return nil, e
		}
		fd = next
		var st syscall.Stat_t
		e = syscall.Fstat(fd, &st)
		if e != nil || (st.Uid != 0 && st.Uid != uint32(os.Geteuid())) || st.Mode&0022 != 0 {
			syscall.Close(fd)
			return nil, errors.New("unsafe store ancestor")
		}
	}
	if err = checkFD(fd, true); err != nil {
		syscall.Close(fd)
		return nil, err
	}
	s := &Store{root: os.NewFile(uintptr(fd), path), dirs: map[string]*os.File{}, fault: fault}
	fail := func(e error) (*Store, error) { s.Close(); return nil, e }
	for _, name := range []string{"records", "snapshots", "raw", "releases"} {
		e := syscall.Mkdirat(fd, name, 0700)
		if e != nil && !errors.Is(e, syscall.EEXIST) {
			return fail(e)
		}
		d, e := syscall.Openat(fd, name, syscall.O_RDONLY|syscall.O_DIRECTORY|syscall.O_NOFOLLOW|syscall.O_CLOEXEC, 0)
		if e != nil {
			return fail(e)
		}
		if e = checkFD(d, true); e != nil {
			syscall.Close(d)
			return fail(e)
		}
		s.dirs[name] = os.NewFile(uintptr(d), name)
	}
	l, e := syscall.Openat(fd, "store.lock", syscall.O_RDWR|syscall.O_CREAT|syscall.O_NOFOLLOW|syscall.O_CLOEXEC, 0600)
	if e != nil {
		return fail(e)
	}
	s.lock = os.NewFile(uintptr(l), "store.lock")
	if e = checkFD(l, false); e != nil {
		return fail(e)
	}
	if e = s.root.Sync(); e != nil {
		return fail(e)
	}
	return s, nil
}
func (s *Store) Close() error {
	for _, f := range s.dirs {
		f.Close()
	}
	if s.lock != nil {
		s.lock.Close()
	}
	if s.root != nil {
		return s.root.Close()
	}
	return nil
}
func (s *Store) withLock(fn func() error) error {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.poisoned {
		return errors.New("store requires recovery")
	}
	if e := syscall.Flock(int(s.lock.Fd()), syscall.LOCK_EX); e != nil {
		return e
	}
	defer syscall.Flock(int(s.lock.Fd()), syscall.LOCK_UN)
	return fn()
}
func (s *Store) step(stage string) error {
	if s.fault != nil {
		if e := s.fault(stage); e != nil {
			s.poisoned = true
			return e
		}
	}
	return nil
}
func validID(id string) bool {
	if len(id) != 64 {
		return false
	}
	for _, c := range id {
		if !(c >= '0' && c <= '9' || c >= 'a' && c <= 'f') {
			return false
		}
	}
	return true
}
func (s *Store) read(dir, id string) ([]byte, error) {
	if !validID(id) {
		return nil, errors.New("invalid host ID")
	}
	fd, e := syscall.Openat(int(s.dirs[dir].Fd()), id+".json", syscall.O_RDONLY|syscall.O_NOFOLLOW|syscall.O_CLOEXEC, 0)
	if e != nil {
		return nil, e
	}
	f := os.NewFile(uintptr(fd), id)
	defer f.Close()
	if e = checkFD(fd, false); e != nil {
		return nil, e
	}
	b, e := io.ReadAll(io.LimitReader(f, maxFile+1))
	if len(b) > maxFile {
		return nil, errors.New("oversize store file")
	}
	return b, e
}

// Private records have exactly our canonical encoding. Besides unknown fields,
// this rejects duplicates, case aliases, missing fields and trailing JSON.
func decode(b []byte, v any) error {
	d := json.NewDecoder(bytes.NewReader(b))
	d.DisallowUnknownFields()
	if e := d.Decode(v); e != nil {
		return e
	}
	canonical, e := json.Marshal(v)
	if e != nil {
		return e
	}
	if !bytes.Equal(canonical, b) {
		return errors.New("noncanonical private record")
	}
	return nil
}
func (s *Store) put(dir, id string, v any, immutable bool) (err error) {
	if !validID(id) {
		return errors.New("invalid host ID")
	}
	b, e := json.Marshal(v)
	if e != nil {
		return e
	}
	if len(b) > maxFile {
		return errors.New("oversize store file")
	}
	d := s.dirs[dir]
	tmp := ".tmp-" + randomID()
	fd, e := syscall.Openat(int(d.Fd()), tmp, syscall.O_WRONLY|syscall.O_CREAT|syscall.O_EXCL|syscall.O_NOFOLLOW|syscall.O_CLOEXEC, 0600)
	if e != nil {
		return e
	}
	f := os.NewFile(uintptr(fd), tmp)
	defer func() {
		f.Close()
		syscall.Unlinkat(int(d.Fd()), tmp)
		if err != nil {
			s.poisoned = true
		}
	}()
	if e = s.step("write"); e != nil {
		return e
	}
	if _, e = f.Write(b); e != nil {
		return e
	}
	if e = s.step("file_sync"); e != nil {
		return e
	}
	if e = f.Sync(); e != nil {
		return e
	}
	if e = s.step("close"); e != nil {
		return e
	}
	if e = f.Close(); e != nil {
		return e
	}
	final := id + ".json"
	if immutable {
		if e = s.step("link"); e != nil {
			return e
		}
		// /proc/self/fd anchors both paths; Go's syscall lacks Linkat.
		base := fmt.Sprintf("/proc/self/fd/%d/", d.Fd())
		if e = os.Link(base+tmp, base+final); e != nil {
			return e
		}
	} else {
		if e = s.step("rename"); e != nil {
			return e
		}
		if e = syscall.Renameat(int(d.Fd()), tmp, int(d.Fd()), final); e != nil {
			return e
		}
	}
	if e = s.step("dir_sync"); e != nil {
		return e
	}
	if e = d.Sync(); e != nil {
		return e
	}
	if immutable {
		if e = s.step("unlink_temp"); e != nil {
			return e
		}
		if e = syscall.Unlinkat(int(d.Fd()), tmp); e != nil {
			return e
		}
		if e = s.step("cleanup_sync"); e != nil {
			return e
		}
		return d.Sync()
	}
	return nil
}
func (s *Store) names(dir string) ([]string, error) {
	fd, e := syscall.Openat(int(s.dirs[dir].Fd()), ".", syscall.O_RDONLY|syscall.O_DIRECTORY|syscall.O_CLOEXEC, 0)
	if e != nil {
		return nil, e
	}
	f := os.NewFile(uintptr(fd), dir)
	defer f.Close()
	names, e := f.Readdirnames(4097)
	if e != nil && e != io.EOF {
		return nil, e
	}
	if len(names) > 4096 {
		return nil, errors.New("store directory capacity exceeded")
	}
	var ids []string
	cleaned := false
	for _, n := range names {
		if strings.HasPrefix(n, ".tmp-") {
			if e = syscall.Unlinkat(fd, n); e != nil {
				return nil, e
			}
			cleaned = true
			continue
		}
		if !strings.HasSuffix(n, ".json") || !validID(strings.TrimSuffix(n, ".json")) {
			return nil, errors.New("unexpected store entry")
		}
		ids = append(ids, strings.TrimSuffix(n, ".json"))
	}
	if cleaned {
		if e = f.Sync(); e != nil {
			return nil, e
		}
	}
	return ids, nil
}
func (s *Store) remove(dir, id string) error {
	if !validID(id) {
		return errors.New("bad ID")
	}
	e := syscall.Unlinkat(int(s.dirs[dir].Fd()), id+".json")
	if e != nil && !errors.Is(e, syscall.ENOENT) {
		return e
	}
	return s.dirs[dir].Sync()
}
