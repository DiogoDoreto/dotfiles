// Package guest implements cooperative guest state, never host authority.
package guest

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"reflect"
	"syscall"
	"time"
	"unsafe"

	"host-command-review/contracts"
	"host-command-review/transport"
)

// One atomic database gives cross-record transactions without an index or a
// host-store dependency. Retained history has an aggregate byte budget independent
// of per-session outstanding-work limits. It never silently evicts publications.
const MaxStateBytes = 900 << 20

type Store struct {
	Dir string
	now func() (string, int64, error)
}
type session struct {
	ID          string `json:"id"`
	Label       string `json:"label"`
	Boot        string `json:"boot"`
	Start       int64  `json:"start"`
	Until       int64  `json:"until"`
	Closed      bool   `json:"closed"`
	CreatedUnix int64  `json:"created_unix"`
}
type record struct {
	Request contracts.Request `json:"request"`
	Reply   contracts.Reply   `json:"reply"`
	Cancel  bool              `json:"cancel"`
	Ack     bool              `json:"ack"`
}
type database struct {
	Version  int       `json:"version"`
	Sessions []session `json:"sessions"`
	Records  []record  `json:"records"`
}

// CLOCK_BOOTTIME includes suspend and survives different guest processes.
// Boot identity prevents reboot from reviving a persisted lease.
func bootNow() (string, int64, error) {
	b, err := os.ReadFile("/proc/sys/kernel/random/boot_id")
	if err != nil {
		return "", 0, err
	}
	var ts syscall.Timespec
	_, _, e := syscall.Syscall(syscall.SYS_CLOCK_GETTIME, 7, uintptr(unsafe.Pointer(&ts)), 0)
	if e != 0 {
		return "", 0, e
	}
	return string(bytes.TrimSpace(b)), ts.Nano(), nil
}
func (s *Store) clock() (string, int64, error) {
	if s.now != nil {
		return s.now()
	}
	return bootNow()
}
func live(v session, boot string, now int64) bool {
	return !v.Closed && v.Boot == boot && now >= v.Start && now < v.Until && now-v.Start < int64(contracts.MaxSessionSeconds)*int64(time.Second)
}

// openDir rejects symlink ancestors. The final directory must be private and
// owned by this UID; this does not defend against guest root (nor should it).
func openDir(path string) (*os.File, error) {
	abs, err := filepath.Abs(path)
	if err != nil {
		return nil, err
	}
	f, err := os.Open("/")
	if err != nil {
		return nil, err
	}
	for _, part := range splitPath(abs) {
		fd, e := syscall.Openat(int(f.Fd()), part, syscall.O_RDONLY|syscall.O_DIRECTORY|syscall.O_NOFOLLOW|syscall.O_CLOEXEC, 0)
		if e == syscall.ENOENT {
			if e = syscall.Mkdirat(int(f.Fd()), part, 0700); e == nil || e == syscall.EEXIST {
				fd, e = syscall.Openat(int(f.Fd()), part, syscall.O_RDONLY|syscall.O_DIRECTORY|syscall.O_NOFOLLOW|syscall.O_CLOEXEC, 0)
			}
		}
		f.Close()
		if e != nil {
			return nil, e
		}
		f = os.NewFile(uintptr(fd), part)
	}
	st, err := f.Stat()
	if err != nil {
		f.Close()
		return nil, err
	}
	stat := st.Sys().(*syscall.Stat_t)
	if stat.Uid != uint32(os.Getuid()) || st.Mode().Perm() != 0700 {
		f.Close()
		return nil, fmt.Errorf("state directory must be private and owned")
	}
	return f, nil
}
func splitPath(p string) []string {
	var parts []string
	for p != "/" {
		parts = append([]string{filepath.Base(p)}, parts...)
		p = filepath.Dir(p)
	}
	return parts
}
func openPrivate(dir *os.File, name string, flags int) (*os.File, error) {
	fd, err := syscall.Openat(int(dir.Fd()), name, flags|syscall.O_NOFOLLOW|syscall.O_CLOEXEC|syscall.O_NONBLOCK, 0600)
	if err != nil {
		return nil, err
	}
	f := os.NewFile(uintptr(fd), name)
	st, err := f.Stat()
	if err != nil {
		f.Close()
		return nil, err
	}
	v := st.Sys().(*syscall.Stat_t)
	if !st.Mode().IsRegular() || st.Mode().Perm() != 0600 || v.Uid != uint32(os.Getuid()) || v.Nlink != 1 {
		f.Close()
		return nil, fmt.Errorf("unsafe state file")
	}
	return f, nil
}
func lock(ctx context.Context, f *os.File) error {
	for {
		err := syscall.Flock(int(f.Fd()), syscall.LOCK_EX|syscall.LOCK_NB)
		if err == nil {
			return nil
		}
		if err != syscall.EWOULDBLOCK && err != syscall.EAGAIN {
			return err
		}
		select {
		case <-ctx.Done():
			return ctx.Err()
		case <-time.After(10 * time.Millisecond):
		}
	}
}
func validateDB(d database) error {
	if d.Version != 1 || d.Sessions == nil || d.Records == nil {
		return fmt.Errorf("invalid database")
	}
	sessions := map[string]bool{}
	records := map[string]bool{}
	pending := map[string]int{}
	liveSessions := 0
	for _, v := range d.Sessions {
		if !transport.ValidID(v.ID) || sessions[v.ID] || v.Boot == "" || v.Start < 0 || v.Until < v.Start || v.Until-v.Start > int64(contracts.MaxSessionSeconds)*int64(time.Second) || v.CreatedUnix <= 0 {
			return fmt.Errorf("invalid session")
		}
		sessions[v.ID] = true
		if !v.Closed {
			liveSessions++
		}
	}
	if liveSessions > transport.MaxLiveSessions {
		return fmt.Errorf("too many live sessions")
	}
	for _, v := range d.Records {
		k := v.Request.SessionID + v.Request.RequestID
		if v.Request.Validate() != nil || v.Request.Operation != contracts.Submit || v.Reply.Validate() != nil || v.Reply.SessionID != v.Request.SessionID || v.Reply.RequestID != v.Request.RequestID || !sessions[v.Request.SessionID] || records[k] {
			return fmt.Errorf("invalid record")
		}
		records[k] = true
		if !terminal(v.Reply.Status) {
			pending[v.Request.SessionID]++
			if pending[v.Request.SessionID] > transport.MaxPendingPerSession {
				return fmt.Errorf("too many pending requests in session")
			}
		}
	}
	return nil
}
func (s *Store) transaction(ctx context.Context, fn func(*database, string, int64) error) error {
	dir, err := openDir(s.Dir)
	if err != nil {
		return err
	}
	defer dir.Close()
	lf, err := openPrivate(dir, "store.lock", syscall.O_RDWR|syscall.O_CREAT)
	if err != nil {
		return err
	}
	defer lf.Close()
	if err = lock(ctx, lf); err != nil {
		return err
	}
	defer syscall.Flock(int(lf.Fd()), syscall.LOCK_UN)
	d := database{Version: 1, Sessions: []session{}, Records: []record{}}
	f, err := openPrivate(dir, "state.json", syscall.O_RDONLY)
	if err == nil {
		var info os.FileInfo
		info, err = f.Stat()
		if err == nil && info.Size() > MaxStateBytes {
			err = fmt.Errorf("state exceeds bound")
		}
		if err == nil {
			err = transport.DecodeStrict(f, MaxStateBytes, &d)
		}
		f.Close()
	} else if errors.Is(err, os.ErrNotExist) {
		err = nil
	}
	if err != nil {
		return err
	}
	if err = validateDB(d); err != nil {
		return err
	}
	boot, now, err := s.clock()
	if err != nil {
		return err
	}
	// Expiry is persisted before handling a request. Never revive a dead route.
	for i := range d.Sessions {
		if !live(d.Sessions[i], boot, now) {
			d.Sessions[i].Closed = true
		}
	}
	for i := range d.Records {
		r := &d.Records[i]
		for _, v := range d.Sessions {
			if v.ID == r.Request.SessionID && v.Closed && !r.Ack && r.Reply.Status == contracts.Pending {
				r.Reply = reply(r.Request, "session_expired")
			}
		}
	}
	// Retention is session-based, never extended by polling. A boot-local stale
	// route is never reopened; after seven days the host must still use fresh IDs.
	kept := d.Sessions[:0]
	valid := map[string]bool{}
	for _, v := range d.Sessions {
		if !v.Closed || time.Now().Unix()-v.CreatedUnix < contracts.ReleasedRetentionSeconds {
			kept = append(kept, v)
			valid[v.ID] = true
		}
	}
	d.Sessions = kept
	rows := d.Records[:0]
	for _, v := range d.Records {
		if valid[v.Request.SessionID] {
			rows = append(rows, v)
		}
	}
	d.Records = rows
	if err = fn(&d, boot, now); err != nil {
		return err
	}
	if err = validateDB(d); err != nil {
		return err
	}
	b, err := json.Marshal(d)
	if err != nil {
		return err
	}
	if len(b) > MaxStateBytes {
		return fmt.Errorf("state exceeds bound")
	}
	// A fixed scratch name is safe under the stable lock. Remove crash leftovers
	// without opening them; no guest-controlled filenames are consumed.
	if err = syscall.Unlinkat(int(dir.Fd()), "state.tmp"); err != nil && err != syscall.ENOENT {
		return err
	}
	tmp, err := openPrivate(dir, "state.tmp", syscall.O_WRONLY|syscall.O_CREAT|syscall.O_EXCL)
	if err != nil {
		return err
	}
	defer tmp.Close()
	defer syscall.Unlinkat(int(dir.Fd()), "state.tmp")
	if _, err = io.Copy(tmp, bytes.NewReader(b)); err != nil {
		return err
	}
	if err = tmp.Sync(); err != nil {
		return err
	}
	if err = tmp.Close(); err != nil {
		return err
	}
	if err = syscall.Renameat(int(dir.Fd()), "state.tmp", int(dir.Fd()), "state.json"); err != nil {
		return err
	}
	return dir.Sync()
}
func reply(q contracts.Request, code string) contracts.Reply {
	r := contracts.Reply{Version: 1, SessionID: q.SessionID, RequestID: q.RequestID, Status: contracts.Pending}
	if code != "" {
		r.Status = contracts.Rejected
		r.Error = code
	}
	return r
}
func terminal(s contracts.CoarseStatus) bool {
	return s == contracts.Released || s == contracts.Rejected || s == contracts.Cancelled || s == contracts.Uncertain
}

// Handle never executes, approves, or reads a host file.
func (s *Store) Handle(ctx context.Context, q contracts.Request) (contracts.Reply, error) {
	if err := q.Validate(); err != nil {
		return contracts.Reply{}, err
	}
	var out contracts.Reply
	err := s.transaction(ctx, func(d *database, boot string, now int64) error {
		active := false
		for _, v := range d.Sessions {
			if v.ID == q.SessionID {
				active = live(v, boot, now)
			}
		}
		for i := range d.Records {
			r := &d.Records[i]
			if r.Request.SessionID != q.SessionID || r.Request.RequestID != q.RequestID {
				continue
			}
			if q.Operation == contracts.Submit && !reflect.DeepEqual(q.Proposal, r.Request.Proposal) {
				out = reply(q, "conflict")
				return nil
			}
			if q.Operation == contracts.Cancel && !terminal(r.Reply.Status) {
				r.Cancel = true
			}
			out = r.Reply
			return nil
		}
		if q.Operation != contracts.Submit {
			out = reply(q, "not_found")
			return nil
		}
		if !active {
			out = reply(q, "no_reviewer")
			return nil
		}
		pending := 0
		for _, r := range d.Records {
			if r.Request.SessionID == q.SessionID && !terminal(r.Reply.Status) {
				pending++
			}
		}
		if pending >= transport.MaxPendingPerSession {
			out = reply(q, "queue_full")
			return nil
		}
		out = reply(q, "")
		d.Records = append(d.Records, record{Request: q, Reply: out})
		return nil
	})
	return out, err
}
func (s *Store) Exchange(ctx context.Context, q transport.Request) (transport.Response, error) {
	if err := q.Validate(); err != nil {
		return transport.Response{}, err
	}
	out := transport.Response{Version: 1, SessionID: q.SessionID, Items: []transport.Item{}}
	err := s.transaction(ctx, func(d *database, boot string, now int64) error {
		idx := -1
		for i, v := range d.Sessions {
			if v.ID == q.SessionID {
				idx = i
				break
			}
		}
		fail := func(code string) error {
			out.Error = code
			out.LeaseSeconds = 0
			out.Items = []transport.Item{}
			return nil
		}
		if q.Operation == "announce" {
			if idx >= 0 {
				v := d.Sessions[idx]
				if !live(v, boot, now) {
					return fail("session_expired")
				}
				if v.Label != q.Label {
					return fail("conflict")
				}
			} else {
				liveSessions := 0
				for _, v := range d.Sessions {
					if live(v, boot, now) {
						liveSessions++
					}
				}
				if liveSessions >= transport.MaxLiveSessions {
					return fail("queue_full")
				}
				d.Sessions = append(d.Sessions, session{ID: q.SessionID, Label: q.Label, Boot: boot, Start: now, Until: now + int64(transport.LeaseSeconds)*int64(time.Second), CreatedUnix: time.Now().Unix()})
				idx = len(d.Sessions) - 1
			}
		}
		if idx < 0 {
			return fail("not_found")
		}
		v := &d.Sessions[idx]
		if q.Operation == "close" {
			v.Closed = true
			for i := range d.Records {
				r := &d.Records[i]
				if r.Request.SessionID == q.SessionID && !r.Ack && r.Reply.Status == contracts.Pending {
					r.Reply = reply(r.Request, "session_expired")
				}
			}
			return nil
		}
		if !live(*v, boot, now) {
			return fail("session_expired")
		}
		if q.Operation == "renew" {
			v.Until = now + int64(transport.LeaseSeconds)*int64(time.Second)
			hard := v.Start + int64(contracts.MaxSessionSeconds)*int64(time.Second)
			if v.Until > hard {
				v.Until = hard
			}
		}
		out.LeaseSeconds = int((v.Until - now + int64(time.Second) - 1) / int64(time.Second))
		switch q.Operation {
		case "pull":
			for _, r := range d.Records {
				if r.Request.SessionID == q.SessionID && !terminal(r.Reply.Status) {
					out.Items = append(out.Items, transport.Item{Request: r.Request, CancelRequested: r.Cancel, Acknowledged: r.Ack})
				}
			}
		case "publish":
			for i := range d.Records {
				r := &d.Records[i]
				if r.Request.SessionID != q.SessionID || r.Request.RequestID != q.Reply.RequestID {
					continue
				}
				if terminal(r.Reply.Status) {
					if !reflect.DeepEqual(r.Reply, *q.Reply) {
						return fail("conflict")
					}
					return nil
				}
				if !coarseTransition(r.Reply.Status, q.Reply.Status) {
					return fail("conflict")
				}
				// Never retain the caller's mutable artifact slices.
				b, _ := json.Marshal(q.Reply)
				copy, err := contracts.ParseReply(bytes.NewReader(b))
				if err != nil {
					return err
				}
				r.Reply = copy
				r.Ack = true
				return nil
			}
			return fail("not_found")
		}
		return nil
	})
	return out, err
}

// This only prevents accidental coarse regressions. It is not host approval.
// Active may return to pending after a known authentication denial/cancellation.
// The host alone retains spent nonces and requires a fresh approval to retry;
// no authentication outcome or capability is disclosed in this coarse view.
func coarseTransition(from, to contracts.CoarseStatus) bool {
	if from == to {
		return true
	}
	switch from {
	case contracts.Pending:
		return true
	case contracts.Active:
		return true
	case contracts.Held:
		return to == contracts.Released || to == contracts.Uncertain
	}
	return false
}
