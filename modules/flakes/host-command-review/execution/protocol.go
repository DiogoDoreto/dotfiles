// Package execution implements the private per-command supervisor protocol.
// It never falls back from transient systemd units to process-group execution.
package execution

import (
	"bytes"
	"crypto/rand"
	"crypto/sha256"
	"encoding/binary"
	"encoding/hex"
	"encoding/json"
	"errors"
	"io"
	"os"
	"time"

	"host-command-review/contracts"
)

const MaxFrame = contracts.MaxArtifactBytes
const Handshake = 5 * time.Second
const Lease = 2 * time.Second
const Renew = 500 * time.Millisecond

type Frame struct {
	Type string `json:"type"`
	Data []byte `json:"data"`
}

func WriteFrame(w io.Writer, typ string, data []byte) error {
	b, e := json.Marshal(Frame{typ, data})
	if e != nil {
		return e
	}
	if len(b) > MaxFrame {
		return errors.New("oversize frame")
	}
	var h [4]byte
	binary.BigEndian.PutUint32(h[:], uint32(len(b)))
	if e = writeFull(w, h[:]); e != nil {
		return e
	}
	return writeFull(w, b)
}
func writeFull(w io.Writer, b []byte) error {
	for len(b) > 0 {
		n, e := w.Write(b)
		if e != nil {
			return e
		}
		if n <= 0 {
			return io.ErrShortWrite
		}
		b = b[n:]
	}
	return nil
}
func ReadFrame(r io.Reader) (Frame, error) {
	var f Frame
	var h [4]byte
	if _, e := io.ReadFull(r, h[:]); e != nil {
		return f, e
	}
	n := binary.BigEndian.Uint32(h[:])
	if n == 0 || n > MaxFrame {
		return f, errors.New("invalid frame size")
	}
	b := make([]byte, n)
	if _, e := io.ReadFull(r, b); e != nil {
		return f, e
	}
	d := json.NewDecoder(bytes.NewReader(b))
	d.DisallowUnknownFields()
	if e := d.Decode(&f); e != nil {
		return f, e
	}
	canonical, _ := json.Marshal(f)
	if !bytes.Equal(canonical, b) || f.Data == nil {
		return Frame{}, errors.New("invalid frame encoding")
	}
	return f, nil
}
func send(w *os.File, typ string, data []byte) error {
	if data == nil {
		data = []byte{}
	}
	if e := w.SetWriteDeadline(time.Now().Add(Handshake)); e != nil {
		return e
	}
	return WriteFrame(w, typ, data)
}
func receive(r *os.File, d time.Duration) (Frame, error) {
	if e := r.SetReadDeadline(time.Now().Add(d)); e != nil {
		return Frame{}, e
	}
	return ReadFrame(r)
}
func nonce() []byte {
	b := make([]byte, 32)
	if _, e := rand.Read(b); e != nil {
		panic(e)
	}
	return b
}
func validDigest(s string) bool {
	if len(s) != 64 {
		return false
	}
	b, e := hex.DecodeString(s)
	return e == nil && hex.EncodeToString(b) == s
}
func Manifest(s contracts.ExecutionSpec) ([]byte, string, error) {
	if e := s.Validate(); e != nil {
		return nil, "", e
	}
	b, e := json.Marshal(s)
	sum := sha256.Sum256(b)
	return b, hex.EncodeToString(sum[:]), e
}
func parseManifest(b []byte, digest string, c Config) (contracts.ExecutionSpec, error) {
	var zero contracts.ExecutionSpec
	if len(b) > contracts.MaxRequestBytes || !validDigest(digest) {
		return zero, errors.New("bad manifest")
	}
	sum := sha256.Sum256(b)
	if hex.EncodeToString(sum[:]) != digest {
		return zero, errors.New("digest mismatch")
	}
	s, e := contracts.ParseExecution(bytes.NewReader(b))
	if e != nil {
		return zero, e
	}
	canonical, _ := json.Marshal(s)
	if !bytes.Equal(canonical, b) || s.Shell != c.Shell {
		return zero, errors.New("untrusted manifest policy")
	}
	return s, nil
}

type Result struct {
	Started   bool               `json:"started"`
	Uncertain bool               `json:"uncertain"`
	Stdout    []byte             `json:"stdout"`
	Stderr    []byte             `json:"stderr"`
	Truncated bool               `json:"truncated"`
	Exit      contracts.ExitInfo `json:"exit"`
}

func unknown(started bool) Result {
	return Result{Started: started, Uncertain: true, Stdout: []byte{}, Stderr: []byte{}, Exit: contracts.ExitInfo{Kind: "unknown", Code: -1}}
}
func (r Result) Validate() error {
	if r.Stdout == nil || r.Stderr == nil || len(r.Stdout)+len(r.Stderr) > contracts.CaptureBytes {
		return errors.New("invalid capture")
	}
	switch r.Exit.Kind {
	case "exited":
		if r.Exit.Code < 0 || r.Exit.Code > 255 || r.Exit.Signal != 0 {
			return errors.New("bad exit")
		}
	case "signaled":
		if r.Exit.Code != -1 || r.Exit.Signal < 1 || r.Exit.Signal > 64 {
			return errors.New("bad signal")
		}
	case "unknown", "cancelled", "timed_out":
		if r.Exit.Code != -1 || r.Exit.Signal != 0 {
			return errors.New("bad outcome")
		}
	default:
		return errors.New("bad outcome")
	}
	if r.Uncertain && r.Exit.Kind != "unknown" {
		return errors.New("uncertain result claims status")
	}
	if !r.Uncertain && (!r.Started || r.Exit.Kind == "unknown") {
		return errors.New("terminal lacks confirmed payload outcome")
	}
	return nil
}
func resultBytes(r Result) []byte { b, _ := json.Marshal(r); return b }
func parseResult(b []byte) (Result, error) {
	var r Result
	d := json.NewDecoder(bytes.NewReader(b))
	d.DisallowUnknownFields()
	if e := d.Decode(&r); e != nil {
		return r, e
	}
	canonical, _ := json.Marshal(r)
	if !bytes.Equal(canonical, b) {
		return r, errors.New("bad terminal frame")
	}
	return r, r.Validate()
}
