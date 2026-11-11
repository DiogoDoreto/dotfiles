// Package probe is an unprivileged feasibility experiment, NOT a root runner.
package probe

import (
	"crypto/sha256"
	"crypto/subtle"
	"encoding/binary"
	"encoding/hex"
	"errors"
	"fmt"
	"io"
)

const MaxFrame = 64 << 10
const CaptureLimit = 4 << 20

// Frames are length (big endian uint32, including type), type byte, body.
// Stream pipes need framing; SOCK_SEQPACKET is unavailable across pkexec's fd close.
type Frame struct {
	Type byte
	Body []byte
}

const (
	Manifest byte = iota + 1
	Ready
	Start
	Lease
	Cancel
)

func ReadFrame(r io.Reader) (Frame, error) {
	var n uint32
	if err := binary.Read(r, binary.BigEndian, &n); err != nil {
		return Frame{}, err
	}
	if n < 1 || n > MaxFrame {
		return Frame{}, errors.New("invalid frame length")
	}
	b := make([]byte, n)
	if _, err := io.ReadFull(r, b); err != nil {
		return Frame{}, err
	}
	return Frame{b[0], b[1:]}, nil
}
func WriteFrame(w io.Writer, f Frame) error {
	if len(f.Body)+1 > MaxFrame {
		return errors.New("frame too large")
	}
	b := make([]byte, 5+len(f.Body))
	binary.BigEndian.PutUint32(b, uint32(1+len(f.Body)))
	b[4] = f.Type
	copy(b[5:], f.Body)
	for len(b) > 0 {
		n, err := w.Write(b)
		if err != nil {
			return err
		}
		if n == 0 {
			return io.ErrShortWrite
		}
		b = b[n:]
	}
	return nil
}

// VerifyApproved binds the exact bytes, not a reserialization. Schema validation
// (including immutable shell, clean environment, cwd, identity and timeout) is
// deliberately owned by the parent project's contracts, not defined here.
func VerifyApproved(raw []byte, approved string) error {
	digest, err := hex.DecodeString(approved)
	if err != nil || len(digest) != sha256.Size || approved != hex.EncodeToString(digest) {
		return errors.New("expected canonical lowercase SHA256")
	}
	sum := sha256.Sum256(raw)
	if subtle.ConstantTimeCompare(sum[:], digest) != 1 {
		return fmt.Errorf("approval digest mismatch")
	}
	return nil
}
