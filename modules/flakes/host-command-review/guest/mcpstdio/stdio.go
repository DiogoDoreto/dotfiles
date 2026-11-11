package mcpstdio

import (
	"bufio"
	"context"
	"fmt"
	"io"
	"os"
	"syscall"
	"time"

	"github.com/modelcontextprotocol/go-sdk/mcp"
	"host-command-review/guest"
	"host-command-review/transport"
)

// FrameReader adds a hard cap before the SDK decodes a frame and rejects
// duplicate/escaped-aliased keys and malformed Unicode at every depth. JSON-RPC
// negotiation, request dispatch and cancellation remain the official SDK's job.
// An incomplete frame is bounded in bytes; RunStdio additionally bounds time.
type FrameReader struct {
	source  io.ReadCloser
	lines   *bufio.Reader
	pending []byte
}

func NewFrameReader(r io.ReadCloser) *FrameReader {
	return &FrameReader{source: r, lines: bufio.NewReaderSize(r, 4096)}
}
func (r *FrameReader) Close() error { return r.source.Close() }
func (r *FrameReader) Read(p []byte) (int, error) {
	if len(r.pending) == 0 {
		var frame []byte
		for {
			part, err := r.lines.ReadSlice('\n')
			if len(frame)+len(part) > MaxMCPFrameBytes {
				return 0, fmt.Errorf("frame exceeds bound")
			}
			frame = append(frame, part...)
			if err == bufio.ErrBufferFull {
				continue
			}
			if err != nil {
				if err == io.EOF && len(frame) == 0 {
					return 0, io.EOF
				}
				return 0, fmt.Errorf("incomplete frame")
			}
			break
		}
		if err := transport.CheckJSON(frame); err != nil {
			return 0, fmt.Errorf("invalid frame")
		}
		r.pending = frame
	}
	n := copy(p, r.pending)
	r.pending = r.pending[n:]
	return n, nil
}

// timedInput allows idle MCP sessions indefinitely but gives each frame 10s
// from its first byte. Closing the source unblocks an incomplete pipe read.
type timedInput struct {
	f     *os.File
	first bool
}

func (r *timedInput) Read(p []byte) (int, error) {
	if r.first {
		p = p[:1]
	}
	n, err := r.f.Read(p)
	if n > 0 && r.first {
		r.first = false
		_ = r.f.SetReadDeadline(time.Now().Add(10 * time.Second))
	}
	// NewFrameReader's buffering can read beyond a newline; use one byte reads
	// here so deadlines are reset exactly at framing boundaries, not ahead of it.
	if n > 0 && p[n-1] == '\n' {
		r.first = true
		_ = r.f.SetReadDeadline(time.Time{})
	}
	return n, err
}
func (r *timedInput) Close() error { return r.f.Close() }

// RunStdio requires pollable stdin/stdout (normal MCP pipes). Reject unsupported
// descriptors rather than silently losing deadlines. No protocol logs on stdout.
func RunStdio(ctx context.Context, s *guest.Store, in, out *os.File) error {
	// Inherited stdio descriptors are usually blocking and not registered with
	// Go's poller. Duplicate then explicitly register nonblocking pipe handles.
	var err error
	in, err = pollable(in)
	if err != nil {
		return err
	}
	defer in.Close()
	out, err = pollable(out)
	if err != nil {
		return err
	}
	defer out.Close()
	if err := in.SetReadDeadline(time.Time{}); err != nil {
		return err
	}
	if err := out.SetWriteDeadline(time.Time{}); err != nil {
		return err
	}
	reader := NewFrameReader(&byteInput{timedInput: timedInput{f: in, first: true}})
	return NewMCPServer(s).Run(ctx, &mcp.IOTransport{Reader: reader, Writer: &timedOutput{f: out}, MaxLineLength: MaxMCPFrameBytes})
}

func pollable(f *os.File) (*os.File, error) {
	fd, err := syscall.Dup(int(f.Fd()))
	if err != nil {
		return nil, err
	}
	syscall.CloseOnExec(fd)
	if err = syscall.SetNonblock(fd, true); err != nil {
		syscall.Close(fd)
		return nil, err
	}
	return os.NewFile(uintptr(fd), f.Name()), nil
}

type byteInput struct{ timedInput }

func (r *byteInput) Read(p []byte) (int, error) {
	if len(p) > 1 {
		p = p[:1]
	}
	return r.timedInput.Read(p)
}

type timedOutput struct{ f *os.File }

func (w *timedOutput) Write(b []byte) (int, error) {
	if len(b) > MaxMCPResponseBytes {
		return 0, fmt.Errorf("response exceeds bound")
	}
	if err := w.f.SetWriteDeadline(time.Now().Add(10 * time.Second)); err != nil {
		return 0, err
	}
	return w.f.Write(b)
}
func (w *timedOutput) Close() error { return nil }
