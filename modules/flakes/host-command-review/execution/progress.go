package execution

import (
	"bytes"
	"encoding/json"
	"errors"
	"os"
	"time"

	"host-command-review/contracts"
)

// Progress is a bounded delta in the retained, host-private capture prefix.
type Progress struct {
	StdoutOffset uint32 `json:"stdout_offset"`
	StderrOffset uint32 `json:"stderr_offset"`
	Stdout       []byte `json:"stdout"`
	Stderr       []byte `json:"stderr"`
	Truncated    bool   `json:"truncated"`
}

// Observer must be trusted, synchronous, and limited to small in-memory work.
type Observer func(Progress)

const ProgressBytes = 32 * 1024

type progressState struct {
	stdout, stderr []byte
	truncated      bool
}

func (s *progressState) accept(p Progress) error {
	if p.Stdout == nil || p.Stderr == nil || len(p.Stdout)+len(p.Stderr) > ProgressBytes || int(p.StdoutOffset) != len(s.stdout) || int(p.StderrOffset) != len(s.stderr) || len(s.stdout)+len(s.stderr)+len(p.Stdout)+len(p.Stderr) > contracts.CaptureBytes || (s.truncated && !p.Truncated) {
		return errors.New("invalid progress")
	}
	s.stdout = append(s.stdout, p.Stdout...)
	s.stderr = append(s.stderr, p.Stderr...)
	s.truncated = p.Truncated
	return nil
}
func (s *progressState) final(r Result) error {
	if len(r.Stdout) < len(s.stdout) || len(r.Stderr) < len(s.stderr) || !bytes.Equal(r.Stdout[:len(s.stdout)], s.stdout) || !bytes.Equal(r.Stderr[:len(s.stderr)], s.stderr) || (s.truncated && !r.Truncated) {
		return errors.New("terminal capture disagrees with progress")
	}
	return nil
}
func parseProgress(b []byte) (Progress, error) {
	var p Progress
	d := json.NewDecoder(bytes.NewReader(b))
	d.DisallowUnknownFields()
	if e := d.Decode(&p); e != nil {
		return p, e
	}
	canonical, _ := json.Marshal(p)
	if !bytes.Equal(b, canonical) {
		return p, errors.New("noncanonical progress")
	}
	return p, nil
}
func progressBytes(p Progress) []byte { b, _ := json.Marshal(p); return b }

// The read deadline is absolute across all events, not renewed by progress.
func receiveResult(out *os.File, deadline time.Time, observe Observer) (Result, error) {
	var state progressState
	if e := out.SetReadDeadline(deadline); e != nil {
		return Result{}, e
	}
	for {
		f, e := ReadFrame(out)
		if e != nil {
			return Result{}, e
		}
		switch f.Type {
		case "progress":
			p, e := parseProgress(f.Data)
			if e != nil {
				return Result{}, e
			}
			if e = state.accept(p); e != nil {
				return Result{}, e
			}
			if observe != nil {
				observe(p)
			}
		case "result":
			r, e := parseResult(f.Data)
			if e == nil {
				e = state.final(r)
			}
			return r, e
		default:
			return Result{}, errors.New("unexpected execution frame")
		}
	}
}
