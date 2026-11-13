package host

import (
	"host-command-review/contracts"
	"host-command-review/execution"
)

// LiveCapture is ephemeral host-only data. It is never part of a ledger record
// or guest status, and a RunID is not an approval capability.
type LiveCapture struct {
	RunID     string `json:"run_id"`
	Sequence  uint64 `json:"sequence"`
	Stdout    []byte `json:"stdout"`
	Stderr    []byte `json:"stderr"`
	Truncated bool   `json:"truncated"`
}

func (c *Controller) clearLive(id, token string) {
	c.liveMu.Lock()
	defer c.liveMu.Unlock()
	if v := c.live[id]; v != nil && (token == "" || v.RunID == token) {
		delete(c.live, id)
	}
}
func (c *Controller) observeLive(id, token string, p execution.Progress) {
	// Never acquire the store or application lifecycle locks here.
	c.liveMu.Lock()
	defer c.liveMu.Unlock()
	v := c.live[id]
	if v == nil || v.RunID != token {
		return
	}
	if p.Stdout == nil || p.Stderr == nil || int(p.StdoutOffset) != len(v.Stdout) || int(p.StderrOffset) != len(v.Stderr) || len(p.Stdout)+len(p.Stderr) > execution.ProgressBytes || len(v.Stdout)+len(v.Stderr)+len(p.Stdout)+len(p.Stderr) > contracts.CaptureBytes || (v.Truncated && !p.Truncated) || v.Sequence == ^uint64(0) {
		delete(c.live, id)
		return
	}
	v.Stdout = append(v.Stdout, p.Stdout...)
	v.Stderr = append(v.Stderr, p.Stderr...)
	v.Truncated = p.Truncated
	v.Sequence++
}
func (c *Controller) copyLive(id string) *LiveCapture {
	c.liveMu.Lock()
	defer c.liveMu.Unlock()
	v := c.live[id]
	if v == nil {
		return nil
	}
	out := *v
	out.Stdout = append([]byte{}, v.Stdout...)
	out.Stderr = append([]byte{}, v.Stderr...)
	return &out
}
