package reviewer

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"host-command-review/transport"
	"io"
	"os/exec"
	"time"
)

const RemoteCommand = "/run/current-system/sw/bin/host-review-exchange"

var ErrTransport = errors.New("guest exchange unavailable or invalid")

type Exchange interface {
	Call(context.Context, transport.Request) (transport.Response, error)
}

// SSH fields are fixed trusted packaging policy, never browser or guest input.
type SSH struct{ Executable, Address, User, Identity, KnownHosts string }

func (s SSH) Args() []string {
	return []string{"-F", "/dev/null", "-T", "-p", "22", "-i", s.Identity,
		"-o", "BatchMode=yes", "-o", "IdentitiesOnly=yes", "-o", "IdentityAgent=none", "-o", "StrictHostKeyChecking=yes",
		"-o", "UserKnownHostsFile=" + s.KnownHosts, "-o", "GlobalKnownHostsFile=/dev/null", "-o", "HostKeyAlgorithms=ssh-ed25519",
		"-o", "UpdateHostKeys=no", "-o", "VerifyHostKeyDNS=no", "-o", "PasswordAuthentication=no", "-o", "KbdInteractiveAuthentication=no",
		"-o", "PreferredAuthentications=publickey", "-o", "ForwardAgent=no", "-o", "ClearAllForwardings=yes", "-o", "ForwardX11=no",
		"-o", "ControlMaster=no", "-o", "ControlPath=none", "-o", "ConnectionAttempts=1", "-o", "ConnectTimeout=3",
		"-o", "ServerAliveInterval=2", "-o", "ServerAliveCountMax=1", "-o", "RequestTTY=no", "-o", "LogLevel=ERROR",
		"--", s.User + "@" + s.Address, RemoteCommand}
}

type cappedBuffer struct {
	bytes.Buffer
	cancel context.CancelFunc
}

func (b *cappedBuffer) Write(p []byte) (int, error) {
	if b.Len()+len(p) > transport.MaxResponseBytes {
		b.cancel()
		return 0, ErrTransport
	}
	return b.Buffer.Write(p)
}
func (s SSH) Call(ctx context.Context, q transport.Request) (transport.Response, error) {
	var zero transport.Response
	if q.Validate() != nil {
		return zero, ErrTransport
	}
	b, e := json.Marshal(q)
	if e != nil || len(b) > transport.MaxRequestBytes {
		return zero, ErrTransport
	}
	ctx, cancel := context.WithTimeout(ctx, 4*time.Second)
	defer cancel()
	cmd := exec.CommandContext(ctx, s.Executable, s.Args()...)
	cmd.Env = []string{"PATH=/run/current-system/sw/bin", "LANG=C.UTF-8", "HOME=/home/dog"}
	cmd.Stdin = bytes.NewReader(b)
	cmd.Stderr = io.Discard
	cmd.WaitDelay = time.Second
	out := &cappedBuffer{cancel: cancel}
	cmd.Stdout = out
	if cmd.Run() != nil {
		return zero, ErrTransport
	}
	r, e := transport.ParseResponse(bytes.NewReader(out.Bytes()))
	if e != nil || r.SessionID != q.SessionID || (q.Operation != "pull" && len(r.Items) != 0) {
		return zero, ErrTransport
	}
	if r.Error != "" {
		return r, ErrTransport
	}
	if q.Operation != "close" && r.LeaseSeconds == 0 {
		return zero, ErrTransport
	}
	return r, nil
}
