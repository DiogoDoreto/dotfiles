package probe

import (
	"errors"
	"fmt"
	"regexp"
	"strconv"
	"time"
)

var unitName = regexp.MustCompile(`^host-review-[a-f0-9]{32}\.service$`)
var storeExecutable = regexp.MustCompile(`^/nix/store/[a-z0-9]{32}-[A-Za-z0-9+._?=-]+/bin/[A-Za-z0-9._-]+$`)

// UnitPlan is a source-supported CLI proposal, NOT a tested systemd transport.
// Its helper receives the manifest on stdin, validates again, makes script fd 3,
// changes cwd/identity/environment, maps command stdin to /dev/null, and execs.
// No script, cwd, environment value or result is encoded in the unit definition.
func UnitPlan(unit, helper, digest string, runtime time.Duration) ([]string, error) {
	if !unitName.MatchString(unit) || !storeExecutable.MatchString(helper) || runtime < time.Millisecond || runtime > MaxRuntime {
		return nil, errors.New("invalid plan")
	}
	// Digest syntax validation without inventing the parent manifest schema.
	if len(digest) != 64 {
		return nil, errors.New("invalid digest")
	}
	for _, c := range digest {
		if !(c >= '0' && c <= '9' || c >= 'a' && c <= 'f') {
			return nil, errors.New("invalid digest")
		}
	}
	return []string{
		"--system", "--no-ask-password", "--quiet", "--wait", "--pipe", "--collect",
		"--service-type=exec", "--expand-environment=no", "--unit=" + unit,
		"--property=RuntimeMaxSec=" + strconv.FormatFloat(runtime.Seconds(), 'f', 3, 64),
		"--property=TimeoutStartSec=5s", "--property=TimeoutStopSec=5s",
		"--property=KillMode=control-group", "--property=SendSIGKILL=yes",
		"--property=Restart=no", "--property=UMask=0077",
		helper, "--unit-payload", "--approved-sha256", digest,
	}, nil
}
func StopPlan(unit string) ([]string, error) {
	if !unitName.MatchString(unit) {
		return nil, fmt.Errorf("invalid unit name")
	}
	return []string{"--system", "--no-ask-password", "stop", unit}, nil
}

// ClassifyPkexec never treats a payload's 126/127 as an auth result once the
// framed runner-ready event was observed. It also never infers payload success
// from pkexec's process status: only the runner's terminal frame can do that.
func ClassifyPkexec(code int, ready, terminal bool) string {
	if terminal {
		return "runner-result"
	}
	if ready {
		return "runner-lost"
	}
	if code == 126 {
		return "authentication-cancelled"
	}
	if code == 127 {
		return "authentication-error-or-runner-failed-before-ready"
	}
	return "no-runner-protocol"
}
