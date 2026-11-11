package execution

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strconv"
	"syscall"

	"host-command-review/contracts"
)

var storeExe = regexp.MustCompile(`^/nix/store/[0-9a-z]{32}-[A-Za-z0-9+._?=-]+/bin/[A-Za-z0-9._+-]+$`)

// Config is trusted build-time policy, never read from guest/browser input or
// environment. Empty command-entrypoint defaults deliberately disable execution.
type Config struct {
	Shell, SystemdRun, Systemctl, Helper, Runner, RuntimeDir string
	ExpectedUID                                              int
}

func (c Config) Validate() error {
	for _, p := range []string{c.Shell, c.SystemdRun, c.Systemctl, c.Helper, c.Runner} {
		if !storeExe.MatchString(p) {
			return errors.New("fixed Nix executable required")
		}
	}
	if c.ExpectedUID <= 0 || !filepath.IsAbs(c.RuntimeDir) || filepath.Clean(c.RuntimeDir) != c.RuntimeDir {
		return errors.New("invalid trusted identity/runtime directory")
	}
	return nil
}
func (c Config) CheckRunner(root bool) error {
	if e := c.Validate(); e != nil {
		return e
	}
	exe, e := os.Executable()
	if e != nil || exe != c.Runner {
		return errors.New("wrong runner executable")
	}
	if root {
		if os.Getuid() != 0 || os.Geteuid() != 0 {
			return errors.New("root runner requires root")
		}
		uid := os.Getenv("PKEXEC_UID")
		if uid != strconv.Itoa(c.ExpectedUID) {
			return errors.New("unexpected pkexec caller")
		}
	} else if os.Getuid() != c.ExpectedUID || os.Geteuid() != c.ExpectedUID {
		return errors.New("unexpected reviewer uid")
	}
	return nil
}
func (c Config) checkHelper(s contracts.ExecutionSpec) error {
	if e := c.Validate(); e != nil {
		return e
	}
	exe, e := os.Executable()
	if e != nil || exe != c.Helper {
		return errors.New("wrong helper executable")
	}
	uid := c.ExpectedUID
	if s.User == contracts.Root {
		uid = 0
	}
	if os.Getuid() != uid || os.Geteuid() != uid {
		return errors.New("wrong helper uid")
	}
	if uid == 0 {
		if os.Getgid() != 0 || os.Getegid() != 0 {
			return errors.New("wrong root groups")
		}
		groups, e := os.Getgroups()
		if e != nil {
			return e
		}
		for _, g := range groups {
			if g != 0 {
				return errors.New("unexpected root supplementary group")
			}
		}
	}
	return nil
}
func mode(s contracts.ExecutionSpec) string {
	if s.User == contracts.Root {
		return "--system"
	}
	return "--user"
}

// UnitArgs contains no command text, cwd, environment values or payload output.
func (c Config) UnitArgs(s contracts.ExecutionSpec, unit, digest string) ([]string, error) {
	if c.Validate() != nil || s.Validate() != nil || s.Shell != c.Shell || !unitPattern.MatchString(unit) || !validDigest(digest) {
		return nil, errors.New("invalid unit plan")
	}
	return []string{mode(s), "--no-ask-password", "--quiet", "--wait", "--pipe", "--collect", "--service-type=exec", "--expand-environment=no", "--unit=" + unit, fmt.Sprintf("--property=RuntimeMaxSec=%ds", s.TimeoutSeconds), "--property=TimeoutStartSec=5s", "--property=TimeoutStopSec=5s", "--property=KillMode=control-group", "--property=SendSIGKILL=yes", "--property=Restart=no", "--property=UMask=0077", c.Helper, "--unit-payload", "--approved-sha256", digest}, nil
}

var unitPattern = regexp.MustCompile(`^host-review-[0-9a-f]{32}\.service$`)

func cleanCLIEnv(root bool) []string {
	env := []string{"PATH=/run/current-system/sw/bin", "LANG=C.UTF-8", "HOME=/root"}
	if !root {
		env[2] = "HOME=/home/dog"
		env = append(env, fmt.Sprintf("XDG_RUNTIME_DIR=/run/user/%d", os.Getuid()))
	}
	return env
}

// PKExecArgs is one prompt per consumed command. The dedicated action MUST bind
// this exact runner path/argv1 with auth_admin (never _keep); packaging owns XML.
func (c Config) PKExecArgs(digest string) (string, []string, error) {
	if c.Validate() != nil || !validDigest(digest) {
		return "", nil, errors.New("invalid root invocation")
	}
	const wrapper = "/run/wrappers/bin/pkexec"
	st, e := os.Stat(wrapper)
	if e != nil {
		return "", nil, e
	}
	sys, ok := st.Sys().(*syscall.Stat_t)
	if !ok || sys.Uid != 0 || st.Mode()&os.ModeSetuid == 0 || st.Mode().Perm()&0022 != 0 || !st.Mode().IsRegular() {
		return "", nil, errors.New("unsafe pkexec wrapper")
	}
	return wrapper, []string{"--disable-internal-agent", c.Runner, "--approved-sha256", digest}, nil
}
