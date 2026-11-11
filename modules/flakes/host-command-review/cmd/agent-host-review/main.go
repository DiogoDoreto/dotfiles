// agent-host-review is manual and foreground-only. It installs no daemon.
package main

import (
	"bufio"
	"context"
	"crypto/sha256"
	"encoding/base64"
	"errors"
	"fmt"
	"host-command-review/execution"
	"host-command-review/host"
	"host-command-review/reviewer"
	"host-command-review/web"
	"io"
	"os"
	"os/exec"
	"os/signal"
	"strconv"
	"strings"
	"syscall"
	"time"
)

var shell, systemdRun, systemctl, helper, runner, rootRuntime, userRuntime, expectedUID string
var ssh, loginctl, dbusMonitor, opener, guestAddress, guestUser, identity string

const state = "/home/dog/.local/state/agent-host-review"
const knownHosts = "/home/dog/.local/state/agent-host-review-pins/known_hosts"

func main() {
	if e := run(); e != nil {
		fmt.Fprintln(os.Stderr, "host reviewer stopped:", e)
		os.Exit(1)
	}
}
func desktop(ctx context.Context) error {
	id := os.Getenv("XDG_SESSION_ID")
	if id == "" {
		return errors.New("local Plasma session required")
	}
	ctx, cancel := context.WithTimeout(ctx, 2*time.Second)
	defer cancel()
	cmd := exec.CommandContext(ctx, loginctl, "show-session", id,
		"--property=Name", "--property=Active", "--property=Remote",
		"--property=Type", "--property=Class", "--property=Desktop")
	cmd.WaitDelay = time.Second
	b, e := cmd.Output()
	if e != nil {
		return errors.New("cannot verify desktop session")
	}
	p := map[string]string{}
	for _, s := range strings.Split(string(b), "\n") {
		k, v, ok := strings.Cut(s, "=")
		if ok {
			p[k] = v
		}
	}
	if p["Name"] != "dog" || p["Active"] != "yes" || p["Remote"] != "no" || p["Class"] != "user" || (p["Type"] != "wayland" && p["Type"] != "x11") || (p["Desktop"] != "KDE" && p["Desktop"] != "plasma") {
		return errors.New("active local dog Plasma session required")
	}
	return nil
}
func run() error {
	syscall.Umask(0077)
	uid, e := strconv.Atoi(expectedUID)
	if e != nil || uid <= 0 || os.Getuid() != uid || os.Geteuid() != uid {
		return errors.New("packaged dog identity required")
	}
	if len(os.Args) > 1 {
		if len(os.Args) == 2 && os.Args[1] == "--cleanup" {
			return cleanup()
		}
		if len(os.Args) == 4 && os.Args[1] == "--enroll" {
			return enroll(os.Args[2], os.Args[3])
		}
		return errors.New("usage: agent-host-review [--cleanup | --enroll TRUSTED_PUBLIC_KEY SHA256:FINGERPRINT]")
	}
	ctx, stop := signal.NotifyContext(context.Background(), os.Interrupt, syscall.SIGTERM, syscall.SIGHUP)
	defer stop()
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()
	cfg := execution.Config{Shell: shell, SystemdRun: systemdRun, Systemctl: systemctl, Helper: helper, Runner: runner, RuntimeDir: userRuntime, ExpectedUID: uid}
	if e = cfg.Validate(); e != nil {
		return e
	}
	if e = desktop(ctx); e != nil {
		return e
	}
	if e = checkPin(); e != nil {
		return e
	}
	if e = os.Mkdir(userRuntime, 0700); e != nil && !os.IsExist(e) {
		return e
	}
	store, e := host.OpenStore(state, nil)
	if e != nil {
		return e
	}
	defer store.Close()
	controller, e := host.NewController(store, host.Config{Shell: shell, BrowserLease: 5 * time.Second})
	if e != nil {
		return e
	}
	defer controller.CloseUnclean()
	adapter := &reviewer.Adapter{Controller: controller, Execution: cfg, Context: ctx, Stop: cancel}
	server, url, e := web.New(adapter)
	if e != nil {
		return e
	}
	defer server.Close()
	monitorCtx, monitorStop := context.WithCancel(context.Background())
	defer monitorStop()
	monErr, ready, monitorDone := monitor(monitorCtx, controller)
	defer func() { monitorStop(); <-monitorDone }()
	select {
	case <-ready:
	case e := <-monErr:
		return e
	case <-time.After(3 * time.Second):
		return errors.New("suspend monitor unavailable")
	}
	workerCtx, workerStop := context.WithCancel(context.Background())
	defer workerStop()
	worker := reviewer.Worker{Controller: controller, Exchange: reviewer.SSH{Executable: ssh, Address: guestAddress, User: guestUser, Identity: identity, KnownHosts: knownHosts}}
	workerErr := make(chan error, 1)
	workerDone := make(chan struct{})
	go func() { defer close(workerDone); workerErr <- worker.Run(workerCtx) }()
	defer func() { workerStop(); <-workerDone }()
	serverErr := make(chan error, 1)
	go func() { serverErr <- server.Serve() }()
	// Do not print/log the bootstrap URL. The desktop opener alone receives it.
	openCtx, openStop := context.WithTimeout(ctx, 10*time.Second)
	cmd := exec.CommandContext(openCtx, opener, url)
	cmd.Stdout = io.Discard
	cmd.Stderr = io.Discard
	cmd.WaitDelay = time.Second
	e = cmd.Run()
	openStop()
	if e != nil {
		cancel()
	}
	fmt.Println("Manual lapdog reviewer running. Session routing ID:", controller.SessionID())
	tick := time.NewTicker(250 * time.Millisecond)
	defer tick.Stop()
	sweep := time.NewTicker(time.Minute)
	defer sweep.Stop()
	desk := time.NewTicker(5 * time.Second)
	defer desk.Stop()
	failure := e
loop:
	for {
		select {
		case <-ctx.Done():
			break loop
		case failure = <-serverErr:
			cancel()
			break loop
		case failure = <-workerErr:
			cancel()
			break loop
		case failure = <-monErr:
			cancel()
			break loop
		case <-tick.C:
			if failure = controller.Tick(); failure != nil {
				cancel()
				break loop
			}
		case <-sweep.C:
			if failure = controller.Sweep(false); failure != nil {
				cancel()
				break loop
			}
		case <-desk.C:
			if failure = desktop(ctx); failure != nil {
				cancel()
				break loop
			}
		}
	}
	// Keep the guest lease renewing while cancellation is reaped/reconciled. Then
	// publish final coarse/released ledger data before closing the guest route.
	_ = server.Close()
	if e = adapter.Shutdown(); e != nil {
		failure = e
	}
	finalCtx, finalStop := context.WithTimeout(context.Background(), 4*time.Second)
	_ = worker.Publish(finalCtx)
	finalStop()
	workerStop()
	monitorStop()
	// Each SSH exchange has its own deadline; join rather than abandoning a
	// goroutine that can still access the store. Also reap dbus-monitor above.
	<-workerDone
	return failure
}

// cleanup never opens a browser, advertises a route or invokes execution. The
// singleton lock prevents recovery/GC from interfering with a live reviewer.
func cleanup() error { return cleanupStore(state, shell) }

func cleanupStore(path, trustedShell string) error {
	if _, err := os.Lstat(path); os.IsNotExist(err) {
		return nil
	}
	s, err := host.OpenStore(path, nil)
	if err != nil {
		return err
	}
	defer s.Close()
	c, err := host.NewController(s, host.Config{Shell: trustedShell})
	if errors.Is(err, syscall.EWOULDBLOCK) {
		return nil
	}
	if err != nil {
		return err
	}
	defer c.CloseUnclean()
	return c.End()
}

func monitor(ctx context.Context, c *host.Controller) (<-chan error, <-chan struct{}, <-chan struct{}) {
	errs := make(chan error, 1)
	ready := make(chan struct{})
	done := make(chan struct{})
	go func() {
		defer close(done)
		cmd := exec.CommandContext(ctx, dbusMonitor, "--system", "type='signal',interface='org.freedesktop.login1.Manager',member='PrepareForSleep',path='/org/freedesktop/login1'")
		pipe, e := cmd.StdoutPipe()
		if e != nil {
			errs <- e
			return
		}
		cmd.Stderr = io.Discard
		cmd.WaitDelay = time.Second
		if e = cmd.Start(); e != nil {
			errs <- e
			return
		}
		scan := bufio.NewScanner(pipe)
		scan.Buffer(make([]byte, 4096), 4096)
		seen := false
		for scan.Scan() {
			line := scan.Text()
			if strings.HasPrefix(line, "signal ") {
				if !seen {
					close(ready)
					seen = true
				}
				if e = c.Invalidate(); e != nil {
					break
				}
			}
		}
		_ = cmd.Process.Kill()
		_ = cmd.Wait()
		errs <- errors.New("suspend monitor stopped")
	}()
	return errs, ready, done
}
func checkPin() error {
	s, e := host.OpenStore(strings.TrimSuffix(knownHosts, "/known_hosts"), nil)
	if e != nil {
		return e
	}
	defer s.Close()
	st, e := os.Lstat(knownHosts)
	if e != nil {
		return errors.New("host key not enrolled; use --enroll with independently verified public key and SHA256 fingerprint")
	}
	sys, ok := st.Sys().(*syscall.Stat_t)
	if !ok || sys.Uid != uint32(os.Getuid()) || !st.Mode().IsRegular() || st.Mode().Perm() != 0600 || sys.Nlink != 1 {
		return errors.New("unsafe host pin")
	}
	return nil
}
func enroll(path, fingerprint string) error {
	f, e := os.Open(path)
	if e != nil {
		return e
	}
	defer f.Close()
	key, e := enrollmentKey(f, fingerprint)
	if e != nil {
		return e
	}
	// The pin directory is validated with the same private ancestor walk as the
	// host store. Existing pins are never replaced, even on an explicit retry.
	s, e := host.OpenStore(strings.TrimSuffix(knownHosts, "/known_hosts"), nil)
	if e != nil {
		return e
	}
	defer s.Close()
	f2, e := os.OpenFile(knownHosts, os.O_WRONLY|os.O_CREATE|os.O_EXCL, 0600)
	if e != nil {
		return e
	}
	_, e = fmt.Fprintf(f2, "%s ssh-ed25519 %s\n", guestAddress, key)
	if e == nil {
		e = f2.Sync()
	}
	closeErr := f2.Close()
	if e == nil {
		e = closeErr
	}
	if e != nil {
		return e
	}
	d, e := os.Open(strings.TrimSuffix(knownHosts, "/known_hosts"))
	if e != nil {
		return e
	}
	defer d.Close()
	return d.Sync()
}

func enrollmentKey(r io.Reader, fingerprint string) (string, error) {
	b, e := io.ReadAll(io.LimitReader(r, 4097))
	if e != nil || len(b) > 4096 {
		return "", errors.New("invalid public key")
	}
	fields := strings.Fields(string(b))
	if len(fields) < 2 || fields[0] != "ssh-ed25519" {
		return "", errors.New("ed25519 public key required")
	}
	raw, e := base64.StdEncoding.DecodeString(fields[1])
	if e != nil || len(raw) != 51 || !strings.HasPrefix(string(raw), "\x00\x00\x00\x0bssh-ed25519\x00\x00\x00\x20") {
		return "", errors.New("invalid ed25519 key")
	}
	sum := sha256.Sum256(raw)
	if "SHA256:"+base64.RawStdEncoding.EncodeToString(sum[:]) != fingerprint {
		return "", errors.New("independent fingerprint mismatch")
	}
	return fields[1], nil
}
