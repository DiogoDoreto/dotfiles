// Fixed SSH endpoint: no remote argv carries proposals, IDs or paths.
package main

import (
	"context"
	"flag"
	"fmt"
	"host-command-review/guest"
	"host-command-review/transport"
	"os"
	"path/filepath"
	"time"
)

func main() {
	home, err := os.UserHomeDir()
	if err != nil {
		os.Exit(1)
	}
	dir := flag.String("state-dir", filepath.Join(home, ".local/state/host-review-guest"), "private guest state directory (local setup only)")
	flag.Parse()
	if flag.NArg() != 0 {
		os.Exit(2)
	}
	// Hard process deadline also bounds peers that never close stdin, blocked
	// stdout, flock contention, fsync and malicious filesystem behavior.
	timer := time.AfterFunc(transport.ExchangeTimeoutSeconds*time.Second, func() { fmt.Fprintln(os.Stderr, "exchange timeout"); os.Exit(1) })
	defer timer.Stop()
	ctx, cancel := context.WithTimeout(context.Background(), transport.ExchangeTimeoutSeconds*time.Second)
	defer cancel()
	if err = guest.ExchangeIO(ctx, &guest.Store{Dir: *dir}, os.Stdin, os.Stdout); err != nil {
		fmt.Fprintln(os.Stderr, "exchange failed")
		os.Exit(1)
	}
}
