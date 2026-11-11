// Native MCP stdio server. No Pi configuration or host deployment is performed.
package main

import (
	"context"
	"flag"
	"fmt"
	"host-command-review/guest"
	"host-command-review/guest/mcpstdio"
	"os"
	"os/signal"
	"path/filepath"
	"syscall"
)

func main() {
	home, err := os.UserHomeDir()
	if err != nil {
		os.Exit(1)
	}
	dir := flag.String("state-dir", filepath.Join(home, ".local/state/host-review-guest"), "private guest state directory")
	flag.Parse()
	if flag.NArg() != 0 {
		os.Exit(2)
	}
	ctx, cancel := signal.NotifyContext(context.Background(), syscall.SIGINT, syscall.SIGTERM)
	defer cancel()
	if err = mcpstdio.RunStdio(ctx, &guest.Store{Dir: *dir}, os.Stdin, os.Stdout); err != nil {
		fmt.Fprintln(os.Stderr, "MCP stopped")
		os.Exit(1)
	}
}
