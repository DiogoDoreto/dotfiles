// Immutable per-command supervisor. Build-time paths are intentionally empty:
// an unpackaged development binary cannot execute commands.
package main

import (
	"host-command-review/execution"
	"os"
	"strconv"
)

var shell, systemdRun, systemctl, helper, runner, rootRuntime, userRuntime, expectedUID string

func main() { os.Exit(run()) }
func run() int {
	root := true
	args := os.Args[1:]
	if len(args) == 3 && args[0] == "--user" {
		root = false
		args = args[1:]
	}
	if len(args) != 2 || args[0] != "--approved-sha256" {
		return 70
	}
	uid, e := strconv.Atoi(expectedUID)
	if e != nil {
		return 70
	}
	runtime := rootRuntime
	if !root {
		runtime = userRuntime
	}
	c := execution.Config{Shell: shell, SystemdRun: systemdRun, Systemctl: systemctl, Helper: helper, Runner: runner, RuntimeDir: runtime, ExpectedUID: uid}
	if c.CheckRunner(root) != nil {
		return 70
	}
	in, out, e := execution.ControlPipes()
	if e != nil {
		return 70
	}
	defer in.Close()
	defer out.Close()
	if execution.Supervisor(c, args[1], root, in, out, nil) != nil {
		return 70
	}
	return 0
}
