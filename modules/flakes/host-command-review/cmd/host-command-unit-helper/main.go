package main

import (
	"host-command-review/execution"
	"os"
	"strconv"
)

var shell, systemdRun, systemctl, helper, runner, rootRuntime, userRuntime, expectedUID string

func main() { os.Exit(run()) }
func run() int {
	if len(os.Args) != 4 || os.Args[1] != "--unit-payload" || os.Args[2] != "--approved-sha256" {
		return 70
	}
	uid, e := strconv.Atoi(expectedUID)
	if e != nil {
		return 70
	}
	runtime := userRuntime
	if os.Geteuid() == 0 {
		runtime = rootRuntime
	}
	c := execution.Config{Shell: shell, SystemdRun: systemdRun, Systemctl: systemctl, Helper: helper, Runner: runner, RuntimeDir: runtime, ExpectedUID: uid}
	in, out, e := execution.ControlPipes()
	if e != nil {
		return 70
	}
	defer in.Close()
	defer out.Close()
	if execution.UnitHelper(c, os.Args[3], in, out) != nil {
		return 70
	}
	return 0
}
