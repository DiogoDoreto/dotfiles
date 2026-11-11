package execution

import (
	"errors"
	"os"
	"syscall"
)

// ControlPipes reconstructs inherited stdio as pollable Linux pipes. Standard
// os.Stdin/os.Stdout are not guaranteed deadline-capable after exec/pkexec.
// Do not ignore ErrNoDeadline: a blocked privileged writer must be bounded.
func ControlPipes() (in, out *os.File, err error) {
	open := func(fd int) (*os.File, error) {
		var st syscall.Stat_t
		if e := syscall.Fstat(fd, &st); e != nil {
			return nil, e
		}
		if st.Mode&syscall.S_IFMT != syscall.S_IFIFO {
			return nil, errors.New("private pipe stdio required")
		}
		dup, e := syscall.Dup(fd)
		if e != nil {
			return nil, e
		}
		syscall.CloseOnExec(dup)
		if e = syscall.SetNonblock(dup, true); e != nil {
			syscall.Close(dup)
			return nil, e
		}
		return os.NewFile(uintptr(dup), "private-control"), nil
	}
	in, err = open(0)
	if err != nil {
		return nil, nil, err
	}
	out, err = open(1)
	if err != nil {
		in.Close()
		return nil, nil, err
	}
	return in, out, nil
}
