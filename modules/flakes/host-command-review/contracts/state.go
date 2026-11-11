package contracts

import "fmt"

// Phase belongs exclusively to the host ledger, never to guest authority.
type Phase string

const (
	Reviewing      Phase = "reviewing"
	Approved       Phase = "approved"
	Starting       Phase = "starting" // nonce consumed durably; includes root prompt
	Running        Phase = "running"
	OutputHeld     Phase = "output_held"
	OutputReleased Phase = "output_released"
	Denied         Phase = "denied"
	Stopped        Phase = "stopped" // cancelled BEFORE nonce consumption
	Ambiguous      Phase = "ambiguous"
)

type Event string

const (
	Approve           Event = "approve"
	Edit              Event = "edit"
	Invalidate        Event = "invalidate"
	Consume           Event = "consume"
	Started           Event = "started"
	AuthDenied        Event = "auth_denied"
	Finish            Event = "finish"
	Release           Event = "release"
	Reject            Event = "reject"
	CancelBeforeStart Event = "cancel_before_start"
	LoseExecution     Event = "lose_execution"
)

// Next is only a transition SHAPE check. It does not check authorization,
// liveness, nonce, digest, revision, epoch, process ownership or persistence.
// Running cancellation is a supervisor operation; Finish records its outcome.
func Next(p Phase, e Event) (Phase, error) {
	switch p {
	case Reviewing:
		switch e {
		case Edit, Invalidate:
			return Reviewing, nil
		case Approve:
			return Approved, nil
		case Reject:
			return Denied, nil
		case CancelBeforeStart:
			return Stopped, nil
		}
	case Approved:
		switch e {
		case Edit, Invalidate:
			return Reviewing, nil
		case Consume:
			return Starting, nil
		case CancelBeforeStart:
			return Stopped, nil
		}
	case Starting:
		switch e {
		case Started:
			return Running, nil
		case AuthDenied:
			return Reviewing, nil // consumed nonce stays spent
		case Finish:
			return OutputHeld, nil // known pre-exec failure
		case LoseExecution:
			return Ambiguous, nil
		}
	case Running:
		switch e {
		case Finish:
			return OutputHeld, nil
		case LoseExecution:
			return Ambiguous, nil
		}
	case OutputHeld:
		if e == Release {
			return OutputReleased, nil
		}
	}
	return "", fmt.Errorf("invalid transition %s + %s", p, e)
}

func (p Phase) Coarse() (CoarseStatus, error) {
	switch p {
	case Reviewing, Approved:
		return Pending, nil
	case Starting, Running:
		return Active, nil
	case OutputHeld:
		return Held, nil
	case OutputReleased:
		return Released, nil
	case Denied:
		return Rejected, nil
	case Stopped:
		return Cancelled, nil
	case Ambiguous:
		return Uncertain, nil
	}
	return "", fmt.Errorf("unknown phase")
}
