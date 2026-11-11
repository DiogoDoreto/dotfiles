package contracts

import "testing"

func TestStateMachine(t *testing.T) {
	// Exhaustive matrix: all edges not explicitly listed must fail closed.
	edges := map[Phase]map[Event]Phase{
		Reviewing:  {Edit: Reviewing, Invalidate: Reviewing, Approve: Approved, Reject: Denied, CancelBeforeStart: Stopped},
		Approved:   {Edit: Reviewing, Invalidate: Reviewing, Consume: Starting, CancelBeforeStart: Stopped},
		Starting:   {Started: Running, AuthDenied: Reviewing, Finish: OutputHeld, LoseExecution: Ambiguous},
		Running:    {Finish: OutputHeld, LoseExecution: Ambiguous},
		OutputHeld: {Release: OutputReleased},
	}
	phases := []Phase{Reviewing, Approved, Starting, Running, OutputHeld, OutputReleased, Denied, Stopped, Ambiguous, "invalid"}
	events := []Event{Approve, Edit, Invalidate, Consume, Started, AuthDenied, Finish, Release, Reject, CancelBeforeStart, LoseExecution, "invalid"}
	for _, p := range phases {
		for _, e := range events {
			want, ok := edges[p][e]
			got, err := Next(p, e)
			if ok && (err != nil || got != want) {
				t.Fatalf("%s + %s = %s, %v", p, e, got, err)
			}
			if !ok && (err == nil || got != "") {
				t.Fatalf("unexpected edge %s + %s", p, e)
			}
		}
	}
	for _, p := range phases[:len(phases)-1] {
		if _, err := p.Coarse(); err != nil {
			t.Fatal(err)
		}
	}
	if _, err := Phase("invalid").Coarse(); err == nil {
		t.Fatal("invalid phase")
	}
	if a, _ := Reviewing.Coarse(); a != Pending {
		t.Fatal("reviewing status")
	}
	if a, _ := Approved.Coarse(); a != Pending {
		t.Fatal("approval details leaked")
	}
}
