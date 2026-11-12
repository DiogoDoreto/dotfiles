package transport

import (
	"strings"
	"testing"
)

func TestAlias(t *testing.T) {
	for i := 0; i < 100; i++ {
		a, e := NewAlias()
		if e != nil {
			t.Fatal(e)
		}
		b, e := CanonicalAlias(strings.ToLower(strings.ReplaceAll(a, "-", "")))
		if e != nil || a != b {
			t.Fatal(a, b, e)
		}
	}
	for _, s := range []string{"abcd", "ABCD-EFGI", "ABCD-EFGO", "ABCD--EFGH", " ABCDEFGH", "ABCDEFGH ", "ABCD_ EFGH", "ＡBCDEFGH"} {
		if _, e := CanonicalAlias(s); e == nil {
			t.Fatal("accepted", s)
		}
	}
}
