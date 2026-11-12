package transport

import (
	"crypto/rand"
	"errors"
	"strings"
)

// Routing codes have 40 random bits. They route only, never authenticate.
const AliasAlphabet = "23456789ABCDEFGHJKLMNPQRSTUVWXYZ"

func NewAlias() (string, error) {
	var b [8]byte
	if _, err := rand.Read(b[:]); err != nil {
		return "", err
	}
	for i := range b {
		b[i] = AliasAlphabet[int(b[i])&31]
	}
	return string(b[:4]) + "-" + string(b[4:]), nil
}
func CanonicalAlias(s string) (string, error) {
	if len(s) == 9 && s[4] == '-' {
		s = s[:4] + s[5:]
	}
	if len(s) != 8 {
		return "", errors.New("invalid routing code")
	}
	for _, c := range s {
		if c > 127 {
			return "", errors.New("invalid routing code")
		}
	}
	s = strings.ToUpper(s)
	for _, c := range s {
		if !strings.ContainsRune(AliasAlphabet, c) {
			return "", errors.New("invalid routing code")
		}
	}
	return s[:4] + "-" + s[4:], nil
}
