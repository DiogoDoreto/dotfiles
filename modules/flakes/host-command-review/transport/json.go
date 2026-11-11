package transport

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"unicode/utf8"
)

// CheckJSON rejects duplicate object keys (after JSON unescaping), invalid
// Unicode and trailing values. Shape/semantic checks are a separate boundary.
func CheckJSON(b []byte) error {
	if !utf8.Valid(b) || !validSurrogates(b) {
		return fmt.Errorf("invalid Unicode")
	}
	d := json.NewDecoder(bytes.NewReader(b))
	d.UseNumber()
	if err := jsonValue(d, 0); err != nil {
		return err
	}
	if _, err := d.Token(); err != io.EOF {
		return fmt.Errorf("trailing JSON")
	}
	return nil
}
func jsonValue(d *json.Decoder, depth int) error {
	if depth > 64 {
		return fmt.Errorf("JSON too deep")
	}
	t, err := d.Token()
	if err != nil {
		return err
	}
	switch t {
	case json.Delim('{'):
		seen := map[string]bool{}
		for d.More() {
			k, err := d.Token()
			if err != nil {
				return err
			}
			key, ok := k.(string)
			if !ok || seen[key] {
				return fmt.Errorf("duplicate key")
			}
			seen[key] = true
			if err = jsonValue(d, depth+1); err != nil {
				return err
			}
		}
		end, err := d.Token()
		if err != nil || end != json.Delim('}') {
			return fmt.Errorf("invalid object")
		}
	case json.Delim('['):
		for d.More() {
			if err = jsonValue(d, depth+1); err != nil {
				return err
			}
		}
		end, err := d.Token()
		if err != nil || end != json.Delim(']') {
			return fmt.Errorf("invalid array")
		}
	case json.Delim('}'), json.Delim(']'):
		return fmt.Errorf("unexpected delimiter")
	}
	return nil
}
