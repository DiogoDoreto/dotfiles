// Strict decoding adapted from the foundation contracts parser. Kept separate
// because the guest is not the host storage trust boundary.
package transport

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"reflect"
	"strconv"
	"strings"
	"unicode/utf8"
)

// decode is intentionally closed: exact case-sensitive keys, all non-omitempty
// fields required, no duplicates, nulls, unknown keys, trailing values, invalid
// UTF-8 or unpaired Unicode surrogates. A caller must also bound read duration;
// LimitReader bounds memory, not a peer that never closes stdin.
func DecodeStrict(r io.Reader, limit int64, dst any) error {
	b, err := io.ReadAll(io.LimitReader(r, limit+1))
	if err != nil {
		return fmt.Errorf("read JSON: %w", err)
	}
	if int64(len(b)) > limit {
		return fmt.Errorf("JSON exceeds size limit")
	}
	if !utf8.Valid(b) || !validSurrogates(b) {
		return fmt.Errorf("invalid Unicode")
	}
	d := json.NewDecoder(bytes.NewReader(b))
	d.UseNumber()
	if err := shape(d, reflect.TypeOf(dst).Elem()); err != nil {
		return err
	}
	if _, err := d.Token(); err != io.EOF {
		return fmt.Errorf("expected exactly one JSON value")
	}
	if err := json.Unmarshal(b, dst); err != nil {
		return fmt.Errorf("decode JSON: %w", err)
	}
	return nil
}

func shape(d *json.Decoder, t reflect.Type) error {
	if t.Kind() == reflect.Pointer {
		t = t.Elem()
	}
	tok, err := d.Token()
	if err != nil {
		return fmt.Errorf("JSON token: %w", err)
	}
	if tok == nil {
		return fmt.Errorf("null is not allowed")
	}
	switch t.Kind() {
	case reflect.Struct:
		if tok != json.Delim('{') {
			return fmt.Errorf("expected object")
		}
		fields := map[string]reflect.StructField{}
		for i := 0; i < t.NumField(); i++ {
			f := t.Field(i)
			fields[strings.Split(f.Tag.Get("json"), ",")[0]] = f
		}
		seen := map[string]bool{}
		for d.More() {
			k, err := d.Token()
			if err != nil {
				return err
			}
			key, ok := k.(string)
			if !ok {
				return fmt.Errorf("expected key")
			}
			f, ok := fields[key]
			if !ok || seen[key] {
				return fmt.Errorf("unknown or duplicate field %q", key)
			}
			seen[key] = true
			if err := shape(d, f.Type); err != nil {
				return fmt.Errorf("field %s: %w", key, err)
			}
		}
		if end, err := d.Token(); err != nil || end != json.Delim('}') {
			return fmt.Errorf("expected object end")
		}
		for key, f := range fields {
			if !seen[key] && !strings.Contains(f.Tag.Get("json"), ",omitempty") {
				return fmt.Errorf("missing field %q", key)
			}
		}
	case reflect.Slice:
		if t.Elem().Kind() == reflect.Uint8 { // encoding/json base64 bytes
			if _, ok := tok.(string); !ok {
				return fmt.Errorf("expected base64 string")
			}
			return nil
		}
		if tok != json.Delim('[') {
			return fmt.Errorf("expected array")
		}
		for d.More() {
			if err := shape(d, t.Elem()); err != nil {
				return err
			}
		}
		if end, err := d.Token(); err != nil || end != json.Delim(']') {
			return fmt.Errorf("expected array end")
		}
	case reflect.String:
		if _, ok := tok.(string); !ok {
			return fmt.Errorf("expected string")
		}
	case reflect.Bool:
		if _, ok := tok.(bool); !ok {
			return fmt.Errorf("expected bool")
		}
	case reflect.Int, reflect.Int64, reflect.Uint64:
		n, ok := tok.(json.Number)
		if !ok || strings.ContainsAny(string(n), ".eE") {
			return fmt.Errorf("expected integer")
		}
	default:
		return fmt.Errorf("unsupported contract type %s", t)
	}
	return nil
}

// encoding/json replaces lone UTF-16 surrogates; reject instead of silently
// changing script bytes. Syntax errors themselves are handled by the decoder.
func validSurrogates(b []byte) bool {
	inString := false
	for i := 0; i < len(b); i++ {
		if b[i] == '"' {
			inString = !inString
			continue
		}
		if !inString || b[i] != '\\' {
			continue
		}
		i++
		if i >= len(b) {
			return false
		}
		if b[i] != 'u' {
			continue
		}
		if i+4 >= len(b) {
			return false
		}
		n, err := strconv.ParseUint(string(b[i+1:i+5]), 16, 16)
		if err != nil {
			return false
		}
		i += 4
		if n >= 0xdc00 && n <= 0xdfff {
			return false
		}
		if n >= 0xd800 && n <= 0xdbff {
			if i+6 >= len(b) || b[i+1] != '\\' || b[i+2] != 'u' {
				return false
			}
			lo, err := strconv.ParseUint(string(b[i+3:i+7]), 16, 16)
			if err != nil || lo < 0xdc00 || lo > 0xdfff {
				return false
			}
			i += 6
		}
	}
	return true
}
