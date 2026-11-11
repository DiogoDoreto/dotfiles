package guest

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"host-command-review/transport"
	"io"
)

// ExchangeIO handles exactly one EOF-delimited SSH invocation. Its caller must
// impose an independent overall deadline (the executable enforces 10 seconds).
func ExchangeIO(ctx context.Context, s *Store, in io.Reader, out io.Writer) error {
	q, err := transport.ParseRequest(in)
	if err != nil {
		return fmt.Errorf("invalid_request")
	}
	response, err := s.Exchange(ctx, q)
	if err != nil {
		return fmt.Errorf("unavailable")
	}
	if err = response.Validate(); err != nil {
		return fmt.Errorf("unavailable")
	}
	b, err := json.Marshal(response)
	if err != nil {
		return err
	}
	if len(b)+1 > transport.MaxResponseBytes {
		return fmt.Errorf("response exceeds bound")
	}
	_, err = io.Copy(out, bytes.NewReader(append(b, '\n')))
	return err
}
