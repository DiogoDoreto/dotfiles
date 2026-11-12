package guest

import (
	"context"
	"errors"
	"host-command-review/transport"
)

// ResolveRoute performs exact resolution, including retained closed tombstones.
// Returning the old identity permits retrieval, never redirects into a new session.
func (s *Store) ResolveRoute(ctx context.Context, route string) (string, error) {
	if transport.ValidID(route) {
		return route, nil
	}
	alias, err := transport.CanonicalAlias(route)
	if err != nil {
		return "", err
	}
	id := ""
	err = s.transaction(ctx, func(d *database, _ string, _ int64) error {
		for _, v := range d.Sessions {
			if v.Alias == alias {
				id = v.ID
				break
			}
		}
		return nil
	})
	if err != nil {
		return "", err
	}
	if id == "" {
		return "", errors.New("no reviewer")
	}
	return id, nil
}
