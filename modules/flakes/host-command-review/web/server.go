package web

import (
	"context"
	"crypto/rand"
	"crypto/subtle"
	"embed"
	"encoding/hex"
	"encoding/json"
	"errors"
	"io"
	"mime"
	"net"
	"net/http"
	"reflect"
	"strings"
	"sync"
	"time"
	"unicode/utf8"

	"host-command-review/contracts"
)

//go:embed assets/*
var assets embed.FS

const LeaseDuration = 10 * time.Second
const BootstrapLifetime = 2 * time.Minute
const maxBody = 9 << 20 // bounded base64 capture plus worst-case metadata escaping

// Server owns a fresh IPv4 loopback listener. Do not wrap Handler behind a proxy
// or expose it on another listener. Browser credentials use bounded tab storage.
type Server struct {
	HTTP                          *http.Server
	listener                      net.Listener
	backend                       Backend
	origin, bootstrap, auth, csrf string
	created, exchanged, lease     time.Time
	bootstrapIssued               time.Time
	mu                            sync.Mutex
	ended                         bool
	tab                           string
}

// New binds 127.0.0.1:0 itself; callers cannot accidentally bind a public address.
// The returned URL is a secret: open locally, never log or transmit to the guest.
func New(backend Backend) (*Server, string, error) {
	if backend == nil {
		return nil, "", errors.New("backend required")
	}
	l, err := net.Listen("tcp4", "127.0.0.1:0")
	if err != nil {
		return nil, "", err
	}
	s := &Server{listener: l, backend: backend, created: time.Now(), tab: "unclaimed"}
	for _, p := range []*string{&s.bootstrap, &s.auth, &s.csrf} {
		b := make([]byte, 32)
		if _, err = rand.Read(b); err != nil {
			l.Close()
			return nil, "", err
		}
		*p = hex.EncodeToString(b)
	}
	s.origin = "http://" + l.Addr().String()
	s.HTTP = &http.Server{Handler: s, ReadHeaderTimeout: 5 * time.Second, ReadTimeout: 15 * time.Second, WriteTimeout: 15 * time.Second, IdleTimeout: 30 * time.Second, MaxHeaderBytes: 16 << 10}
	return s, s.origin + "/#" + s.bootstrap, nil
}
func (s *Server) Serve() error { return s.HTTP.Serve(s.listener) }
func (s *Server) Close() error {
	s.mu.Lock()
	s.ended = true
	s.lease = time.Time{}
	s.mu.Unlock()
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	err := s.backend.Reviewer(ctx, time.Time{})
	closeErr := s.HTTP.Close()
	_ = s.listener.Close()
	if err != nil {
		return err
	}
	return closeErr
}
func equal(a, b string) bool {
	return len(a) == 64 && len(b) == 64 && subtle.ConstantTimeCompare([]byte(a), []byte(b)) == 1
}
func fail(w http.ResponseWriter, code int) { http.Error(w, http.StatusText(code), code) }
func reply(w http.ResponseWriter, v any) {
	w.Header().Set("Content-Type", "application/json")
	_ = json.NewEncoder(w).Encode(v)
}

func (s *Server) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	h := w.Header()
	h.Set("Cache-Control", "no-store")
	h.Set("X-Content-Type-Options", "nosniff")
	h.Set("Referrer-Policy", "no-referrer")
	h.Set("X-Frame-Options", "DENY")
	h.Set("Content-Security-Policy", "default-src 'none'; script-src 'self'; style-src 'self'; connect-src 'self'; frame-ancestors 'none'; base-uri 'none'; form-action 'none'")
	h.Set("Cross-Origin-Resource-Policy", "same-origin")
	if r.Host != strings.TrimPrefix(s.origin, "http://") || r.URL.IsAbs() || r.URL.RawQuery != "" {
		fail(w, 403)
		return
	}
	if o := r.Header.Get("Origin"); len(r.Header.Values("Origin")) > 1 || (o != "" && o != s.origin) {
		fail(w, 403)
		return
	}
	if site := r.Header.Get("Sec-Fetch-Site"); site != "" && site != "same-origin" && site != "none" {
		fail(w, 403)
		return
	}
	switch r.URL.Path {
	case "/", "/app.js", "/style.css":
		if r.Method != "GET" {
			fail(w, 405)
			return
		}
		file, typ := "index.html", "text/html; charset=utf-8"
		if r.URL.Path == "/app.js" {
			file, typ = "app.js", "text/javascript; charset=utf-8"
		}
		if r.URL.Path == "/style.css" {
			file, typ = "style.css", "text/css; charset=utf-8"
		}
		b, _ := assets.ReadFile("assets/" + file)
		h.Set("Content-Type", typ)
		_, _ = w.Write(b)
		return
	}
	if r.URL.Path != "/api/bootstrap" && r.URL.Path != "/api/view" && r.URL.Path != "/api/heartbeat" && r.URL.Path != "/api/operation" && r.URL.Path != "/api/claim" {
		fail(w, 404)
		return
	}
	post := r.URL.Path != "/api/view"
	if (post && r.Method != "POST") || (!post && r.Method != "GET") {
		fail(w, 405)
		return
	}
	if post {
		if r.Header.Get("Origin") != s.origin {
			fail(w, 403)
			return
		}
		mt, params, err := mime.ParseMediaType(r.Header.Get("Content-Type"))
		if err != nil || mt != "application/json" || len(params) != 0 {
			fail(w, 415)
			return
		}
	}
	s.mu.Lock()
	defer s.mu.Unlock() // never concurrently renew/revoke a web lease
	now := time.Now()
	if s.ended || (r.URL.Path != "/api/bootstrap" && now.Sub(s.created) >= time.Duration(contracts.MaxSessionSeconds)*time.Second) {
		fail(w, 401)
		return
	}
	if r.URL.Path == "/api/bootstrap" {
		var input struct {
			Token string `json:"token"`
		}
		if strict(w, r, &input, 1024) != nil {
			fail(w, 400)
			return
		}
		issued := s.bootstrapIssued
		if issued.IsZero() {
			issued = s.created
		}
		if s.bootstrap == "" || now.Sub(issued) > BootstrapLifetime || !equal(input.Token, s.bootstrap) {
			fail(w, 401)
			return
		}
		if !s.bootstrapIssued.IsZero() {
			_, auth, csrf, err := browserTokens()
			if err != nil || s.backend.Reviewer(r.Context(), time.Time{}) != nil {
				fail(w, 409)
				return
			}
			s.auth, s.csrf = auth, csrf
			s.created = now
			s.lease = time.Time{}
			s.tab = "unclaimed"
		}
		s.bootstrap = ""
		s.bootstrapIssued = time.Time{}
		s.exchanged = now
		reply(w, struct {
			Auth        string `json:"auth"`
			CSRF        string `json:"csrf"`
			ExpiresUnix int64  `json:"expires_unix"`
		}{s.auth, s.csrf, s.created.Add(time.Duration(contracts.MaxSessionSeconds) * time.Second).Unix()})
		return
	}
	if s.exchanged.IsZero() || len(r.Header.Values("Authorization")) != 1 || !equal(strings.TrimPrefix(r.Header.Get("Authorization"), "Bearer "), s.auth) || !strings.HasPrefix(r.Header.Get("Authorization"), "Bearer ") {
		fail(w, 401)
		return
	}
	if post && (len(r.Header.Values("X-CSRF-Token")) != 1 || !equal(r.Header.Get("X-CSRF-Token"), s.csrf)) {
		fail(w, 403)
		return
	}
	if r.URL.Path == "/api/claim" {
		var input struct {
			Tab string `json:"tab"`
		}
		if strict(w, r, &input, 256) != nil || len(input.Tab) != 64 {
			fail(w, 400)
			return
		}
		if _, err := hex.DecodeString(input.Tab); err != nil {
			fail(w, 400)
			return
		}
		s.tab = input.Tab
		s.lease = time.Time{}
		if s.backend.Reviewer(r.Context(), time.Time{}) != nil {
			fail(w, 409)
			return
		}
		reply(w, struct{}{})
		return
	}
	if len(r.Header.Values("X-Review-Tab")) != 1 || !equal(r.Header.Get("X-Review-Tab"), s.tab) {
		fail(w, 409)
		return
	}
	switch r.URL.Path {
	case "/api/view":
		v, err := s.backend.View(r.Context())
		if err != nil {
			fail(w, 409)
			return
		}
		reply(w, v)
	case "/api/heartbeat":
		var input struct{}
		if strict(w, r, &input, 128) != nil {
			fail(w, 400)
			return
		}
		deadline := now.Add(LeaseDuration)
		end := s.created.Add(time.Duration(contracts.MaxSessionSeconds) * time.Second)
		if deadline.After(end) {
			deadline = end
		}
		if s.backend.Reviewer(r.Context(), deadline) != nil {
			fail(w, 409)
			return
		}
		s.lease = deadline
		reply(w, struct {
			LeaseSeconds int `json:"lease_seconds"`
		}{int(LeaseDuration / time.Second)})
	case "/api/operation":
		var op Operation
		if strict(w, r, &op, maxBody) != nil || !op.valid() {
			fail(w, 400)
			return
		}
		if (op.Action == "run" || op.Action == "run_root") && !now.Before(s.lease) {
			fail(w, 409)
			return
		}
		if s.backend.Apply(r.Context(), op) != nil {
			fail(w, 409)
			return
		}
		if op.Action == "end" || op.Action == "restart" || op.Action == "start" {
			// Tab ownership survives session replacement; authority does not.
			s.lease = time.Time{}
			_ = s.backend.Reviewer(r.Context(), time.Time{})
		}
		reply(w, struct{}{})
	}
}

// strict rejects duplicate/aliased/unknown keys, null, malformed UTF-8, trailing
// values and numeric coercions. Field spelling is exact at every nested level.
func strict(w http.ResponseWriter, r *http.Request, dst any, limit int64) error {
	b, err := io.ReadAll(http.MaxBytesReader(w, r.Body, limit))
	if err != nil || !utf8.Valid(b) || !validEscapes(b) {
		return errors.New("body")
	}
	d := json.NewDecoder(strings.NewReader(string(b)))
	d.UseNumber()
	if err = shape(d, reflect.TypeOf(dst).Elem(), 0); err != nil {
		return err
	}
	if _, err = d.Token(); err != io.EOF {
		return errors.New("trailing")
	}
	return json.Unmarshal(b, dst)
}
func shape(d *json.Decoder, t reflect.Type, depth int) error {
	if depth > 32 {
		return errors.New("depth")
	}
	for t.Kind() == reflect.Pointer {
		t = t.Elem()
	}
	tok, err := d.Token()
	if err != nil {
		return err
	}
	if tok == nil {
		return errors.New("null")
	}
	switch t.Kind() {
	case reflect.Struct:
		if tok != json.Delim('{') {
			return errors.New("object")
		}
		fields := map[string]reflect.Type{}
		for i := 0; i < t.NumField(); i++ {
			f := t.Field(i)
			fields[strings.Split(f.Tag.Get("json"), ",")[0]] = f.Type
		}
		seen := map[string]bool{}
		for d.More() {
			k, e := d.Token()
			if e != nil {
				return e
			}
			key, ok := k.(string)
			if !ok {
				return errors.New("key")
			}
			ft, ok := fields[key]
			if !ok || seen[key] {
				return errors.New("field")
			}
			seen[key] = true
			if e = shape(d, ft, depth+1); e != nil {
				return e
			}
		}
		for i := 0; i < t.NumField(); i++ {
			f := t.Field(i)
			tag := strings.Split(f.Tag.Get("json"), ",")
			if len(tag) == 1 && !seen[tag[0]] {
				return errors.New("missing field")
			}
		}
		_, err = d.Token()
		return err
	case reflect.Slice:
		if t.Elem().Kind() == reflect.Uint8 {
			if _, ok := tok.(string); !ok {
				return errors.New("base64")
			}
			return nil
		}
		if tok != json.Delim('[') {
			return errors.New("array")
		}
		for d.More() {
			if err = shape(d, t.Elem(), depth+1); err != nil {
				return err
			}
		}
		_, err = d.Token()
		return err
	default:
		if _, ok := tok.(json.Delim); ok {
			return errors.New("scalar")
		}
		return nil
	}
}
