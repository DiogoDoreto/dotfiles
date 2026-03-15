# FreshRSS + Authentik SSO Integration Design

**Date:** 2026-03-15
**Status:** Approved

## Goal

Integrate FreshRSS authentication with the Authentik instance running on the same server
(`auth.local.doreto.com.br`), replacing form-based login with Authentik-only SSO.
Multi-user: each Authentik user maps to their own FreshRSS account, auto-created on first login.

## Why Not Native OIDC

FreshRSS's official OIDC support (`OIDC_ENABLED`, `OIDC_PROVIDER_METADATA_URL`, etc.) is
implemented via Apache's `mod_auth_openidc` module and is only available in the official Docker
image (Debian/x86_64). The NixOS FreshRSS module uses Caddy, not Apache, so this path is not
available. The end-user experience is identical either way.

## Approach

**Caddy `forward_auth` + Authentik Proxy Provider (Forward auth, single application)**

- Caddy validates every request (except API paths) against Authentik's forward auth endpoint
- Unauthenticated users are redirected to Authentik's login page
- After login, Authentik signals Caddy with `X-Authentik-Username`; Caddy maps this to `Remote-User`
- FreshRSS in `http_auth` mode reads `Remote-User` and auto-creates the account on first login

## Architecture

### `freshrss.nix` changes

**`services.freshrss`:**
- `authType = "http_auth"` — FreshRSS reads the `Remote-User` header set by Caddy
- Remove `passwordFile` — the NixOS module only requires it for `authType = "form"`; removing it
  for `http_auth` is safe and correct per the module's assertions

**`services.phpfpm.pools.freshrss.phpEnv`:**
- Add `TRUSTED_PROXY = "127.0.0.1"` — FreshRSS reads this via `getenv("TRUSTED_PROXY")` to decide
  which IPs are allowed to supply identity via `Remote-User` / `X-WebAuth-User` HTTP headers.
  Caddy sends requests to php-fpm over a Unix socket; the FastCGI `REMOTE_ADDR` is `127.0.0.1`.

**`services.caddy.virtualHosts."freshrss.local.doreto.com.br".extraConfig`:**

```caddyfile
# Strip any client-supplied Remote-User to prevent header injection
request_header -Remote-User

# Gate all non-API paths through Authentik
# API paths (/api/*) use FreshRSS's own auth (HTTP Basic + API password) and must bypass SSO
@notApi not path /api/*
forward_auth @notApi https://auth.local.doreto.com.br {
  uri /outpost.goauthentik.io/auth/caddy
  copy_headers X-Authentik-Username X-Authentik-Groups X-Authentik-Email
  transport http {
    tls_insecure_skip_verify
  }
}

# Map Authentik's username header to what FreshRSS expects
@notApiRequest not path /api/*
request_header @notApiRequest Remote-User {http.request.header.X-Authentik-Username}
```

Notes:
- `request_header -Remote-User` strips any client-supplied header before the auth pipeline runs,
  preventing header injection attacks (a client cannot fake their identity by sending `Remote-User`)
- `copy_headers` copies `X-Authentik-Username` from Authentik's auth response into the proxied
  request. `X-Authentik-Username` is not in FreshRSS's checked header set so it is harmless
- `request_header` maps it to `Remote-User` so FreshRSS's `httpAuthUser()` reads
  `$_SERVER['HTTP_REMOTE_USER']`
- `tls_insecure_skip_verify` is required because the local Caddy CA is not in the system trust
  store (consistent with the existing `auth.local.doreto.com.br` reverse proxy in `caddy.nix`)
- The `forward_auth` endpoint `https://auth.local.doreto.com.br/outpost.goauthentik.io/auth/caddy`
  is reachable because `auth.local.doreto.com.br` reverse-proxies all paths to Authentik, which
  handles `/outpost.goauthentik.io/` via its embedded outpost

**Comment in `freshrss.nix`:**
Document that:
- `http_auth_auto_register` (FreshRSS default: `true`) creates FreshRSS accounts automatically on
  first Authentik login — no manual user provisioning needed
- The FreshRSS username will equal the Authentik internal username (`X-Authentik-Username`)
- API paths (`/api/*`) bypass SSO and use FreshRSS's own API password auth
- Recovery: if Authentik is unavailable, temporarily set `authType = "form"` + `passwordFile` and rebuild

## Authentik Configuration (manual)

Done through the Authentik admin UI.

1. **Create Proxy Provider** (`Admin → Providers → Create → Proxy Provider`):
   - Name: `freshrss`
   - Mode: Forward auth (single application)
   - External host: `https://freshrss.local.doreto.com.br`

2. **Create Application** pointing to that provider:
   - Name: `FreshRSS`
   - Slug: `freshrss`

3. **Assign to embedded outpost** (`Admin → Outposts → authentik Embedded Outpost → Edit`):
   - Move `freshrss` from Available to Selected Applications
   - Save — the outpost will hot-reload and begin serving the new provider

## Login Flow

1. User visits `https://freshrss.local.doreto.com.br`
2. Caddy strips any `Remote-User` header from the request
3. Caddy calls `https://auth.local.doreto.com.br/outpost.goauthentik.io/auth/caddy`
4. Not authenticated → Authentik returns 302 → Caddy redirects user to Authentik login
5. User logs in via Authentik
6. Authentik redirects back; Caddy re-checks forward auth → Authentik returns 200 with
   `X-Authentik-Username: alice`
7. Caddy copies `X-Authentik-Username` to request; sets `Remote-User: alice` via `request_header`
8. FreshRSS reads `$_SERVER['HTTP_REMOTE_USER']` = `alice` (trusted because REMOTE_ADDR = 127.0.0.1)
9. If account doesn't exist, `http_auth_auto_register = true` creates it automatically
10. User is logged in

## User Provisioning

FreshRSS auto-creates accounts via `http_auth_auto_register` (default `true`). No manual steps
needed per user. The FreshRSS username will equal the Authentik username.

The auto-created account will be a **regular user, not admin**. To grant admin rights after first
login, use the FreshRSS CLI:
```
freshrss-cli user update --user <username> --role admin
```
Or: name the Authentik user to match the existing FreshRSS admin account so the auto-created
account inherits the admin role from the existing account data.

This will be documented as a comment in `freshrss.nix`.

## Migration Note

Changing from `authType = "form"` to `"http_auth"` on an existing installation immediately locks
out any existing form-based accounts. The safest path:
- Set `authType = "http_auth"` and redeploy
- Log in via Authentik — FreshRSS auto-creates the new account
- Grant admin role via CLI (see above)
- The old form-based admin account data remains in the data directory

## Files Changed

- `hosts/mini/services/freshrss.nix` — update auth config, add TRUSTED_PROXY, add Caddy extraConfig

## Out of Scope

- Any changes to `authentik.nix`
- Secrets file (not needed for Proxy Provider approach)
