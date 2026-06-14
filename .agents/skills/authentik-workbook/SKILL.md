---
name: authentik-workbook
description: >
  Operational workbook for debugging and maintaining Authentik on the mini/dogdot
  server. Use when the user reports Authentik is broken, timing out, returning 404
  or 502 errors, or when a protected service can't authenticate — covers service
  health, outpost troubleshooting, provider/application assignments, and the boot
  race with Caddy.
---

# Authentik Workbook

## Service Stack

| Unit | Role |
|---|---|
| `authentik.service` | Main server: Go proxy outpost + Python/gunicorn API |
| `authentik-worker.service` | Background worker (tasks, scheduled jobs) |
| `authentik-migrate.service` | One-shot DB migration (runs at boot) |
| `caddy.service` | Reverse proxy with `forward_auth` pointing at Authentik |
| `postgresql.service` | Database backend |

Authentik config: `hosts/mini/services/authentik.nix`

## Ports

| Port | What |
|---|---|
| `9000` | HTTP — Caddy proxies here for `auth.local.doreto.com.br` and all `forward_auth` / outpost traffic |
| `9443` | HTTPS — not used directly (Caddy handles TLS) |

## First-Response Checklist

```bash
# All at once — get the big picture
systemctl status authentik.service authentik-worker.service caddy.service postgresql.service

# Recent service logs
journalctl -u authentik.service -n 50 --no-pager
journalctl -u authentik-worker.service -n 30 --no-pager

# Is Authentik actually listening?
ss -tlnp | grep -E '9000|9443'

# Can it respond?
curl -sv http://localhost:9000/if/admin/ 2>&1 | head -20
```

Key things to look for:
- **`authentik.service` not found** — you queried the wrong name; the correct unit is `authentik.service`, not `authentik-server.service`
- **502 from Caddy for `auth.local.doreto.com.br`** — Authentik isn't up yet (likely boot race, see below)
- **404 from outpost** for a protected service — provider not assigned to the outpost (most common issue)
- **Collation version mismatch warning** in PostgreSQL logs — cosmetic/harmless, see note below

## Boot Race (Caddy starts before Authentik)

Caddy starts several seconds before Authentik finishes loading. This causes
transient 502 errors in Caddy logs right after boot for:
- `auth.local.doreto.com.br`
- Any service using `forward_auth` (FreshRSS, Readeck, Calibre, etc.)

These errors are expected and self-resolve once Authentik is up (~30–60 s after
boot). If errors persist more than 2 minutes after boot, investigate further.

To confirm Authentik finished loading:
```bash
journalctl -u authentik.service --no-pager | grep -i "listening\|started\|ready"
```

## Outpost: 404 "Not Found" for a Protected Service

**Symptom:** Accessing `https://someapp.local.doreto.com.br` redirects through
Authentik but returns a 404 page from Authentik, or `forward_auth` returns 404.

**Cause:** The application's proxy provider is not assigned to the embedded outpost.

**Diagnosis:**

```bash
# Test the forward-auth endpoint directly for the affected host
curl -sv \
  -H "X-Forwarded-Host: someapp.local.doreto.com.br" \
  -H "X-Forwarded-Proto: https" \
  "http://localhost:9000/outpost.goauthentik.io/auth/caddy" 2>&1 | grep "< HTTP"
# 404 = provider not in outpost
# 302 = working (redirects to login)
# 200 = working (already authenticated)

# Check which providers ARE in the outpost via DB
sudo -u authentik psql -d authentik -tAc \
  "SELECT outpost_id, provider_id FROM authentik_outposts_outpost_providers;"

# Check outpost ID and all provider IDs/names
sudo -u authentik psql -d authentik -tAc \
  "SELECT id, name FROM authentik_core_provider ORDER BY id;"

# Check all applications and their slugs
sudo -u authentik psql -d authentik -tAc \
  "SELECT slug, name FROM authentik_core_application;"

# Verify the proxy provider has the right external_host
sudo -u authentik psql -d authentik -tAc \
  "SELECT oauth2provider_ptr_id, external_host FROM authentik_providers_proxy_proxyprovider;"
```

**Fix:** Go to Authentik Admin UI → **Outposts** → **authentik Embedded Outpost** →
Edit → add the missing application under "Applications". Save. The outpost
reloads its config automatically within ~60 seconds.

Do **not** patch the database directly — use the admin UI to avoid cache
inconsistency.

## Database Access

Authentik uses PostgreSQL. The `authentik` user owns the database.

```bash
# Interactive psql
sudo -u authentik psql -d authentik

# One-liner query
sudo -u authentik psql -d authentik -tAc "SELECT id, name FROM authentik_core_provider;"
```

Useful tables:

| Table | Content |
|---|---|
| `authentik_core_application` | Applications (slug, name, provider_id) |
| `authentik_core_provider` | All providers (id, name) |
| `authentik_providers_proxy_proxyprovider` | Proxy providers (external_host, oauth2provider_ptr_id) |
| `authentik_outposts_outpost` | Outpost instances (pk, name) |
| `authentik_outposts_outpost_providers` | Outpost↔provider assignments |
| `authentik_core_token` | API tokens |

Note: the primary key column in `authentik_core_provider` is `id`, not `pk`.

## Collation Version Mismatch

PostgreSQL logs a warning at every connection:
```
WARNING: database "authentik" has a collation version mismatch
DETAIL: The database was created using collation version 2.40, but the operating system provides version 2.42.
HINT: Rebuild all objects in this database that use the default collation and run ALTER DATABASE authentik REFRESH COLLATION VERSION...
```

This is **cosmetic and does not affect functionality**. It appears after glibc
upgrades because the collation version embedded in the DB doesn't match the new
system version. To silence it:

```bash
sudo -u authentik psql -d authentik -c "ALTER DATABASE authentik REFRESH COLLATION VERSION;"
```

## Caddy Integration Pattern

All Caddy-protected services follow the same pattern. Required elements:

```caddyfile
someapp.local.doreto.com.br {
    # Must be set before forward_auth — Authentik uses X-Forwarded-Host to match app URL
    request_header X-Forwarded-Host {http.request.host}

    # Outpost paths go directly to Authentik (login flow, OAuth callbacks)
    @outpost path /outpost.goauthentik.io/*
    reverse_proxy @outpost http://localhost:9000

    # Gate everything else through forward auth
    @needsAuth not path /outpost.goauthentik.io/*
    forward_auth @needsAuth http://localhost:9000 {
        uri /outpost.goauthentik.io/auth/caddy
        copy_headers X-Authentik-Username>Remote-User X-Authentik-Email>Remote-Email
        trusted_proxies private_ranges
    }

    reverse_proxy 127.0.0.1:<app-port>
}
```

If `X-Forwarded-Host` is missing, Authentik uses the `Host` header (which is
the Caddy internal hostname, not the external one) and fails to match the application.

## Adding a New Protected Application (Checklist)

When adding a new service behind Authentik SSO:

1. **In Authentik Admin UI:**
   - Create a **Proxy Provider** → "Forward auth (single application)" mode
   - Set `External Host` to `https://someapp.local.doreto.com.br`
   - Create an **Application** bound to that provider
   - Go to **Outposts** → **authentik Embedded Outpost** → Edit → add the new application

2. **In Nix (Caddy config):** add the `forward_auth` block (see pattern above)

3. **Verify:** `curl -H "X-Forwarded-Host: someapp.local.doreto.com.br" -H "X-Forwarded-Proto: https" http://localhost:9000/outpost.goauthentik.io/auth/caddy` should return 302 (not 404).

## Log Locations

| Log | Path |
|---|---|
| Authentik server (JSON) | `journalctl -u authentik.service` |
| Authentik worker | `journalctl -u authentik-worker.service` |
| Caddy access (per-service) | `/var/log/caddy/access-<hostname>.log` |
| Caddy auth access | `/var/log/caddy/access-auth.local.doreto.com.br.log` |
