---
name: forgejo-workbook
description: >
  Operational workbook for debugging and maintaining Forgejo on the mini/dogdot
  server. Use when the user reports Forgejo, git.local.doreto.com.br, Forgejo
  Actions, the gitea-actions runner, git-pages Pages publishing, local action
  mirrors, repository migration/import, OAuth login, or Forgejo SSH behavior is
  broken or behaving unexpectedly.
---

# Forgejo Workbook

## Service Stack

Forgejo on mini is served by Forgejo behind Caddy, with a local actions runner
and an action mirror setup unit.

| Unit | Role |
|---|---|
| `forgejo.service` | Main Forgejo web/API/SSH service |
| `forgejo-oauth-setup.service` | One-shot Authentik OAuth source setup |
| `forgejo-action-mirror-setup.service` | One-shot creation of local `actions/*` mirrors |
| `gitea-runner-mini.service` | Forgejo Actions runner instance |
| `caddy.service` | TLS termination and reverse proxy for `git.local.doreto.com.br` |
| `caddy-cert-trust.service` | Builds local CA bundle used by services that call Caddy TLS |

Forgejo config: `hosts/mini/services/forgejo.nix`

Related config:

| File | Purpose |
|---|---|
| `hosts/mini/caddy.nix` | Reverse proxy for `git.local.doreto.com.br` and Pages hosts |
| `hosts/mini/services/git-pages.nix` | Static Pages service and preview expiration |
| `hosts/mini/_variables.nix` | Port registry (`forgejo`, `forgejoSsh`, `gitPages`, etc.) |

## Ports And Hostnames

| Endpoint | What |
|---|---|
| `https://git.local.doreto.com.br` | Canonical Forgejo URL through Caddy |
| `127.0.0.1:3000` | Forgejo HTTP listener, local only |
| `git.local.doreto.com.br:2222` | Forgejo built-in SSH server |
| `https://*.pages.local.doreto.com.br` | git-pages published sites |
| `https://*.preview.pages.local.doreto.com.br` | git-pages preview sites |

Use the canonical hostname for user-visible behavior and action resolution.
Only use localhost probes when explicitly isolating Forgejo from Caddy/DNS.

## First-Response Checklist

```bash
# Overall health
systemctl status forgejo.service caddy.service gitea-runner-mini.service

# Setup units
systemctl status forgejo-oauth-setup.service forgejo-action-mirror-setup.service

# Recent logs
journalctl -u forgejo.service -n 80 --no-pager
journalctl -u forgejo-action-mirror-setup.service -n 80 --no-pager
journalctl -u gitea-runner-mini.service -n 80 --no-pager

# Hostname path through Caddy and local CA
curl -sv --cacert /etc/ssl/certs/ca-bundle-with-local-ca.crt \
  https://git.local.doreto.com.br/api/v1/version 2>&1 | head -40

# Direct Forgejo listener, only to isolate Caddy/DNS/TLS issues
curl -sv http://127.0.0.1:3000/api/v1/version 2>&1 | head -40
```

Key things to look for:

- **`curl: (6) Could not resolve host: git.local.doreto.com.br`** - DNS or
  network readiness issue for the canonical hostname.
- **Caddy 502 for `git.local.doreto.com.br`** - Caddy is up but Forgejo is not
  accepting HTTP on `127.0.0.1:3000`.
- **TLS verification error** - local Caddy CA bundle is missing/stale or the
  service is not using `/etc/ssl/certs/ca-bundle-with-local-ca.crt`.
- **`forgejo-action-mirror-setup` HTTP 422 with "disallowed hosts"** - Forgejo
  migration/import policy is blocking the remote clone host.
- **Runner registration/token errors** - `/etc/secrets/forgejo/runner-token`
  is missing, stale, or not readable by the runner service.

## Canonical Hostname Readiness

When a unit must prove Forgejo is reachable as users and actions see it, check
the canonical hostname through Caddy:

```bash
curl --silent --show-error --output /tmp/forgejo-version.json --write-out '%{http_code}\n' \
  --cacert /etc/ssl/certs/ca-bundle-with-local-ca.crt \
  https://git.local.doreto.com.br/api/v1/version
cat /tmp/forgejo-version.json
```

Use this path for `forgejo-action-mirror-setup.service`. It verifies DNS, Caddy,
TLS trust, and Forgejo readiness together. Do not work around canonical hostname
failures by switching service automation to `127.0.0.1` unless the user asks for
an isolation test.

If this fails but `curl http://127.0.0.1:3000/api/v1/version` works, debug
Caddy, DNS, and the CA bundle rather than Forgejo itself.

## Local CA And Caddy

Services that call `https://*.local.doreto.com.br` should use:

```nix
environment.SSL_CERT_FILE = "/etc/ssl/certs/ca-bundle-with-local-ca.crt";
```

or pass curl:

```bash
--cacert /etc/ssl/certs/ca-bundle-with-local-ca.crt
```

The local bundle is produced by `caddy-cert-trust.service`. If TLS verification
fails, check:

```bash
systemctl status caddy-cert-trust.service caddy.service
ls -l /etc/ssl/certs/ca-bundle-with-local-ca.crt
journalctl -u caddy-cert-trust.service -b --no-pager
```

## Actions Runner

The runner is configured in `hosts/mini/services/forgejo.nix` as
`services.gitea-actions-runner.instances.mini`.

Runner token provisioning is manual:

```bash
# In Forgejo UI:
# https://git.local.doreto.com.br/admin/actions/runners

sudo install -d -m 700 -o root -g root /etc/secrets/forgejo
sudo install -m 600 -o root -g root /dev/stdin /etc/secrets/forgejo/runner-token
# paste token, then Ctrl-D
```

Useful checks:

```bash
systemctl status gitea-runner-mini.service
journalctl -u gitea-runner-mini.service -n 100 --no-pager
```

The runner uses `url = "https://git.local.doreto.com.br"` and must be able to
verify the local Caddy certificate.

## Local Action Mirror Setup

`forgejo-action-mirror-setup.service` creates local mirrors under:

```text
https://git.local.doreto.com.br/actions/
```

The managed mirrors are:

- `actions/cache`
- `actions/checkout`
- `actions/download-artifact`
- `actions/forgejo-release`
- `actions/git-pages`
- `actions/setup-go`
- `actions/setup-node`
- `actions/setup-python`
- `actions/upload-artifact`

Each source is:

```text
https://code.forgejo.org/actions/<repo>.git
```

It needs `/etc/secrets/forgejo/admin-api-token`, loaded through systemd
credentials as `admin-api-token`. The token must belong to a Forgejo site admin
or otherwise have enough permission to create the `actions` org, migrate
repositories into it, and replace managed mirrors whose source no longer
matches the declared `code.forgejo.org/actions` source.

Provision token:

```bash
sudo install -d -m 700 -o root -g root /etc/secrets/forgejo
sudo install -m 600 -o root -g root /dev/stdin /etc/secrets/forgejo/admin-api-token
# paste token, then Ctrl-D
```

Retry setup after fixing dependencies:

```bash
sudo systemctl reset-failed forgejo-action-mirror-setup.service
sudo systemctl restart forgejo-action-mirror-setup.service
sudo journalctl -u forgejo-action-mirror-setup.service -b --no-pager
```

Verify result:

```bash
for repo in git-pages cache checkout upload-artifact download-artifact forgejo-release setup-node setup-go setup-python; do
  curl -I --cacert /etc/ssl/certs/ca-bundle-with-local-ca.crt \
    "https://git.local.doreto.com.br/actions/$repo"
done
```

Then check the Forgejo UI for expected tags, such as `v2` for `git-pages`.

## Migration / Import Policy

Symptom:

```text
Failed to create actions/git-pages mirror: HTTP 422
{"message":"You can not import from disallowed hosts.","url":"https://git.local.doreto.com.br/api/swagger"}
```

Cause: Forgejo's migration/import policy disallows the remote clone host, e.g.
`code.forgejo.org`.

Fix in Nix by adding the narrowest policy that allows the intended source. Do
not broadly allow all hosts unless the user explicitly wants that. After
changing Forgejo settings, validate and switch mini, then rerun
`forgejo-action-mirror-setup.service`.

Useful places to inspect:

```bash
# Generated app.ini location from the service
systemctl cat forgejo.service

# Effective Forgejo settings in Nix
nix eval ./hosts/mini#nixosConfigurations.dogdot.config.services.forgejo.settings --json
```

## Actions URL Resolution

Forgejo is configured with:

```nix
services.forgejo.settings.actions.DEFAULT_ACTIONS_URL = "https://git.local.doreto.com.br";
```

This means unqualified workflow actions such as:

```yaml
uses: actions/git-pages@v2
```

resolve to local Forgejo. Keep production workflow `actions/...` references in
the managed mirror list, or add a new mirror before using the action.

## Git Pages Publishing

The Pages service is separate from Forgejo but depends on Forgejo for repository
authorization and action tokens. Relevant units:

| Unit | Role |
|---|---|
| `git-pages.service` | Serves Pages and handles publish requests |
| `git-pages-site-expire.service` | One-shot preview cleanup |
| `git-pages-site-expire.timer` | Hourly preview cleanup timer |

Checks:

```bash
systemctl status git-pages.service git-pages-site-expire.timer
journalctl -u git-pages.service -n 100 --no-pager
curl -sv --cacert /etc/ssl/certs/ca-bundle-with-local-ca.crt \
  https://test.pages.local.doreto.com.br/.git-pages/health 2>&1 | head -40
```

Published sites use:

```text
https://<user>.pages.local.doreto.com.br/<repo>/
```

Preview sites use:

```text
https://<user>.preview.pages.local.doreto.com.br/<repo>@<pr-number>/
```

## OAuth / Authentik Login

Forgejo OAuth source setup is handled by `forgejo-oauth-setup.service`.

Check:

```bash
systemctl status forgejo-oauth-setup.service
journalctl -u forgejo-oauth-setup.service -b --no-pager
```

The unit uses `/etc/secrets/forgejo/oauth` as an environment file. If login
breaks after Authentik changes, verify the client ID, client secret, redirect
URI, and discovery URL in Authentik and in this environment file.

## SSH

Forgejo uses its built-in SSH server:

```text
git.local.doreto.com.br:2222
```

Checks:

```bash
ss -tlnp | grep ':2222'
ssh -T -p 2222 git@git.local.doreto.com.br
journalctl -u forgejo.service -n 100 --no-pager | grep -i ssh
```

If HTTP works but SSH clone fails, check `START_SSH_SERVER`, `SSH_DOMAIN`,
`SSH_PORT`, firewall allowed TCP ports, and the user's SSH key in Forgejo.
