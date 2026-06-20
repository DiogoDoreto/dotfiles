# Git Pages Forgejo Caddy Integration Design

Date: 2026-06-19
Host: mini (NixOS)
Status: Approved design, awaiting implementation plan

## Overview

Add a `git-pages` static site service to the `mini` host and integrate it with the existing Forgejo and Caddy stack. The service starts as an internal-only Pages deployment under `*.pages.local.doreto.com.br` and `*.preview.pages.local.doreto.com.br`, while keeping the routing and service boundaries compatible with later public exposure.

The preferred publishing path is Forgejo Actions using a local mirror of `git-pages/action@v2` at `actions/git-pages@v2`. Forgejo's action resolver must point `DEFAULT_ACTIONS_URL` at local Forgejo so unqualified `actions/...` references resolve to `git.local.doreto.com.br`, not an external action host. This supports generated static sites, repository-matching wildcard authorization through Forgejo's automatic action token, and optional pull request preview deployments without relying on external action references at workflow runtime.

Pull request preview authorization also requires Forgejo's `/api/v1/actions/run` endpoint. The mini host temporarily patches the packaged Forgejo 15.0.2 derivation with Forgejo PR 12727 until nixpkgs ships a Forgejo release that includes that endpoint.

## Goals

- Serve user and project static sites from repositories hosted on `git.local.doreto.com.br`.
- Keep the `git-pages` backend bound to localhost and expose it only through Caddy.
- Use Forgejo authorization for matching wildcard deployments rather than shared upload passwords.
- Provide a local Forgejo mirror of Codeberg's `git-pages/action` under the `actions/git-pages` repository.
- Use filesystem storage under `/var/lib/git-pages` for the single-host deployment.
- Start with internal TLS and dnsmasq-backed wildcard DNS.
- Preserve a straightforward path to public ACME-backed Pages domains later.

## Non-Goals

- Do not replace Forgejo or the existing Forgejo Actions runner.
- Do not expose arbitrary custom domains in the first implementation.
- Do not use S3/object storage for the initial single-host setup.
- Do not enable unauthenticated `PAGES_INSECURE` mode.
- Do not require external Forgejo Action references in repository workflows.
- Do not put generated site files into the existing `/var/lib/www` Caddy file server path.

## Files Changed

| File | Change |
|------|--------|
| `hosts/mini/flake.nix` | Add upstream `git-pages` as a flake input and pass it through existing `inputs`. |
| `hosts/mini/_variables.nix` | Add localhost ports for `git-pages` pages, Caddy ask, and metrics listeners. |
| `hosts/mini/services/git-pages.nix` | New NixOS module for package, config file, state directory, and systemd service. |
| `hosts/mini/services/forgejo.nix` | Patch Forgejo with PR 12727 for `git-pages` preview authorization, configure Forgejo Actions to resolve unqualified actions locally, and add idempotent setup for the local `actions/git-pages` pull mirror. |
| `hosts/mini/caddy.nix` | Add wildcard Pages virtual hosts that reverse proxy to `git-pages`. |
| `hosts/mini/configuration.nix` | Import `./services/git-pages.nix`. |
| `hosts/mini/backup.nix` | Include `/var/lib/git-pages` in the Borg include patterns. |

## Service Architecture

`git-pages` runs as a dedicated systemd service on `mini`. It listens only on localhost:

- Pages HTTP traffic: `127.0.0.1:3010`
- Caddy on-demand TLS decision endpoint: `127.0.0.1:3011`
- Prometheus metrics: `127.0.0.1:3012`

Caddy terminates TLS and reverse proxies normal site traffic to the Pages HTTP listener. The Caddy decision endpoint is not used for normal internal traffic, but keeping it enabled makes later public custom-domain support simpler.

```text
Browser
  -> Caddy (*.pages.local.doreto.com.br, tls internal)
  -> git-pages pages listener (127.0.0.1:3010)
  -> /var/lib/git-pages filesystem storage
```

Publishing from Forgejo Actions uses the same Caddy route:

```text
Forgejo Actions runner
  -> HTTPS Pages URL
  -> Caddy
  -> git-pages update API
  -> Forgejo authorization check against git.local.doreto.com.br
  -> /var/lib/git-pages filesystem storage
```

## Package Source

Use upstream `git-pages` as a flake input rather than relying on an uncertain nixpkgs package version:

```nix
git-pages = {
  url = "git+https://codeberg.org/git-pages/git-pages";
  inputs.nixpkgs.follows = "nixpkgs";
};
```

The new service module can reference `inputs.git-pages.packages.${pkgs.system}.default`. The host already passes `inputs` through `specialArgs`, so no new special-argument plumbing is needed.

For Forgejo itself, keep the override local to `hosts/mini/services/forgejo.nix`: start from `pkgs.forgejo`, append PR 12727 with `pkgs.fetchpatch`, and set `services.forgejo.package` to that patched derivation. Keep the Forgejo package version at 15.0.2, but name the local derivation with the PR number so targeted evals can distinguish it from the unpatched nixpkgs package. The fetchpatch filter excludes only PR 12727's upstream integration test, which depends on newer test helpers not present in Forgejo 15.0.2; the runtime endpoint and swagger hunks remain included. Remove this override when the packaged Forgejo version includes `/api/v1/actions/run`.

## Git Pages Config

The generated config should be written into the Nix store and passed with `git-pages -config <path>`.

```toml
features = ["expiration", "preview"]
log-format = "text"

[server]
pages = "tcp/127.0.0.1:3010"
caddy = "tcp/127.0.0.1:3011"
metrics = "tcp/127.0.0.1:3012"

[[wildcard]]
domain = "pages.local.doreto.com.br"
preview-domain = "preview.pages.local.doreto.com.br"
clone-url = "https://git.local.doreto.com.br/<user>/<project>.git"
index-repo = "pages"
index-repo-branch = "main"
authorization = "forgejo"
max-preview-lifetime = 7

[storage]
type = "fs"

[storage.fs]
root = "/var/lib/git-pages"

[limits]
max-site-size = "128MB"
max-manifest-size = "1MB"
allow-expiration = true
```

Do not set `allowed-repository-url-prefixes` in the initial config. Upstream documents that setting it prohibits archive uploads, which conflicts with the Forgejo Action publishing path.

Because expiration is enabled for preview cleanup, add a systemd timer that runs `git-pages -config <path> -site-expire` periodically.

## Systemd Service

Create a system user and state directory for `git-pages`. The service should run with a restricted systemd sandbox where practical, but it must retain write access to `/var/lib/git-pages` and network access to `git.local.doreto.com.br` for Forgejo authorization and repository cloning.

The service should set:

```nix
environment.SSL_CERT_FILE = "/etc/ssl/certs/ca-bundle-with-local-ca.crt";
```

It should start after and want `caddy-cert-trust.service` so local TLS verification works for `git.local.doreto.com.br`.

## Local Action Mirror

Repository workflows must use the local Forgejo action reference:

```yaml
uses: actions/git-pages@v2
```

Set Forgejo's `[actions] DEFAULT_ACTIONS_URL` to `https://git.local.doreto.com.br` so the unqualified `actions/git-pages@v2` reference resolves against this local Forgejo instance.

The implementation should ensure this exists as a pull mirror of Codeberg's action repository:

```text
local:  https://git.local.doreto.com.br/actions/git-pages
remote: https://codeberg.org/git-pages/action.git
```

Add an idempotent `forgejo-action-mirror-setup` oneshot, colocated with the Forgejo service configuration. It should run after `forgejo.service`, `caddy.service`, and `caddy-cert-trust.service`, set `SSL_CERT_FILE=/etc/ssl/certs/ca-bundle-with-local-ca.crt`, and use the Forgejo API over `https://git.local.doreto.com.br`.

The setup service should:

- Check whether the `actions` organization exists; create it if missing.
- Check whether `actions/git-pages` exists; create it as a public pull mirror if missing.
- Use `/repos/migrate` with `mirror = true`, `service = "git"`, `clone_addr = "https://codeberg.org/git-pages/action.git"`, `repo_owner = "actions"`, and `repo_name = "git-pages"`.
- Treat an existing repository as success and leave mirror scheduling to Forgejo.

The setup service needs a Forgejo API token with enough privileges to create an organization and migrate a repository into it. Store it outside the Nix store, for example:

```text
/etc/secrets/forgejo/admin-api-token
```

If the token is missing, the setup service may fail without preventing Forgejo or `git-pages` from starting, but Pages publishing workflows are not considered ready until `https://git.local.doreto.com.br/actions/git-pages` exists and has tag `v2`.

## Caddy Routing

Add internal wildcard routes:

```caddy
*.pages.local.doreto.com.br {
  tls internal
  reverse_proxy 127.0.0.1:3010
}

*.preview.pages.local.doreto.com.br {
  tls internal
  reverse_proxy 127.0.0.1:3010
}
```

The existing dnsmasq configuration already routes `*.local.doreto.com.br` to `mini`, so no internal DNS change is required for the initial namespace.

Do not protect Pages hosts with Authentik forward auth. Authorization belongs on write/update requests handled by `git-pages`; public read access is expected for published static sites.

## Forgejo Actions Publishing

Repository workflows should publish generated site output with the local `actions/git-pages@v2` mirror. Do not use full external action references such as `uses: https://codeberg.org/git-pages/action@v2`.

All `uses:` entries in production workflows should resolve locally. This design guarantees the `actions/git-pages` mirror; if `actions/checkout` is not already mirrored locally, mirror it separately or replace the checkout step with the team's existing local checkout mechanism.

Example matching wildcard deployment:

```yaml
name: Publish Pages
on:
  push:
    branches: [main]

jobs:
  publish:
    runs-on: nix
    steps:
      - uses: actions/checkout@v5
      - run: |
          mkdir -p _site
          cp -r public/. _site/
      - uses: actions/git-pages@v2
        with:
          site: https://${{ forge.event.repository.owner.username }}.pages.local.doreto.com.br/${{ forge.event.repository.name }}/
          token: ${{ forge.token }}
          source: _site/
```

Example pull request preview:

```yaml
- if: ${{ forge.event_name == 'pull_request' }}
  uses: actions/git-pages@v2
  with:
    site: https://${{ forge.event.repository.owner.username }}.preview.pages.local.doreto.com.br/${{ forge.event.repository.name }}@${{ forge.event.number }}/
    token: ${{ forge.token }}
    source: _site/
    expires: 7
```

## Public Exposure Path

The initial implementation is internal-only. Public exposure later should reuse the same `git-pages` service and add public Caddy hosts plus DNS.

Recommended public namespaces:

- `*.pages.doreto.com.br`
- `*.preview.pages.doreto.com.br`

The public phase requires:

- Public wildcard DNS or DNS-01 automation for the Pages namespaces.
- Caddy ACME certificates instead of `tls internal` for public hosts.
- A second `[[wildcard]]` entry if the public domain differs from the internal domain.
- Firewall/router exposure for ports 80 and 443 if not already public.

Custom arbitrary domains should be a later design. If added, Caddy can use `git-pages`'s `server.caddy` listener as an on-demand TLS `ask` endpoint so certificates are only issued for domains with deployed content.

## Operations

- Back up `/var/lib/git-pages` with the rest of service state.
- Check health with `https://<site>/.git-pages/health` after a site is deployed.
- Inspect service logs with `journalctl -u git-pages.service`.
- If expiration is enabled, add and monitor a `git-pages-site-expire.timer`.
- Keep `PAGES_INSECURE` unset in production.

## Testing

Before deployment:

- Run `nix flake check ./hosts/mini/`.
- Build the mini NixOS configuration if feasible.
- Evaluate `config.services.forgejo.package.version` and the package path/marker to confirm mini is using the PR 12727-patched Forgejo derivation.

After deployment:

- Confirm `git-pages.service` is active.
- Confirm `https://git.local.doreto.com.br/actions/git-pages` exists and includes tag `v2`.
- Confirm Caddy serves `https://test.pages.local.doreto.com.br/.git-pages/health` after publishing a test site.
- Publish a minimal Forgejo Actions workflow from a test repository.
- Verify a normal project site path such as `https://dog.pages.local.doreto.com.br/example/`.
- Verify a preview path if pull request previews are enabled.

## Open Decisions

- Whether public exposure should use `pages.doreto.com.br` or another public domain.
