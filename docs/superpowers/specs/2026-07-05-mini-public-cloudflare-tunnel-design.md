# Mini Public Cloudflare Tunnel Design

Date: 2026-07-05
Host: mini (NixOS)
Status: Approved design, awaiting user review

## Overview

Expose a small, explicitly reviewed subset of the `mini` host to the public internet under `*.doreto.com.br` using Cloudflare Tunnel. Cloudflare provides the public ingress and edge TLS. The `cloudflared` connector on `mini` opens only outbound connections to Cloudflare and forwards public HTTP traffic to a dedicated localhost-only Caddy listener.

Caddy remains the local allow-list boundary. The public listener serves only explicitly configured public hostnames and returns `404` for everything else. Existing internal services under `*.local.doreto.com.br` remain on the current internal Caddy routes and are not reused for public tunnel traffic.

The first implementation should validate the public Pages workflow before migrating Forgejo and Authentik to their public canonical hostnames.

## Goals

- Route public `*.doreto.com.br` traffic to `mini` through Cloudflare Tunnel, without opening router/firewall inbound ports to the host.
- Keep public exposure fine-grained and auditable in Nix/Caddy configuration.
- Allow public access to manually allow-listed Forgejo Pages sites only.
- Eventually expose Forgejo at `git.doreto.com.br` and Authentik at `auth.doreto.com.br`.
- Keep Authentik account creation/manual user control under the administrator's control.
- Require security and data-exposure review before adding each public hostname.
- Keep the existing internal `*.local.doreto.com.br` service surface private.

## Non-Goals

- Do not use Tailscale Funnel for the public `*.doreto.com.br` URLs. Tailscale Funnel is limited to the tailnet's `*.ts.net` names.
- Do not expose the existing `*.local.doreto.com.br` wildcard through Cloudflare.
- Do not add automatic public Pages wildcard routing.
- Do not add Cloudflare Access policies in the first phase; application authentication remains the responsibility of Forgejo and Authentik.
- Do not automate Cloudflare account, zone, tunnel, or DNS management in Nix for the first phase.
- Do not expose arbitrary future services without a separate review and explicit Caddy host.

## Files To Change

| File | Change |
|------|--------|
| `hosts/mini/_variables.nix` | Add a localhost-only port for the public Caddy listener, for example `publicCaddy = 8088`. |
| `hosts/mini/services/cloudflared.nix` | New NixOS module enabling `services.cloudflared` and declaring the mini tunnel. |
| `hosts/mini/configuration.nix` | Import `./services/cloudflared.nix`. |
| `hosts/mini/caddy.nix` | Keep existing internal routes and add a separate public listener with explicit public hosts plus a catch-all `404`. |
| `hosts/mini/services/git-pages.nix` | Add a public wildcard entry only if required by `git-pages` for public custom-domain serving. This must not create public wildcard Caddy exposure. |
| `hosts/mini/services/forgejo.nix` | Final phase: migrate Forgejo canonical domain, runner URLs, and OAuth discovery to public hostnames after Pages validation. |

## Architecture

Cloudflare becomes the only public ingress for `*.doreto.com.br`. It forwards wildcard traffic through a named Cloudflare Tunnel to `cloudflared` on `mini`. `cloudflared` forwards HTTP requests to a dedicated public Caddy listener bound to localhost, for example:

```text
127.0.0.1:8088
```

The existing Caddy configuration for local services remains separate. Public traffic must not be forwarded to the existing internal `*.local.doreto.com.br` wildcard listener.

Public request path:

```text
Browser
  -> Cloudflare edge TLS for *.doreto.com.br
  -> Cloudflare Tunnel
  -> cloudflared on mini
  -> http://127.0.0.1:<publicCaddy>
  -> Caddy public host allow-list
  -> local backend
```

Internal request path remains unchanged:

```text
LAN/Tailnet client
  -> dnsmasq resolves *.local.doreto.com.br to mini LAN/Tailscale IP
  -> existing Caddy internal listener
  -> local backend
```

## Cloudflare Setup

Cloudflare setup is manual/bootstrap-only in the first implementation. Use a locally managed named tunnel: Cloudflare maps public DNS to the tunnel, while the origin routing target remains in the Nix-managed `cloudflared` ingress config.

- Create a Cloudflare Tunnel for `mini`.
- Install its credentials JSON at `/etc/secrets/cloudflared/doreto-tunnel.json` with restrictive permissions.
- Add a wildcard DNS route for `*.doreto.com.br` to the tunnel, for example a proxied wildcard CNAME to `<tunnel-uuid>.cfargotunnel.com` or the equivalent `cloudflared tunnel route dns` result.
- Keep Cloudflare Access disabled for these applications in the first phase.
- Confirm Cloudflare edge TLS covers the intended public hostnames. The first phase should use one-label public hosts such as `project.doreto.com.br`; multi-label public hosts require a separate Cloudflare certificate review.

Cloudflare wildcard DNS records are multi-level by default. A public wildcard for `*.doreto.com.br` can match names under `*.local.doreto.com.br` unless a more specific DNS record or delegation prevents it. Add a specific public DNS record, delegation, or equivalent blocker for `local.doreto.com.br` so Cloudflare does not synthesize public wildcard records for the internal namespace.

## Cloudflared Module

Use the nixpkgs `services.cloudflared` module rather than a custom systemd unit. The new `hosts/mini/services/cloudflared.nix` module should enable cloudflared and declare one named tunnel using the credentials file:

```nix
services.cloudflared = {
  enable = true;
  tunnels."<tunnel-uuid>" = {
    credentialsFile = "/etc/secrets/cloudflared/doreto-tunnel.json";
    ingress = {
      "*.doreto.com.br" = "http://127.0.0.1:${p.publicCaddy}";
    };
    default = "http_status:404";
  };
};
```

The tunnel UUID is not secret but should be treated as deployment-specific configuration. The credentials JSON must never enter the Nix store or git repository.

## Public Caddy Listener

Caddy should get a dedicated public listener bound to localhost on the configured `publicCaddy` port. This listener should define only reviewed public hosts and a catch-all fallback.

Initial Pages validation can start with one manually reviewed public Pages hostname:

```caddy
project.doreto.com.br:8088 {
  bind 127.0.0.1
  reverse_proxy 127.0.0.1:3010
}

:8088 {
  bind 127.0.0.1
  respond "not found" 404
}
```

The exact hostnames are intentionally manual Caddy entries in the first phase. This makes every public Pages exposure a visible code review point. There is no public `*.pages.doreto.com.br` or general `*.doreto.com.br` Pages proxy.

After Pages validation, add explicit public service hosts:

```caddy
git.doreto.com.br:8088 {
  bind 127.0.0.1
  reverse_proxy 127.0.0.1:3000
}

auth.doreto.com.br:8088 {
  bind 127.0.0.1
  reverse_proxy http://127.0.0.1:9000
}
```

The existing local hosts such as `auth.local.doreto.com.br` and `git.local.doreto.com.br` stay on the internal Caddy listener.

## Pages Exposure Policy

Pages public exposure starts as explicit Caddy host entries. A page is public only when a reviewed hostname is added to the public Caddy listener.

For each new public Pages host, review:

- The Forgejo repository owner and repository name.
- Whether the generated site content is intended for unauthenticated public access.
- Whether the site includes private photos, personal data, secrets, private links, local-only hostnames, or drafts.
- Whether the route should proxy the whole host or a constrained path.
- Whether caching headers are appropriate for the content.

No wildcard public Pages exposure should be added in the first phase. A later iteration may replace manual Caddy entries with a structured Nix allow-list after the desired pattern is clear.

The first phase should prefer one-label public Pages hostnames under `doreto.com.br`, such as `project.doreto.com.br`. Multi-label public Pages hostnames, such as `demo.project.doreto.com.br`, require separate Cloudflare DNS and certificate review before use.

## Forgejo And Authentik Canonical Domain Migration

Canonical-domain migration is deliberately the last phase. Validate the Pages workflow through Cloudflare first, while Forgejo and Authentik continue to operate canonically on their internal hostnames.

After Pages validation succeeds, migrate Forgejo and Authentik to public canonical hostnames:

- Set Forgejo `DOMAIN`, `ROOT_URL`, and `SSH_DOMAIN` to `git.doreto.com.br`.
- Update Forgejo Actions `DEFAULT_ACTIONS_URL` and runner `url` values to `https://git.doreto.com.br`.
- Update Forgejo OAuth setup to discover Authentik at `https://auth.doreto.com.br/application/o/forgejo/.well-known/openid-configuration`.
- Update the Authentik Forgejo provider/application redirect URI to include `https://git.doreto.com.br/user/oauth2/authentik/callback`.
- Keep Forgejo registration disabled and internal sign-in disabled unless there is a separate decision to change account policy.
- Keep Authentik public sign-up disabled; accounts remain manually created/admin-managed.

Internal access to `git.local.doreto.com.br` and `auth.local.doreto.com.br` may remain as aliases if useful, but generated public links and OAuth redirects should use the canonical public hostnames after this phase. If local clients should avoid hairpinning through Cloudflare for the public names, add internal DNS overrides for `git.doreto.com.br` and `auth.doreto.com.br` in a later change.

## Security Controls

- Public exposure is opt-in at the Caddy host level.
- Public Caddy has a catch-all `404` for unknown hostnames.
- Public Pages has no wildcard route.
- Internal `*.local.doreto.com.br` routes stay separate from public tunnel traffic.
- Public DNS should block or override the `local.doreto.com.br` subtree to prevent Cloudflare wildcard synthesis for internal names.
- Forgejo registration remains disabled.
- Authentik account creation remains manual/admin-controlled.
- Cloudflare Access is not used in the first phase.
- Secrets remain outside the Nix store, especially Cloudflare tunnel credentials.
- Public additions require explicit review of security and data exposure before a Caddy host is added.

## Operations

Manual setup:

1. Create the Cloudflare Tunnel in Cloudflare.
2. Install `/etc/secrets/cloudflared/doreto-tunnel.json` on `mini`.
3. Add the Cloudflare wildcard DNS route for `*.doreto.com.br` to the named tunnel.
4. Add a specific public DNS record/delegation/blocker for `local.doreto.com.br`.
5. Rebuild `mini` with the new NixOS modules.

Useful service checks on `mini`:

```sh
systemctl status cloudflared-tunnel-<tunnel-uuid>.service
journalctl -u cloudflared-tunnel-<tunnel-uuid>.service -n 100 --no-pager
systemctl status caddy.service
```

## Testing

Build-time verification:

```sh
nix flake check ./hosts/mini/
```

Runtime verification after deployment:

```sh
curl -I https://<allowed-page>.doreto.com.br
curl -I https://unknown-test.doreto.com.br
curl -I https://auth.local.doreto.com.br
```

Expected results:

- Allowed public Pages host returns the deployed site or an expected Pages response.
- Unknown public host returns `404` from public Caddy.
- Public `*.local.doreto.com.br` does not resolve publicly or does not route to internal services.
- Internal `https://auth.local.doreto.com.br` still works from LAN/Tailscale.

After the final canonical-domain migration phase:

```sh
curl -I https://git.doreto.com.br
curl -I https://auth.doreto.com.br
```

Expected results:

- Forgejo loads at `https://git.doreto.com.br` and emits public clone/login links using that hostname.
- Authentik loads at `https://auth.doreto.com.br`.
- Forgejo OAuth login redirects to and from Authentik successfully.

## Rollback

Rollback options are intentionally simple:

- Remove or disable the Cloudflare wildcard public hostname route.
- Stop `cloudflared-tunnel-<tunnel-uuid>.service` on `mini`.
- Remove specific public Caddy hosts while keeping the catch-all `404`.
- For the final canonical-domain migration, revert Forgejo/Auth settings to internal hostnames if OAuth or runner workflows fail.

Because Cloudflare Tunnel is outbound-only, disabling the tunnel route or service removes public ingress without changing router/firewall rules.
