# Mini Public Cloudflare Tunnel Phase 1 Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Expose only `https://apps.doreto.com.br/app-agenda-escolar/` through Cloudflare Tunnel to the existing internal Forgejo Pages site `https://diogo.pages.local.doreto.com.br/app-agenda-escolar/`.

**Architecture:** Cloudflare routes public `*.doreto.com.br` DNS to a locally managed tunnel with UUID `ed93f0fa-f178-4219-8b08-070240906058`. `cloudflared` on `mini` forwards tunnel traffic to a localhost-only public Caddy listener on `127.0.0.1:8088`. Caddy allow-lists only `apps.doreto.com.br/app-agenda-escolar/` and returns `404` for all other public hosts and paths.

**Tech Stack:** NixOS, nixpkgs `services.cloudflared`, Caddy, Cloudflare Tunnel, Forgejo Pages `git-pages`, `nix flake check`.

## Global Constraints

- Public tunnel UUID: `ed93f0fa-f178-4219-8b08-070240906058`.
- Cloudflare tunnel credentials path: `/etc/secrets/cloudflared/doreto-tunnel.json`.
- Public Caddy listener port: `8088`.
- First public Pages URL: `https://apps.doreto.com.br/app-agenda-escolar/`.
- Internal Pages origin URL: `https://diogo.pages.local.doreto.com.br/app-agenda-escolar/`.
- Do not expose any public Pages wildcard route.
- Do not expose `*.local.doreto.com.br` through Cloudflare.
- Do not enable Cloudflare Access in this phase.
- Do not migrate Forgejo or Authentik canonical domains in this phase.
- Format Nix changes with `make format-nix` before validation.
- Validate with `nix flake check ./hosts/mini/` after Nix changes.
- Building `mini` locally is allowed; running `nixos-rebuild switch`, `nixos-rebuild boot`, or `home-manager switch` is forbidden for agents.
- Do not commit implementation changes unless the user explicitly authorizes implementation commits in the execution session.

---

## File Structure

- `hosts/mini/_variables.nix`: adds `ports.publicCaddy = 8088` for the localhost-only public Caddy listener.
- `hosts/mini/services/cloudflared.nix`: new focused NixOS module for the Cloudflare Tunnel connector. It owns only `services.cloudflared` settings.
- `hosts/mini/configuration.nix`: imports `./services/cloudflared.nix` with the other mini services.
- `hosts/mini/caddy.nix`: keeps existing internal routes and adds public Caddy site blocks for `apps.doreto.com.br:8088` and `:8088`.

## Task Boundaries

- Task 1 creates the tunnel connector and public listener port.
- Task 2 adds the Caddy allow-list for the first Pages path.
- Task 3 formats and evaluates the Nix configuration.
- Task 4 gives the manual Cloudflare/deployment verification checklist. Agents must not run activation commands.

---

### Task 1: Add Cloudflare Tunnel Connector Module

**Files:**
- Modify: `hosts/mini/_variables.nix`
- Create: `hosts/mini/services/cloudflared.nix`
- Modify: `hosts/mini/configuration.nix`

**Interfaces:**
- Consumes: `vars.ports.publicCaddy` as the public Caddy listener port.
- Produces: `services.cloudflared.tunnels."ed93f0fa-f178-4219-8b08-070240906058"` forwarding `*.doreto.com.br` to `http://127.0.0.1:8088`.

- [ ] **Step 1: Verify current cloudflared state**

Run:

```sh
nix eval --raw './hosts/mini#nixosConfigurations.dogdot.config.services.cloudflared.enable'
```

Expected output before this task:

```text
false
```

- [ ] **Step 2: Add the public Caddy port**

Modify `hosts/mini/_variables.nix` so the standard port block becomes:

```nix
    # standard / well-known
    dns = 53;
    http = 80;
    https = 443;
    publicCaddy = 8088; # localhost-only Cloudflare Tunnel ingress listener
    mdns = 5353;
```

- [ ] **Step 3: Create the Cloudflare Tunnel service module**

Create `hosts/mini/services/cloudflared.nix` with exactly:

```nix
{ ... }:

let
  vars = import ../_variables.nix;
  p = builtins.mapAttrs (_: toString) vars.ports;
in

{
  services.cloudflared = {
    enable = true;
    tunnels."ed93f0fa-f178-4219-8b08-070240906058" = {
      credentialsFile = "/etc/secrets/cloudflared/doreto-tunnel.json";
      ingress = {
        "*.doreto.com.br" = "http://127.0.0.1:${p.publicCaddy}";
      };
      default = "http_status:404";
    };
  };
}
```

- [ ] **Step 4: Import the module**

Modify `hosts/mini/configuration.nix` imports so this section becomes:

```nix
    ./services/authentik.nix
    ./services/forgejo.nix
    ./services/git-pages.nix
    ./services/cloudflared.nix
    ./services/freshrss.nix
```

- [ ] **Step 5: Verify the connector evaluates**

Run:

```sh
nix eval --raw './hosts/mini#nixosConfigurations.dogdot.config.services.cloudflared.enable'
```

Expected output:

```text
true
```

Run:

```sh
nix eval --raw './hosts/mini#nixosConfigurations.dogdot.config.services.cloudflared.tunnels."ed93f0fa-f178-4219-8b08-070240906058".ingress."*.doreto.com.br"'
```

Expected output:

```text
http://127.0.0.1:8088
```

- [ ] **Step 6: Commit if explicitly authorized**

If the user has explicitly authorized implementation commits, run:

```sh
git add -- hosts/mini/_variables.nix hosts/mini/services/cloudflared.nix hosts/mini/configuration.nix
git commit -m "mini: add Cloudflare tunnel connector"
```

If the user has not explicitly authorized implementation commits, skip this step and leave the files modified for later review.

---

### Task 2: Add Public Caddy Pages Allow-List

**Files:**
- Modify: `hosts/mini/caddy.nix`

**Interfaces:**
- Consumes: `p.publicCaddy = "8088"` from `hosts/mini/_variables.nix`.
- Consumes: `p.gitPages = "3010"` for the internal `git-pages` listener.
- Produces: public Caddy site `apps.doreto.com.br:8088` that proxies only `/app-agenda-escolar/` to the internal Pages backend with `Host: diogo.pages.local.doreto.com.br`.
- Produces: public Caddy catch-all `:8088` that returns `404`.

- [ ] **Step 1: Verify public Caddy host is absent before editing**

Run:

```sh
nix eval --raw './hosts/mini#nixosConfigurations.dogdot.config.services.caddy.virtualHosts."apps.doreto.com.br:8088".extraConfig'
```

Expected result before this task: the command fails because `apps.doreto.com.br:8088` does not exist yet.

- [ ] **Step 2: Add the explicit public Pages route and catch-all**

In `hosts/mini/caddy.nix`, insert these two virtual hosts inside `services.caddy.virtualHosts`, immediately after the existing `"*.pages-preview.local.doreto.com.br"` block and before `"audiobook.local.doreto.com.br"`:

```nix
      "apps.doreto.com.br:${p.publicCaddy}" = {
        extraConfig = ''
          bind 127.0.0.1

          redir /app-agenda-escolar /app-agenda-escolar/ 308

          @appAgendaEscolar path /app-agenda-escolar/*
          reverse_proxy @appAgendaEscolar 127.0.0.1:${p.gitPages} {
            header_up Host diogo.pages.local.doreto.com.br
          }

          respond "not found" 404
        '';
      };
      ":${p.publicCaddy}" = {
        extraConfig = ''
          bind 127.0.0.1
          respond "not found" 404
        '';
      };
```

- [ ] **Step 3: Verify the public Pages route evaluates**

Run:

```sh
nix eval --raw './hosts/mini#nixosConfigurations.dogdot.config.services.caddy.virtualHosts."apps.doreto.com.br:8088".extraConfig'
```

Expected output contains all of these lines:

```text
bind 127.0.0.1
redir /app-agenda-escolar /app-agenda-escolar/ 308
@appAgendaEscolar path /app-agenda-escolar/*
header_up Host diogo.pages.local.doreto.com.br
respond "not found" 404
```

Run:

```sh
nix eval --raw './hosts/mini#nixosConfigurations.dogdot.config.services.caddy.virtualHosts.":8088".extraConfig'
```

Expected output:

```text
bind 127.0.0.1
respond "not found" 404
```

- [ ] **Step 4: Commit if explicitly authorized**

If the user has explicitly authorized implementation commits, run:

```sh
git add -- hosts/mini/caddy.nix
git commit -m "mini: allow-list public Pages route"
```

If the user has not explicitly authorized implementation commits, skip this step and leave the file modified for later review.

---

### Task 3: Format And Validate Mini Nix Configuration

**Files:**
- Modify: formatted Nix files from Tasks 1 and 2 if `make format-nix` changes them.

**Interfaces:**
- Consumes: all Nix changes from Tasks 1 and 2.
- Produces: evaluated and buildable `mini` system configuration.

- [ ] **Step 1: Format Nix files**

Run:

```sh
make format-nix
```

Expected result: command exits `0`. If it changes files, keep those formatting changes.

- [ ] **Step 2: Run the mini flake check**

Run:

```sh
nix flake check ./hosts/mini/
```

Expected result: command exits `0`.

- [ ] **Step 3: Build the mini NixOS system toplevel**

Run:

```sh
nix build ./hosts/mini#nixosConfigurations.dogdot.config.system.build.toplevel
```

Expected result: command exits `0` and creates or updates the local `result` symlink.

- [ ] **Step 4: Inspect final diff**

Run:

```sh
git diff -- hosts/mini/_variables.nix hosts/mini/services/cloudflared.nix hosts/mini/configuration.nix hosts/mini/caddy.nix
```

Expected result: diff contains only the public Caddy port, `cloudflared.nix`, the module import, and the public Caddy allow-list/catch-all.

- [ ] **Step 5: Commit if explicitly authorized**

If the user has explicitly authorized implementation commits and Tasks 1 and 2 were not already committed, run:

```sh
git add -- hosts/mini/_variables.nix hosts/mini/services/cloudflared.nix hosts/mini/configuration.nix hosts/mini/caddy.nix
git commit -m "mini: expose allow-listed Pages through Cloudflare tunnel"
```

If earlier tasks were already committed and `make format-nix` changed files, run:

```sh
git add -- hosts/mini/_variables.nix hosts/mini/services/cloudflared.nix hosts/mini/configuration.nix hosts/mini/caddy.nix
git commit -m "mini: format Cloudflare tunnel config"
```

If the user has not explicitly authorized implementation commits, skip this step and report the uncommitted diff.

---

### Task 4: Manual Cloudflare Setup And Runtime Verification

**Files:**
- No repository files.

**Interfaces:**
- Consumes: built NixOS configuration from Task 3.
- Consumes: Cloudflare tunnel UUID `ed93f0fa-f178-4219-8b08-070240906058`.
- Produces: running public route `https://apps.doreto.com.br/app-agenda-escolar/` after the user deploys the NixOS generation on `mini`.

- [ ] **Step 1: Install tunnel credentials on mini**

Run on `mini` as the user/admin who has the tunnel credentials file:

```sh
sudo install -d -m 700 -o root -g root /etc/secrets/cloudflared
sudo install -m 600 -o root -g root "$HOME/.cloudflared/ed93f0fa-f178-4219-8b08-070240906058.json" /etc/secrets/cloudflared/doreto-tunnel.json
```

Expected result: `/etc/secrets/cloudflared/doreto-tunnel.json` exists, is owned by `root:root`, and has mode `0600`.

- [ ] **Step 2: Route wildcard DNS to the named tunnel**

Run from a machine that has `cloudflared` authenticated to the Cloudflare account:

```sh
cloudflared tunnel route dns ed93f0fa-f178-4219-8b08-070240906058 "*.doreto.com.br"
```

Expected result: Cloudflare creates or confirms a proxied wildcard DNS route for `*.doreto.com.br` pointing at `ed93f0fa-f178-4219-8b08-070240906058.cfargotunnel.com`.

- [ ] **Step 3: Add the `local.doreto.com.br` public DNS blocker**

In the Cloudflare DNS dashboard for `doreto.com.br`, create this exact DNS record:

```text
Type: TXT
Name: local
Content: internal-only namespace; do not route public wildcard
TTL: Auto
```

Expected result: public wildcard DNS does not synthesize records below `local.doreto.com.br` on Cloudflare standard nameservers.

- [ ] **Step 4: User deploys the NixOS generation**

The user, not an agent, runs the NixOS activation on `mini` using their normal deployment flow.

Expected result: `caddy.service` and `cloudflared-tunnel-ed93f0fa-f178-4219-8b08-070240906058.service` start successfully.

- [ ] **Step 5: Check services on mini**

Run on `mini` after deployment:

```sh
systemctl status cloudflared-tunnel-ed93f0fa-f178-4219-8b08-070240906058.service --no-pager
systemctl status caddy.service --no-pager
journalctl -u cloudflared-tunnel-ed93f0fa-f178-4219-8b08-070240906058.service -n 100 --no-pager
```

Expected result: `systemctl status` shows `Active: active (running)` for both `cloudflared-tunnel-ed93f0fa-f178-4219-8b08-070240906058.service` and `caddy.service`. The cloudflared journal contains at least one `Registered tunnel connection` line and contains no `Cannot determine default origin certificate`, `credentials`, or `failed` errors.

- [ ] **Step 6: Verify public Pages behavior**

Run from outside the LAN or from a network path that uses public DNS:

```sh
curl -I https://apps.doreto.com.br/app-agenda-escolar
```

Expected output includes:

```text
HTTP/2 308
location: /app-agenda-escolar/
```

Run:

```sh
curl -I https://apps.doreto.com.br/app-agenda-escolar/
```

Expected output starts with:

```text
HTTP/2 200
```

Run:

```sh
curl -I https://apps.doreto.com.br/
```

Expected output starts with:

```text
HTTP/2 404
```

Run:

```sh
curl -I https://unknown-test.doreto.com.br/
```

Expected output starts with:

```text
HTTP/2 404
```

- [ ] **Step 7: Verify internal namespace is not public**

Run from a public DNS resolver path:

```sh
dig +short auth.local.doreto.com.br @1.1.1.1
```

Expected output: no `A`, `AAAA`, or `CNAME` records are printed.

Run from LAN or Tailscale where internal DNS is active:

```sh
curl -Ik https://auth.local.doreto.com.br
```

Expected output contains an HTTP status line and does not contain `server: cloudflare` or `cf-ray` headers.

- [ ] **Step 8: Stop before Forgejo/Auth public migration**

Do not edit `hosts/mini/services/forgejo.nix` for `git.doreto.com.br` in this phase.

Do not edit Authentik provider/application redirect URLs in this phase.

Expected result: Forgejo and Authentik remain canonical on `git.local.doreto.com.br` and `auth.local.doreto.com.br` while the Pages validation route is tested.
