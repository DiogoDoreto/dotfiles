# FreshRSS Authentik SSO Implementation Plan

> **For agentic workers:** REQUIRED: Use superpowers:subagent-driven-development (if subagents available) or superpowers:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace FreshRSS form-based login with Authentik SSO using Caddy `forward_auth`, with auto-provisioning of FreshRSS user accounts on first login.

**Architecture:** Caddy's `forward_auth` directive gates all non-API requests through Authentik's embedded outpost. Authentik returns `X-Authentik-Username` on success; Caddy maps it to `Remote-User`; FreshRSS in `http_auth` mode reads it and auto-creates accounts. `TRUSTED_PROXY=127.0.0.1` is set in the php-fpm pool environment so FreshRSS trusts the header from Caddy (FastCGI REMOTE_ADDR is 127.0.0.1 over a Unix socket).

**Tech Stack:** NixOS `services.freshrss` module (Caddy webserver), Caddy `forward_auth`, Authentik Proxy Provider (Forward auth mode)

**Spec:** `docs/superpowers/specs/2026-03-15-freshrss-authentik-oidc-design.md`

---

## Chunk 1: Authentik manual setup + freshrss.nix changes

### File Structure

| Action | File | Responsibility |
|--------|------|----------------|
| Modify | `hosts/mini/services/freshrss.nix` | Switch to http_auth, add TRUSTED_PROXY, add Caddy forward_auth config |

---

### Task 1: Configure Authentik (manual prerequisite)

These steps must be completed before deploying the NixOS changes. Authentik is at
`https://auth.local.doreto.com.br`.

- [ ] **Step 1: Create Proxy Provider in Authentik**

  Navigate to `Admin → Providers → Create → Proxy Provider` and configure:
  - Name: `freshrss`
  - Authorization flow: `default-provider-authorization-implicit-consent`
  - Mode: **Forward auth (single application)**
  - External host: `https://freshrss.local.doreto.com.br`
  - Leave all other fields at defaults and save.

- [ ] **Step 2: Create Application in Authentik**

  Navigate to `Admin → Applications → Create` and configure:
  - Name: `FreshRSS`
  - Slug: `freshrss`
  - Provider: `freshrss` (the one just created)
  - Save.

- [ ] **Step 3: Assign provider to embedded outpost**

  Navigate to `Admin → Outposts → authentik Embedded Outpost → Edit`:
  - In the **Applications** section, move `FreshRSS` from Available to Selected
  - Save — the outpost hot-reloads within seconds

- [x] **Step 4: Verify outpost is serving the provider**

  ```bash
  curl -v http://[::1]:9001/outpost.goauthentik.io/ping
  ```
  Expected: HTTP 204. The outpost listens on `[::1]:9001` (authentik-nix worker default).
  Note: visiting the URL in a browser returns 404 because Authentik needs forwarded host headers
  to match a provider — test with curl only.

---

### Task 2: Update freshrss.nix

**Files:**
- Modify: `hosts/mini/services/freshrss.nix`

- [ ] **Step 1: Verify current config evaluates**

  ```bash
  cd /home/dog/projects/dotfiles/hosts/mini
  nix eval .#nixosConfigurations.dogdot.config.services.freshrss.authType
  ```
  Expected output: `"form"` (the current default)

- [ ] **Step 2: Rewrite freshrss.nix**

  Replace the entire contents of `hosts/mini/services/freshrss.nix` with:

  ```nix
  { config, pkgs, ... }:

  {
    services.freshrss = {
      enable = true;
      extensions = with pkgs.freshrss-extensions; [
        youtube
        reading-time
      ];
      webserver = "caddy";
      virtualHost = "freshrss.local.doreto.com.br";
      baseUrl = "https://freshrss.local.doreto.com.br";
      # Auth is handled by Caddy forward_auth → Authentik (see extraConfig below).
      # passwordFile is not needed for http_auth mode.
      authType = "http_auth";
    };

    # Allow FreshRSS to trust the Remote-User header from Caddy.
    # FreshRSS only trusts this header if REMOTE_ADDR is in the TRUSTED_PROXY list.
    # Caddy → php-fpm via Unix socket sets REMOTE_ADDR=127.0.0.1.
    services.phpfpm.pools.freshrss.phpEnv = {
      TRUSTED_PROXY = "127.0.0.1";
    };

    services.caddy.virtualHosts."freshrss.local.doreto.com.br".extraConfig = ''
      # Strip any client-supplied Remote-User to prevent identity injection
      request_header -Remote-User

      route {
        # Proxy outpost paths back to Authentik (needed for sign-out callbacks)
        reverse_proxy /outpost.goauthentik.io/* http://[::1]:9001

        # Gate all non-API requests through Authentik's forward auth.
        # /api/* paths (GReader, Fever) use FreshRSS's own API password auth and bypass SSO.
        # The outpost HTTP listener is at [::1]:9001 (authentik-nix worker default).
        @notApi not path /api/*
        forward_auth @notApi http://[::1]:9001 {
          uri /outpost.goauthentik.io/auth/caddy
          copy_headers X-Authentik-Username X-Authentik-Groups X-Authentik-Entitlements X-Authentik-Email X-Authentik-Name X-Authentik-Uid X-Authentik-Jwt X-Authentik-Meta-Jwks X-Authentik-Meta-Outpost X-Authentik-Meta-Provider X-Authentik-Meta-App X-Authentik-Meta-Version
          trusted_proxies private_ranges
        }

        # Map Authentik's username header to what FreshRSS expects ($SERVER['HTTP_REMOTE_USER'])
        @notApiRequest not path /api/*
        request_header @notApiRequest Remote-User {http.request.header.X-Authentik-Username}
      }

      # User provisioning notes:
      # - FreshRSS auto-creates accounts on first login (http_auth_auto_register = true by default)
      # - The FreshRSS username will equal the Authentik username (X-Authentik-Username)
      # - New accounts are regular users. Grant admin via:
      #     sudo -u freshrss DATA_PATH=/var/lib/freshrss php \
      #       /run/current-system/sw/share/freshrss/cli/update-user.php --user <name> --admin 1
      # - Recovery if Authentik is unavailable: temporarily set authType = "form" and
      #   passwordFile = "/var/lib/freshrss-pass.txt", then nixos-rebuild switch
    '';
  }
  ```

- [ ] **Step 3: Verify config evaluates without errors**

  ```bash
  nix eval .#nixosConfigurations.dogdot.config.services.freshrss.authType
  ```
  Expected: `"http_auth"`

  ```bash
  nix eval .#nixosConfigurations.dogdot.config.services.phpfpm.pools.freshrss.phpEnv
  ```
  Expected: `{ TRUSTED_PROXY = "127.0.0.1"; ... }` (may include other env vars from the module)

- [ ] **Step 4: Build the system without switching**

  ```bash
  nixos-rebuild build --flake hosts/mini#dogdot
  ```
  Expected: build succeeds with no errors. Fix any Nix evaluation errors before proceeding.

- [ ] **Step 5: Commit**

  ```bash
  git add hosts/mini/services/freshrss.nix
  git commit -m "freshrss: integrate Authentik SSO via Caddy forward_auth"
  ```

---

### Task 3: Deploy and verify

- [ ] **Step 1: Deploy**

  ```bash
  nixos-rebuild switch --flake hosts/mini#dogdot
  ```
  Expected: switch completes; `freshrss-config.service` restarts cleanly.

  If `freshrss-config.service` fails, check:
  ```bash
  systemctl status freshrss-config.service
  journalctl -u freshrss-config.service -n 50
  ```

- [ ] **Step 2: Verify Caddy config loaded**

  ```bash
  systemctl status caddy.service
  journalctl -u caddy.service -n 20
  ```
  Expected: no errors about invalid Caddyfile syntax.

- [ ] **Step 3: Test SSO login flow**

  Open `https://freshrss.local.doreto.com.br` in a browser (logged out of Authentik).
  Expected: redirected to `https://auth.local.doreto.com.br/` for login.

  Log in with your Authentik credentials.
  Expected: redirected back to FreshRSS and logged in automatically.
  If this is the first login, FreshRSS shows the account setup screen.

- [ ] **Step 4: Test API path bypass**

  ```bash
  curl -v https://freshrss.local.doreto.com.br/api/greader.php \
    --cacert /var/lib/caddy/.local/share/caddy/pki/authorities/local/root.crt
  ```
  Expected: HTTP 401 (FreshRSS API auth required) — NOT a redirect to Authentik.
  A redirect to Authentik means the `/api/*` matcher isn't working.

- [ ] **Step 5: Grant admin role to your account**

  Replace `<username>` with your Authentik username. The CLI scripts have a nix store shebang
  so no `php` in PATH is needed — get the package path from the live service:
  ```bash
  pkg=$(systemctl show freshrss-config --property=WorkingDirectory --value)
  sudo -u freshrss DATA_PATH=/var/lib/freshrss "$pkg/cli/update-user.php" --user <username> --admin 1
  ```

  Verify admin panel is accessible in FreshRSS UI under `Administration`.
