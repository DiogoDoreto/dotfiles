# Forgejo Service Implementation Plan

> **For agentic workers:** REQUIRED: Use superpowers:subagent-driven-development (if subagents available) or superpowers:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a Forgejo git forge service to the mini NixOS host, proxied by Caddy, with Authentik as the sole authentication provider.

**Architecture:** Forgejo runs as a systemd service listening on localhost only; Caddy reverse-proxies it under `git.local.doreto.com.br`. A second systemd oneshot (`forgejo-oauth-setup`) runs after Forgejo starts and registers Authentik as an OAuth2 source in Forgejo's database using the Forgejo admin CLI.

**Tech Stack:** NixOS module `services.forgejo` (forgejo-lts), SQLite, Caddy reverse proxy, Authentik OIDC

**Spec:** `docs/superpowers/specs/2026-03-15-forgejo-service-design.md`

---

## Chunk 1: All changes

### File Structure

| Action | File | Responsibility |
|--------|------|---------------|
| Modify | `hosts/mini/_variables.nix` | Add `forgejo` port (3000) |
| Create | `hosts/mini/services/forgejo.nix` | Forgejo service config + OAuth2 setup oneshot |
| Modify | `hosts/mini/caddy.nix` | Add `git.local.doreto.com.br` virtual host |
| Modify | `hosts/mini/configuration.nix` | Import `./services/forgejo.nix` |

---

### Task 1: Add forgejo port to _variables.nix

**Files:**
- Modify: `hosts/mini/_variables.nix`

The port 3000 is Forgejo's default and is not currently used by any other service.

- [ ] **Step 1: Add the port entry**

In `hosts/mini/_variables.nix`, add `forgejo = 3000;` to the `ports` attrset. The list is alphabetically ordered; `forgejo` belongs after `calibre` and before `home-assistant`:

```nix
calibre = 18083;
forgejo = 3000;   # <-- add here
home-assistant = 8123;
```

- [ ] **Step 2: Verify Nix evaluates**

From the repo root:

```bash
nix eval --file hosts/mini/_variables.nix '.ports.forgejo'
```

Expected output: `3000`

- [ ] **Step 3: Commit**

```bash
git add hosts/mini/_variables.nix
git commit -m "mini: add forgejo port to _variables.nix"
```

---

### Task 2: Create hosts/mini/services/forgejo.nix

**Files:**
- Create: `hosts/mini/services/forgejo.nix`

This file contains both the `services.forgejo` NixOS config and the `forgejo-oauth-setup` systemd oneshot. These belong together because the oneshot is tightly coupled to the Forgejo service (it uses `config.services.forgejo.package` and targets Forgejo's state directory).

- [ ] **Step 1: Create the file**

Create `hosts/mini/services/forgejo.nix` with the following content:

```nix
{ config, pkgs, ... }:

let
  vars = import ../_variables.nix;
in

{
  services.forgejo = {
    enable = true;
    # package defaults to pkgs.forgejo-lts per the NixOS module
    settings = {
      server = {
        HTTP_ADDR = "127.0.0.1"; # localhost only — Caddy proxies
        HTTP_PORT = vars.ports.forgejo;
        DOMAIN = "git.local.doreto.com.br";
        ROOT_URL = "https://git.local.doreto.com.br/";
        DISABLE_SSH = true;
      };
      service = {
        DISABLE_REGISTRATION = true; # Authentik-only login
      };
      session = {
        COOKIE_SECURE = true;
      };
    };
  };

  systemd.services.forgejo-oauth-setup = {
    description = "Configure Authentik OAuth2 source in Forgejo";
    after = [ "forgejo.service" ];
    requires = [ "forgejo.service" ];
    wantedBy = [ "multi-user.target" ];
    path = [ config.services.forgejo.package pkgs.gnugrep ];
    environment = {
      USER = "forgejo";
      HOME = "/var/lib/forgejo";
      FORGEJO_WORK_DIR = "/var/lib/forgejo";
      FORGEJO_CUSTOM = "/var/lib/forgejo/custom";
    };
    script = ''
      if forgejo admin auth list | grep -qw "authentik"; then
        echo "Auth source 'authentik' already exists, skipping"
        exit 0
      fi

      forgejo admin auth add-oauth \
        --name authentik \
        --provider openidConnect \
        --auto-discover-url https://auth.local.doreto.com.br/application/o/forgejo/.well-known/openid-configuration \
        --key "$CLIENT_ID" \
        --secret "$CLIENT_SECRET" \
        --scopes "openid email profile"
    '';
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      User = "forgejo";
      Group = "forgejo";
      WorkingDirectory = "/var/lib/forgejo";
      EnvironmentFile = "/etc/secrets/forgejo/oauth";
    };
  };
}
```

- [ ] **Step 2: Verify the file parses**

```bash
nix-instantiate --parse hosts/mini/services/forgejo.nix
```

Expected: prints the parsed AST without errors.

- [ ] **Step 3: Commit**

```bash
git add hosts/mini/services/forgejo.nix
git commit -m "mini: add forgejo service with authentik oauth2 setup"
```

---

### Task 3: Add Caddy virtual host

**Files:**
- Modify: `hosts/mini/caddy.nix`

- [ ] **Step 1: Add the virtual host entry**

In `hosts/mini/caddy.nix`, add the following entry to `services.caddy.virtualHosts`, keeping it with the other service entries (alphabetically, between `freshrss` and `ha` if those exist, or anywhere among the service blocks):

```nix
"git.local.doreto.com.br" = {
  extraConfig = ''
    reverse_proxy localhost:${p.forgejo}
  '';
};
```

The `p` variable is already defined at the top of `caddy.nix` as:
```nix
p = builtins.mapAttrs (_: toString) vars.ports;
```
So `p.forgejo` will resolve to `"3000"`.

- [ ] **Step 2: Verify Nix parses**

```bash
nix-instantiate --parse hosts/mini/caddy.nix
```

Expected: no errors.

- [ ] **Step 3: Commit**

```bash
git add hosts/mini/caddy.nix
git commit -m "mini: add git.local.doreto.com.br caddy vhost for forgejo"
```

---

### Task 4: Import forgejo.nix in configuration.nix

**Files:**
- Modify: `hosts/mini/configuration.nix`

- [ ] **Step 1: Add the import**

In `hosts/mini/configuration.nix`, add `./services/forgejo.nix` to the `imports` list. Keep it alphabetically sorted with the other service imports:

```nix
imports = [
  ./hardware.nix
  ./networking.nix
  ./caddy.nix
  ./backup.nix
  ./services/authentik.nix
  ./services/forgejo.nix        # <-- add this line
  ./services/freshrss.nix
  ...
];
```

- [ ] **Step 2: Dry-run the full NixOS build**

This confirms the entire configuration evaluates without errors before touching the live system.

```bash
nixos-rebuild dry-run --flake .#mini
```

Expected: completes without evaluation errors. It will show what would be built/activated.

- [ ] **Step 3: Commit**

```bash
git add hosts/mini/configuration.nix
git commit -m "mini: import forgejo service module"
```

---

### Task 5: Deploy and verify

This task is performed on the mini host (or via a deployment mechanism). Complete the manual Authentik steps first if not already done — see spec section "Manual Authentik Steps".

- [ ] **Step 1: (Pre-deploy) Create secrets file on mini host**

On the mini host, run as root:

```bash
mkdir -p /etc/secrets/forgejo
printf 'CLIENT_ID=<your-client-id>\nCLIENT_SECRET=<your-client-secret>\n' \
  > /etc/secrets/forgejo/oauth
chmod 600 /etc/secrets/forgejo/oauth
chown forgejo:forgejo /etc/secrets/forgejo/oauth
```

(You cannot do this before Forgejo has been deployed once, since the `forgejo` user won't exist yet. If deploying for the first time: deploy without the secrets file, then create the file and run `systemctl restart forgejo-oauth-setup`.)

- [ ] **Step 2: Deploy**

```bash
nixos-rebuild switch --flake .#mini
```

Expected: switch completes, no activation errors.

- [ ] **Step 3: Verify Forgejo is running**

```bash
systemctl status forgejo
```

Expected: `active (running)`

- [ ] **Step 4: Verify OAuth2 setup oneshot ran**

```bash
systemctl status forgejo-oauth-setup
```

Expected: `active (exited)` with no errors in the log. If it shows `failed` it means the secrets file was missing or had wrong credentials — check with `journalctl -u forgejo-oauth-setup`.

- [ ] **Step 5: Verify Forgejo is reachable**

```bash
curl -sI http://localhost:3000
```

Expected: HTTP 200 or 302 response from Forgejo.

- [ ] **Step 6: Verify Caddy is proxying**

Open `https://git.local.doreto.com.br` in a browser (or from a machine that trusts the local CA cert).

Expected: Forgejo login page loads, showing "Sign in with authentik" and no local registration option.

- [ ] **Step 7: Verify OAuth2 source in Forgejo admin**

Log in to Forgejo at `https://git.local.doreto.com.br` using your Authentik account. Then navigate to `https://git.local.doreto.com.br/-/admin/auths` to confirm the `authentik` OAuth2 source is listed.
