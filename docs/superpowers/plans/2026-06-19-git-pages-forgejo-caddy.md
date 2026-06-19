# Git Pages Forgejo Caddy Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add an internal-first `git-pages` service on `mini`, integrate it with Forgejo Actions and Caddy, and guarantee a local `actions/git-pages@v2` action mirror.

**Architecture:** `git-pages` runs as a localhost-only systemd service with filesystem state in `/var/lib/git-pages`. Caddy terminates internal TLS for `*.pages.local.doreto.com.br` and `*.preview.pages.local.doreto.com.br`, then reverse proxies to the Pages listener. Forgejo provides repository authorization, points its action resolver at local Forgejo for unqualified `actions/...` references, and an idempotent setup service creates a local pull mirror of Codeberg's `git-pages/action` repository.

**Tech Stack:** NixOS modules, per-host flake under `hosts/mini`, systemd services and timers, Caddy, Forgejo API, Forgejo Actions, Borg backup patterns.

## Global Constraints

- Work on branch `feature/git-pages-forgejo-caddy`.
- Only modify the `mini` host and docs for this feature.
- Keep `git-pages` bound to localhost; expose it only through Caddy.
- Use internal namespaces `*.pages.local.doreto.com.br` and `*.preview.pages.local.doreto.com.br` first.
- Store `git-pages` state in `/var/lib/git-pages`.
- Do not set `PAGES_INSECURE`.
- Do not set `allowed-repository-url-prefixes` because it prohibits archive uploads used by `git-pages/action`.
- Repository workflows must use `actions/git-pages@v2`; do not require full external action references.
- Forgejo's `[actions] DEFAULT_ACTIONS_URL` must be `https://git.local.doreto.com.br` so `actions/git-pages@v2` resolves locally.
- Do not expose arbitrary custom domains in this implementation.
- Do not run `nixos-rebuild switch`, `nixos-rebuild boot`, `home-manager switch`, or any command that activates a generation.
- Format Nix files with `make format-nix` before validation.
- Validate with `nix flake check ./hosts/mini/` and `nix build ./hosts/mini#nixosConfigurations.dogdot.config.system.build.toplevel`.

---

## File Structure

- `hosts/mini/flake.nix`: declares the upstream `git-pages` flake input used by the service module.
- `hosts/mini/flake.lock`: generated lock entry for the new `git-pages` input.
- `hosts/mini/_variables.nix`: central port registry for the three `git-pages` localhost listeners.
- `hosts/mini/services/git-pages.nix`: new focused module for `git-pages` package selection, config generation, system user, systemd service, and expiration timer.
- `hosts/mini/configuration.nix`: imports the new service module.
- `hosts/mini/caddy.nix`: adds Pages wildcard virtual hosts.
- `hosts/mini/backup.nix`: adds `/var/lib/git-pages` to the explicit Borg include patterns.
- `hosts/mini/services/forgejo.nix`: points Forgejo's action resolver at local Forgejo and adds the local action mirror setup service alongside the existing Forgejo service and runner config.
- `docs/superpowers/specs/2026-06-19-git-pages-forgejo-caddy-design.md`: approved design reference, already written.
- `docs/superpowers/plans/2026-06-19-git-pages-forgejo-caddy.md`: this implementation plan.

---

### Task 1: Add `git-pages` Package Input And Service Module

**Files:**
- Modify: `hosts/mini/flake.nix`
- Modify: `hosts/mini/flake.lock`
- Modify: `hosts/mini/_variables.nix`
- Modify: `hosts/mini/configuration.nix`
- Create: `hosts/mini/services/git-pages.nix`

**Interfaces:**
- Consumes: `inputs` from `hosts/mini/flake.nix`, existing `caddy-cert-trust.service`, existing `/etc/ssl/certs/ca-bundle-with-local-ca.crt`.
- Produces: `vars.ports.gitPages`, `vars.ports.gitPagesCaddy`, `vars.ports.gitPagesMetrics`, `systemd.services.git-pages`, `systemd.services.git-pages-site-expire`, `systemd.timers.git-pages-site-expire`, and a generated `git-pages.toml` in the Nix store.

- [ ] **Step 1: Verify the service is not already present**

Run:

```bash
nix eval ./hosts/mini#nixosConfigurations.dogdot.config.systemd.services.git-pages.description
```

Expected: FAIL with an attribute-missing error for `git-pages`.

- [ ] **Step 2: Add the upstream `git-pages` flake input**

Edit `hosts/mini/flake.nix`. Insert this input block after the existing `llm-agents.url` entry:

```nix
    git-pages = {
      url = "git+https://codeberg.org/git-pages/git-pages";
      inputs.nixpkgs.follows = "nixpkgs";
    };
```

- [ ] **Step 3: Add localhost listener ports**

Edit `hosts/mini/_variables.nix`. Insert these ports after `forgejoSsh = 2222;`:

```nix
    gitPages = 3010;
    gitPagesCaddy = 3011;
    gitPagesMetrics = 3012;
```

- [ ] **Step 4: Import the new service module**

Edit `hosts/mini/configuration.nix`. Insert this import after `./services/forgejo.nix`:

```nix
    ./services/git-pages.nix
```

- [ ] **Step 5: Create the `git-pages` service module**

Create `hosts/mini/services/git-pages.nix` with this exact content:

```nix
args:

let
  inputs = args.inputs;
  pkgs = args.pkgs;
  vars = import ../_variables.nix;
  p = builtins.mapAttrs (_: toString) vars.ports;
  gitPagesPackage = inputs.git-pages.packages.${pkgs.system}.default;
  gitPagesConfig = pkgs.writeText "git-pages.toml" ''
    features = ["expiration"]
    log-format = "text"

    [server]
    pages = "tcp/127.0.0.1:${p.gitPages}"
    caddy = "tcp/127.0.0.1:${p.gitPagesCaddy}"
    metrics = "tcp/127.0.0.1:${p.gitPagesMetrics}"

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
  '';
in

{
  users.groups."git-pages" = { };
  users.users."git-pages" = {
    isSystemUser = true;
    group = "git-pages";
    home = "/var/lib/git-pages";
    createHome = true;
  };

  systemd.services.git-pages = {
    description = "Static site server for Forgejo Pages";
    after = [ "caddy-cert-trust.service" ];
    wants = [ "caddy-cert-trust.service" ];
    wantedBy = [ "multi-user.target" ];
    environment.SSL_CERT_FILE = "/etc/ssl/certs/ca-bundle-with-local-ca.crt";
    serviceConfig = {
      ExecStart = "${gitPagesPackage}/bin/git-pages -config ${gitPagesConfig}";
      Restart = "on-failure";
      User = "git-pages";
      Group = "git-pages";
      StateDirectory = "git-pages";
      WorkingDirectory = "/var/lib/git-pages";
      NoNewPrivileges = true;
      PrivateTmp = true;
      ProtectHome = true;
      ProtectSystem = "strict";
      ReadWritePaths = [ "/var/lib/git-pages" ];
    };
  };

  systemd.services.git-pages-site-expire = {
    description = "Expire old git-pages preview sites";
    after = [ "git-pages.service" ];
    wants = [ "git-pages.service" ];
    environment.SSL_CERT_FILE = "/etc/ssl/certs/ca-bundle-with-local-ca.crt";
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${gitPagesPackage}/bin/git-pages -config ${gitPagesConfig} -site-expire";
      User = "git-pages";
      Group = "git-pages";
      StateDirectory = "git-pages";
      WorkingDirectory = "/var/lib/git-pages";
      NoNewPrivileges = true;
      PrivateTmp = true;
      ProtectHome = true;
      ProtectSystem = "strict";
      ReadWritePaths = [ "/var/lib/git-pages" ];
    };
  };

  systemd.timers.git-pages-site-expire = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnCalendar = "hourly";
      Persistent = true;
      Unit = "git-pages-site-expire.service";
    };
  };
}
```

- [ ] **Step 6: Lock the new flake input**

Run:

```bash
nix flake lock --flake ./hosts/mini
```

Expected: `hosts/mini/flake.lock` is updated with nodes for `git-pages` and its upstream flake inputs.

- [ ] **Step 7: Verify the service evaluates**

Run:

```bash
nix eval ./hosts/mini#nixosConfigurations.dogdot.config.systemd.services.git-pages.description --raw
```

Expected output:

```text
Static site server for Forgejo Pages
```

Run:

```bash
nix eval ./hosts/mini#nixosConfigurations.dogdot.config.systemd.timers.git-pages-site-expire.timerConfig.OnCalendar --raw
```

Expected output:

```text
hourly
```

- [ ] **Step 8: Commit Task 1**

Run:

```bash
git add hosts/mini/flake.nix hosts/mini/flake.lock hosts/mini/_variables.nix hosts/mini/configuration.nix hosts/mini/services/git-pages.nix
git commit -m "feat: add git-pages service"
```

---

### Task 2: Add Caddy Pages Routes And Backup Include

**Files:**
- Modify: `hosts/mini/caddy.nix`
- Modify: `hosts/mini/backup.nix`

**Interfaces:**
- Consumes: `vars.ports.gitPages` from Task 1.
- Produces: Caddy virtual hosts for `*.pages.local.doreto.com.br` and `*.preview.pages.local.doreto.com.br`, plus Borg inclusion of `/var/lib/git-pages`.

- [ ] **Step 1: Verify the Pages Caddy host is not already present**

Run:

```bash
nix eval './hosts/mini#nixosConfigurations.dogdot.config.services.caddy.virtualHosts."*.pages.local.doreto.com.br".extraConfig' --raw
```

Expected: FAIL with an attribute-missing error for `*.pages.local.doreto.com.br`.

- [ ] **Step 2: Add internal wildcard Pages virtual hosts**

Edit `hosts/mini/caddy.nix`. Insert these virtual hosts after the existing `git.local.doreto.com.br` block:

```nix
      "*.pages.local.doreto.com.br" = {
        extraConfig = ''
          tls internal
          reverse_proxy 127.0.0.1:${p.gitPages}
        '';
      };
      "*.preview.pages.local.doreto.com.br" = {
        extraConfig = ''
          tls internal
          reverse_proxy 127.0.0.1:${p.gitPages}
        '';
      };
```

- [ ] **Step 3: Include `git-pages` state in Borg backup patterns**

Edit `hosts/mini/backup.nix`. Insert this pattern after the existing Readeck include patterns:

```nix
      "+ /var/lib/git-pages"
```

The surrounding section should become:

```nix
      "+ /var/lib/readeck/data"
      "+ /var/lib/private/readeck/data"

      "+ /var/lib/git-pages"

      "+ /var/lib/radarr/.config/Radarr/config.xml"
```

- [ ] **Step 4: Verify the route and backup pattern evaluate**

Run:

```bash
nix eval './hosts/mini#nixosConfigurations.dogdot.config.services.caddy.virtualHosts."*.pages.local.doreto.com.br".extraConfig' --raw
```

Expected output contains both lines:

```text
tls internal
reverse_proxy 127.0.0.1:3010
```

Run:

```bash
nix eval './hosts/mini#nixosConfigurations.dogdot.config.services.caddy.virtualHosts."*.preview.pages.local.doreto.com.br".extraConfig' --raw
```

Expected output contains both lines:

```text
tls internal
reverse_proxy 127.0.0.1:3010
```

Run:

```bash
nix eval ./hosts/mini#nixosConfigurations.dogdot.config.services.borgbackup.jobs.root.patterns --json
```

Expected output contains:

```json
"+ /var/lib/git-pages"
```

- [ ] **Step 5: Commit Task 2**

Run:

```bash
git add hosts/mini/caddy.nix hosts/mini/backup.nix
git commit -m "feat: route git-pages through caddy"
```

---

### Task 3: Add Local `actions/git-pages` Mirror Setup

**Files:**
- Modify: `hosts/mini/services/forgejo.nix`

**Interfaces:**
- Consumes: existing Forgejo service, `caddy.service`, `caddy-cert-trust.service`, and `/etc/secrets/forgejo/admin-api-token`.
- Produces: `services.forgejo.settings.actions.DEFAULT_ACTIONS_URL = "https://git.local.doreto.com.br"` and `systemd.services.forgejo-action-mirror-setup`, which creates `https://git.local.doreto.com.br/actions/git-pages` as a public pull mirror of `https://codeberg.org/git-pages/action.git`.

- [ ] **Step 1: Verify the mirror setup service is not already present**

Run:

```bash
nix eval ./hosts/mini#nixosConfigurations.dogdot.config.systemd.services.forgejo-action-mirror-setup.description --raw
```

Expected: FAIL with an attribute-missing error for `forgejo-action-mirror-setup`.

- [ ] **Step 2: Point the Forgejo action resolver at local Forgejo and add the idempotent mirror setup oneshot**

Edit `hosts/mini/services/forgejo.nix`. In the existing `services.forgejo.settings.actions` block, set:

```nix
        DEFAULT_ACTIONS_URL = "https://git.local.doreto.com.br";
```

Edit `hosts/mini/services/forgejo.nix`. Insert this block after the existing `systemd.services.forgejo-oauth-setup` block and before the module's final closing brace:

```nix
  systemd.services.forgejo-action-mirror-setup = {
    description = "Create local git-pages Forgejo Action mirror";
    after = [
      "forgejo.service"
      "caddy.service"
      "caddy-cert-trust.service"
    ];
    wants = [
      "caddy.service"
      "caddy-cert-trust.service"
    ];
    requires = [ "forgejo.service" ];
    wantedBy = [ "multi-user.target" ];
    path = [
      pkgs.coreutils
      pkgs.curl
    ];
    environment = {
      SSL_CERT_FILE = "/etc/ssl/certs/ca-bundle-with-local-ca.crt";
    };
    script = ''
      set -euo pipefail

      TOKEN="$(tr -d '\n' < "$CREDENTIALS_DIRECTORY/admin-api-token")"
      API="https://git.local.doreto.com.br/api/v1"
      tmpdir="$(mktemp -d)"
      trap 'rm -rf "$tmpdir"' EXIT

      org_status="$(curl --silent --show-error --output "$tmpdir/org.json" --write-out "%{http_code}" \
        --cacert "$SSL_CERT_FILE" \
        --header "Authorization: token $TOKEN" \
        --header "Accept: application/json" \
        "$API/orgs/actions")"

      case "$org_status" in
        200)
          echo "Forgejo organization actions already exists"
          ;;
        404)
          create_org_status="$(curl --silent --show-error --output "$tmpdir/create-org.json" --write-out "%{http_code}" \
            --request POST \
            --cacert "$SSL_CERT_FILE" \
            --header "Authorization: token $TOKEN" \
            --header "Accept: application/json" \
            --header "Content-Type: application/json" \
            --data '{"username":"actions","full_name":"Actions","description":"Local mirrors for Forgejo Actions","visibility":"public"}' \
            "$API/orgs")"
          if [ "$create_org_status" != "201" ]; then
            echo "Failed to create actions organization: HTTP $create_org_status" >&2
            cat "$tmpdir/create-org.json" >&2
            exit 1
          fi
          echo "Created Forgejo organization actions"
          ;;
        *)
          echo "Failed to check actions organization: HTTP $org_status" >&2
          cat "$tmpdir/org.json" >&2
          exit 1
          ;;
      esac

      repo_status="$(curl --silent --show-error --output "$tmpdir/repo.json" --write-out "%{http_code}" \
        --cacert "$SSL_CERT_FILE" \
        --header "Authorization: token $TOKEN" \
        --header "Accept: application/json" \
        "$API/repos/actions/git-pages")"

      case "$repo_status" in
        200)
          echo "Forgejo action mirror actions/git-pages already exists"
          ;;
        404)
          migrate_status="$(curl --silent --show-error --output "$tmpdir/migrate.json" --write-out "%{http_code}" \
            --request POST \
            --cacert "$SSL_CERT_FILE" \
            --header "Authorization: token $TOKEN" \
            --header "Accept: application/json" \
            --header "Content-Type: application/json" \
            --data '{"clone_addr":"https://codeberg.org/git-pages/action.git","mirror":true,"private":false,"repo_name":"git-pages","repo_owner":"actions","service":"git","wiki":false,"issues":false,"pull_requests":false,"releases":false}' \
            "$API/repos/migrate")"
          case "$migrate_status" in
            200|201)
              echo "Created Forgejo action mirror actions/git-pages"
              ;;
            409)
              echo "Forgejo action mirror actions/git-pages already exists"
              ;;
            *)
              echo "Failed to create actions/git-pages mirror: HTTP $migrate_status" >&2
              cat "$tmpdir/migrate.json" >&2
              exit 1
              ;;
          esac
          ;;
        *)
          echo "Failed to check actions/git-pages repository: HTTP $repo_status" >&2
          cat "$tmpdir/repo.json" >&2
          exit 1
          ;;
      esac
    '';
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      User = "forgejo";
      Group = "forgejo";
      LoadCredential = [ "admin-api-token:/etc/secrets/forgejo/admin-api-token" ];
    };
  };
```

- [ ] **Step 3: Verify the service evaluates with the right ordering**

Run:

```bash
nix eval ./hosts/mini#nixosConfigurations.dogdot.config.systemd.services.forgejo-action-mirror-setup.description --raw
```

Expected output:

```text
Create local git-pages Forgejo Action mirror
```

Run:

```bash
nix eval ./hosts/mini#nixosConfigurations.dogdot.config.systemd.services.forgejo-action-mirror-setup.after --json
```

Expected output contains:

```json
"forgejo.service"
```

Expected output also contains:

```json
"caddy.service"
```

Expected output also contains:

```json
"caddy-cert-trust.service"
```

Run:

```bash
nix eval ./hosts/mini#nixosConfigurations.dogdot.config.systemd.services.forgejo-action-mirror-setup.serviceConfig.LoadCredential --json
```

Expected output:

```json
["admin-api-token:/etc/secrets/forgejo/admin-api-token"]
```

- [ ] **Step 4: Commit Task 3**

Run:

```bash
git add hosts/mini/services/forgejo.nix
git commit -m "feat: mirror git-pages action locally"
```

---

### Task 4: Format, Validate, And Document Operator Checks

**Files:**
- Modify: Nix files touched by formatter, only if `make format-nix` changes them.

**Interfaces:**
- Consumes: all outputs from Tasks 1 through 3.
- Produces: formatted Nix code and evidence that the `mini` host evaluates and builds.

- [ ] **Step 1: Format Nix files**

Run:

```bash
make format-nix
```

Expected: command exits with status 0. If files change, inspect the diff before committing.

- [ ] **Step 2: Run the host flake check**

Run:

```bash
nix flake check ./hosts/mini/
```

Expected: command exits with status 0.

- [ ] **Step 3: Build the mini NixOS system closure**

Run:

```bash
nix build ./hosts/mini#nixosConfigurations.dogdot.config.system.build.toplevel
```

Expected: command exits with status 0 and produces a `result` symlink. Do not activate the build.

- [ ] **Step 4: Inspect the final diff**

Run:

```bash
git status --short
git diff --stat
git diff -- hosts/mini/flake.nix hosts/mini/_variables.nix hosts/mini/configuration.nix hosts/mini/services/git-pages.nix hosts/mini/caddy.nix hosts/mini/backup.nix hosts/mini/services/forgejo.nix
```

Expected: diff only contains the `git-pages` service, Caddy routes, Borg include, and local action mirror setup described in this plan.

- [ ] **Step 5: Commit formatting changes if any exist**

Run:

```bash
git status --short
```

If the formatter changed files after Task 3, run:

```bash
git add hosts/mini/flake.nix hosts/mini/_variables.nix hosts/mini/configuration.nix hosts/mini/services/git-pages.nix hosts/mini/caddy.nix hosts/mini/backup.nix hosts/mini/services/forgejo.nix
git commit -m "chore: format git-pages integration"
```

If there are no formatter changes, do not create a formatting commit.

- [ ] **Step 6: Record post-deploy operator checks in the final implementation summary**

Include these checks in the final response after implementation:

```text
After the user deploys mini:
1. Create /etc/secrets/forgejo/admin-api-token with an admin token that can create the actions organization and migrate repositories.
2. Confirm systemctl status git-pages.service is active.
3. Confirm systemctl status forgejo-action-mirror-setup.service succeeds.
4. Confirm https://git.local.doreto.com.br/actions/git-pages exists and includes tag v2.
5. Publish a test site with uses: actions/git-pages@v2.
6. Confirm https://<user>.pages.local.doreto.com.br/<repo>/.git-pages/health returns ok.
```

---

## Plan Self-Review Notes

- Spec coverage: Tasks 1 through 4 cover package input, localhost service, expiration timer, Caddy routing, Borg backup, local action mirror, no external action references, formatting, flake check, and mini build validation.
- Scope check: Public ACME-backed domains and arbitrary custom domains remain outside this implementation, matching the approved design.
- Type consistency: Port names are `gitPages`, `gitPagesCaddy`, and `gitPagesMetrics`; every consumer uses those exact names.
