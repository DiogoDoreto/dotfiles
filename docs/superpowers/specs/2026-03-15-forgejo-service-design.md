# Forgejo Service Design

Date: 2026-03-15
Host: mini (NixOS)

## Overview

Add a Forgejo git forge service to the mini host, proxied by Caddy, using SQLite as the database. Authentication is delegated entirely to the existing Authentik instance via OAuth2/OIDC. Local registration is disabled.

## Files Changed

| File | Change |
|------|--------|
| `hosts/mini/services/forgejo.nix` | New file — Forgejo service + OAuth2 setup oneshot |
| `hosts/mini/_variables.nix` | Add `forgejo` port entry |
| `hosts/mini/caddy.nix` | Add `git.local.doreto.com.br` virtual host |
| `hosts/mini/configuration.nix` | Import `./services/forgejo.nix` |

## Forgejo NixOS Service Config

`vars` is imported from `../_variables.nix` (relative to `hosts/mini/services/forgejo.nix`).

```nix
services.forgejo = {
  enable = true;
  # package defaults to pkgs.forgejo-lts per the NixOS module
  settings = {
    server = {
      HTTP_ADDR = "127.0.0.1";       # localhost only — Caddy proxies
      HTTP_PORT = vars.ports.forgejo; # from _variables.nix
      DOMAIN = "git.local.doreto.com.br";
      ROOT_URL = "https://git.local.doreto.com.br/";
      DISABLE_SSH = true;
    };
    service = {
      DISABLE_REGISTRATION = true;   # Authentik-only login
    };
    session = {
      COOKIE_SECURE = true;
    };
  };
};
```

`stateDir` defaults to `/var/lib/forgejo` — intentionally left at default.
Database defaults to `sqlite3` with path `/var/lib/forgejo/data/forgejo.db` — no extra DB service needed.

## OAuth2 Setup Oneshot

A `forgejo-oauth-setup` systemd oneshot runs after `forgejo.service` is up and configures the Authentik OAuth2 source in Forgejo's database. It runs once; subsequent deploys skip it if the source already exists.

**Full NixOS expression:**

The full `forgejo.nix` module structure (showing required preamble):

```nix
{ config, pkgs, ... }:

let
  vars = import ../_variables.nix;
in

{
  services.forgejo = { ... };  # see above

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

> **Operational note:** If the secrets file is missing at deploy time, `forgejo-oauth-setup` will fail. Forgejo itself will still start. Once the secrets file is created, run `systemctl restart forgejo-oauth-setup` to configure the OAuth2 source without redeploying.

**Secrets file format** (`/etc/secrets/forgejo/oauth`, permissions `600`, owned by `forgejo`):

```
CLIENT_ID=<from-authentik>
CLIENT_SECRET=<from-authentik>
```

`EnvironmentFile` loads these as environment variables directly — no shell sourcing needed. `$CLIENT_ID` and `$CLIENT_SECRET` are available in the script.

## Caddy Virtual Host

Add to `caddy.nix`:

```nix
"git.local.doreto.com.br" = {
  extraConfig = ''
    reverse_proxy localhost:${p.forgejo}
  '';
};
```

The existing `*.local.doreto.com.br` wildcard TLS block covers this automatically.

## Manual Authentik Steps (one-time, before first deploy)

1. In the Authentik admin UI, create an **OAuth2/OpenID Connect provider**:
   - Name: `forgejo`
   - Redirect URI: `https://git.local.doreto.com.br/user/oauth2/authentik/callback`
   - Note the generated **Client ID** and **Client Secret**

2. Create an **Application** in Authentik:
   - Name: `Forgejo`
   - Slug: `forgejo` (this determines the OIDC discovery URL used in the oneshot)
   - Provider: the one created above

3. On the mini host, create the secrets file before deploying:
   ```sh
   mkdir -p /etc/secrets/forgejo
   printf 'CLIENT_ID=<client-id>\nCLIENT_SECRET=<client-secret>\n' > /etc/secrets/forgejo/oauth
   chmod 600 /etc/secrets/forgejo/oauth
   chown forgejo:forgejo /etc/secrets/forgejo/oauth
   ```

4. Deploy the NixOS configuration. `forgejo-oauth-setup` will run once on first boot after Forgejo starts.

## Data Flow

```
Browser → Caddy (git.local.doreto.com.br, TLS) → Forgejo (127.0.0.1:PORT)
                                                        ↓ login redirect
                                              Authentik (auth.local.doreto.com.br)
                                                        ↓ OAuth2 callback
                                                    Forgejo session created
```

## Constraints

- SSH git access is out of scope for this implementation
- The Authentik provider/application must be created manually — Authentik has no NixOS declarative API for this
- The secrets file must exist before deployment; if missing, `forgejo-oauth-setup` will fail but Forgejo itself will still start
