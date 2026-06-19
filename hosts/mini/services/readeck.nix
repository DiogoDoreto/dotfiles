{ pkgs, ... }:

let
  vars = import ../_variables.nix;
  p = builtins.mapAttrs (_: toString) vars.ports;
in

{
  # To promote a user to admin (readeck user CLI is not on PATH, run as the service user):
  #   READECK_PID=$(systemctl show readeck --property=MainPID --value)
  #   READECK_BIN=$(readlink /proc/$READECK_PID/exe)
  #   cp $(tr '\0' '\n' < /proc/$READECK_PID/cmdline | grep readeck.toml) /tmp/readeck.toml
  #   nsenter -t $READECK_PID -m -u -- bash -c \
  #     "cd /var/lib/readeck && $READECK_BIN user -config /tmp/readeck.toml -user <username> -group admin"
  #
  # Authentik setup (manual, one-time):
  #   1. Create a Proxy Provider for https://readeck.local.doreto.com.br
  #      using "Forward auth (single application)" mode.
  #   2. Create an application bound to that provider.
  #   3. Add the application to the embedded outpost.
  #
  # Readeck only supports external auth through trusted forwarded headers.
  # We intentionally do not forward Authentik groups because Readeck accepts
  # only one of "user", "staff" or "admin" in Remote-Groups.
  systemd.services.readeck-secret-setup = {
    description = "Generate secret key for Readeck";
    serviceConfig = {
      Type = "oneshot";
      ExecStart = pkgs.writeShellScript "readeck-secret-setup" ''
        if [ ! -f /etc/secrets/readeck/env ]; then
          install -d -m 700 /etc/secrets/readeck
          new_secret=$(${pkgs.openssl}/bin/openssl rand -base64 60)
          echo "READECK_SECRET_KEY=$new_secret" > /etc/secrets/readeck/env
          chmod 600 /etc/secrets/readeck/env
        fi
      '';
      User = "root";
      Group = "root";
    };
    wantedBy = [ "readeck.service" ];
  };

  services.readeck = {
    enable = true;
    environmentFile = "/etc/secrets/readeck/env";
    settings = {
      server = {
        host = "127.0.0.1";
        port = vars.ports.readeck;
        base_url = "https://readeck.local.doreto.com.br";
        allowed_hosts = [ "readeck.local.doreto.com.br" ];
        trusted_proxies = [ "127.0.0.1" ];
      };
      auth.forwarded = {
        enabled = true;
        provisioning = true;
        header_user = "Remote-User";
        header_email = "Remote-Email";
      };
    };
  };

  systemd.services.readeck = {
    after = [ "readeck-secret-setup.service" ];
    wants = [ "readeck-secret-setup.service" ];
  };

  services.caddy.virtualHosts."readeck.local.doreto.com.br".extraConfig = ''
    # Authentik's forwardHandleCaddy uses X-Forwarded-Host to match the application URL.
    # Caddy does not set this automatically; must be set before forward_auth runs.
    request_header X-Forwarded-Host {http.request.host}

    # Proxy outpost paths directly to Authentik. These must be excluded from
    # forward_auth or the Authentik login redirect loops.
    @outpost path /outpost.goauthentik.io/*
    reverse_proxy @outpost http://127.0.0.1:${p.authentik}

    # Readeck API and OPDS endpoints use Readeck API keys, not Authentik.
    @needsAuth {
      not path /api /api/* /opds /opds/* /outpost.goauthentik.io/*
    }
    forward_auth @needsAuth http://127.0.0.1:${p.authentik} {
      uri /outpost.goauthentik.io/auth/caddy
      copy_headers X-Authentik-Username>Remote-User X-Authentik-Email>Remote-Email
      trusted_proxies private_ranges
    }

    reverse_proxy 127.0.0.1:${p.readeck}
  '';
}
