{ pkgs, lib, ... }:

{
  users.groups.authentik = { };
  users.users.authentik = {
    isSystemUser = true;
    group = "authentik";
  };
  systemd.services.authentik-secret-setup = {
    description = "Generate secret key for Authentik";
    serviceConfig = {
      Type = "oneshot";
      ExecStart = pkgs.writeShellScript "authentik-secret-setup" ''
        if [ ! -f /etc/secrets/authentik/env ]; then
          mkdir -p /etc/secrets/authentik
          new_secret=$(${pkgs.openssl}/bin/openssl rand -base64 60)
          echo "AUTHENTIK_SECRET_KEY=$new_secret" > /etc/secrets/authentik/env
          chmod 600 /etc/secrets/authentik/env
        fi
      '';
      User = "root";
      Group = "root";
    };
    wantedBy = [ "authentik.service" ];
  };
  systemd.services.authentik = {
    after = lib.mkAfter [ "authentik-secret-setup.service" ];
    wants = lib.mkAfter [ "authentik-secret-setup.service" ];
  };
  services.authentik = {
    enable = true;
    environmentFile = "/etc/secrets/authentik/env";
    settings = {
      disable_startup_analytics = true;
      avatars = "initials";
    };
  };
}
