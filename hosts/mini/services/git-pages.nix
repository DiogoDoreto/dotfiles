{ inputs, pkgs, ... }:

let
  vars = import ../_variables.nix;
  p = builtins.mapAttrs (_: toString) vars.ports;
  gitPagesPackage = inputs.git-pages.packages.${pkgs.system}.default;
  gitPagesConfig = pkgs.writeText "git-pages.toml" ''
    features = ["expiration", "preview"]
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
