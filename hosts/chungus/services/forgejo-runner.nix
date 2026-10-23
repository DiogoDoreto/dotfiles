{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.dog.services.nix-build-runner;
  roots = pkgs.writeScriptBin "nix-cache-roots" ''
    #!${pkgs.python3}/bin/python3
    ${builtins.readFile ../../../scripts/nix-cache-roots/roots.py}
  '';
in
{
  # Deliberately opt-in: see docs/nix-actions.md security/registration gates.
  options.dog.services.nix-build-runner.enable = lib.mkEnableOption "trusted dotfiles host runner";

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = builtins.attrNames config.services.gitea-actions-runner.instances == [ "chungus" ];
        message = "The Chungus host runner must not share its service state with other runners.";
      }
    ];

    users.groups.chungus-nix = { };
    users.users.chungus-nix = {
      isSystemUser = true;
      group = "chungus-nix";
      home = "/var/lib/gitea-runner/chungus";
    };
    systemd.tmpfiles.rules = [
      "d /var/lib/chungus-nix-roots 0700 chungus-nix chungus-nix -"
    ];

    services.gitea-actions-runner.instances.chungus = {
      enable = true;
      url = "https://git.local.doreto.com.br";
      name = "chungus-nix";
      # Repository registration token, NOT Mini's instance token; raw content.
      tokenFile = "/etc/secrets/forgejo/chungus-nix-token";
      labels = [ "chungus-nix:host" ];
      settings = {
        runner.capacity = 1;
        cache.enabled = false;
      };
      hostPackages = with pkgs; [
        bash
        coreutils
        gitMinimal
        nix
        nodejs
        roots
      ];
    };

    # The pinned module defaults to shared DynamicUser=gitea-runner. Override
    # the identity explicitly; do not accidentally create an unused account.
    systemd.services.gitea-runner-chungus = {
      environment = {
        SSL_CERT_FILE = "/etc/ssl/certs/ca-certificates.crt";
        NODE_EXTRA_CA_CERTS = "/etc/ssl/certs/ca-certificates.crt";
      };
      serviceConfig = {
        DynamicUser = lib.mkForce false;
        User = lib.mkForce "chungus-nix";
        Group = lib.mkForce "chungus-nix";
        UMask = "0077";
        NoNewPrivileges = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        PrivateTmp = true;
        ReadWritePaths = [ "/var/lib/chungus-nix-roots" ];
        InaccessiblePaths = [
          "-/run/podman"
          "-/run/docker.sock"
          "/etc/secrets"
        ];
      };
    };
  };
}
