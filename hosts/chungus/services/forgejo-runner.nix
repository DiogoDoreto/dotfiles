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
  maintenance = pkgs.writeShellApplication {
    name = "chungus-nix-cache-maintenance";
    runtimeInputs = [
      pkgs.coreutils
      pkgs.gitMinimal
      pkgs.nix
      pkgs.systemd
      pkgs.util-linux
      roots
    ];
    text = builtins.readFile ../../../scripts/nix-cache-roots/maintenance.sh;
  };
in
{
  # Deliberately opt-in: see docs/nix-actions.md security/registration gates.
  options.dog.services.nix-build-runner.enable = lib.mkEnableOption "trusted dotfiles host runner";

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion =
          builtins.attrNames config.services.forgejo-runner.instances == [ "chungus" ]
          && config.services.gitea-actions-runner.instances == { };
        message = "The Chungus host runner must not share its service state with other runners.";
      }
      {
        assertion =
          config.dog.services.nixos-generation-cleanup.enable
          && !config.dog.services.nixos-generation-cleanup.timer.enable;
        message = "Chungus cache maintenance requires the shared generation cleanup policy without its timer.";
      }
    ];

    users.groups.chungus-nix = { };
    users.users.chungus-nix = {
      isSystemUser = true;
      group = "chungus-nix";
      home = "/var/lib/forgejo-runner/chungus";
    };
    systemd.tmpfiles.rules = [
      "d /var/lib/chungus-nix-roots 0700 chungus-nix chungus-nix -"
    ];

    services.forgejo-runner.instances.chungus = {
      enable = true;
      # Existing UI-created runner's authentication secret, NOT a registration token.
      secrets.server.connections.default.token_url = "/etc/secrets/forgejo/chungus-nix-token";
      settings = {
        server.connections.default = {
          url = "https://git.local.doreto.com.br";
          uuid = "8e6aeca1-88bd-4333-9d8c-f382648ca963";
        };
        runner = {
          capacity = 1;
          labels = [ "chungus-nix:host" ];
          # Maintenance stops waiting after 24h, before this could cancel jobs.
          shutdown_timeout = lib.mkForce "25h";
        };
        cache.enabled = false;
      };
      hostPackages = with pkgs; [
        bash
        coreutils
        gitMinimal
        nix
        nodejs
        python3
        roots
      ];
    };

    # The pinned module defaults to DynamicUser. Override
    # the identity explicitly; do not accidentally create an unused account.
    systemd.services.forgejo-runner-chungus = {
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
        # SIGTERM must reach only the daemon, not the running host-job children.
        KillMode = lib.mkForce "mixed";
        TimeoutStopSec = lib.mkForce "infinity";
        ReadWritePaths = [ "/var/lib/chungus-nix-roots" ];
        InaccessiblePaths = [
          "-/run/podman"
          "-/run/docker.sock"
          "/etc/secrets"
        ];
      };
    };

    # No wantedBy, timer or scheduler: only explicit operator starts run GC.
    systemd.services.chungus-nix-cache-maintenance = {
      description = "Drain trusted Nix runner, reconcile cache roots and run generation cleanup";
      wants = [ "network-online.target" ];
      after = [ "network-online.target" ];
      environment.SSL_CERT_FILE = "/etc/ssl/certs/ca-certificates.crt";
      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${maintenance}/bin/chungus-nix-cache-maintenance";
        TimeoutStartSec = "infinity";
        LoadCredential = [
          "repository-token:/etc/secrets/forgejo/chungus-nix-reconcile-token"
        ];
        UMask = "0077";
        NoNewPrivileges = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        PrivateTmp = true;
        ReadWritePaths = [ "/var/lib/chungus-nix-roots" ];
        InaccessiblePaths = [ "/etc/secrets" ];
      };
    };
  };
}
