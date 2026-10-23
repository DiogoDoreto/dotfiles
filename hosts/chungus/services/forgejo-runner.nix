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
        assertion =
          builtins.attrNames config.services.forgejo-runner.instances == [ "chungus" ]
          && config.services.gitea-actions-runner.instances == { };
        message = "The Chungus host runner must not share its service state with other runners.";
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
          uuid = "3cec0ee6-b681-46ae-bb8d-eba9b6eec76c";
        };
        runner = {
          capacity = 1;
          labels = [ "chungus-nix:host" ];
        };
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
