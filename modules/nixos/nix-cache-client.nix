{ config, lib, ... }:

let
  cfg = config.dog.nix-cache;
in
{
  options.dog.nix-cache = {
    enable = lib.mkEnableOption "Chungus's signed binary cache";
    publicKey = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      # Public half only; the secret stays on Chungus outside the Nix store.
      default = "nix-cache.local.doreto.com.br-1:/37nJvnSImJtzKHzMDI1e2iKk1SisZAEFujENOmB/co=";
      description = "Public signing key for nix-cache.local.doreto.com.br";
    };
  };

  config = lib.mkIf cfg.enable {
    warnings = lib.optional (cfg.publicKey == null) ''
      Chungus binary cache is not configured: set dog.nix-cache.publicKey
      after provisioning /etc/secrets/harmonia/cache.secret on Chungus.
    '';

    nix.settings = lib.mkIf (cfg.publicKey != null) {
      substituters = [ "https://nix-cache.local.doreto.com.br" ];
      trusted-public-keys = [ cfg.publicKey ];
    };
  };
}
