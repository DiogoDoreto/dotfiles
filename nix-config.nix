inputs:
{
  lib,
  config,
  options,
  pkgs,
  ...
}:

let
  isNixOS = options ? environment;
  filteredInputs = lib.filterAttrs (_: v: v ? outputs) inputs;
in
{
  config = lib.mkMerge (
    [
      {
        nix.registry = lib.mapAttrs (_: flake: { inherit flake; }) filteredInputs;

        # but NIX_PATH is still used by many useful tools, so we set it to the same value as the one used by this flake.
        # Make `nix repl '<nixpkgs>'` use the same nixpkgs as the one used by this flake.
        # https://github.com/NixOS/nix/issues/9574
        nix.settings.nix-path = lib.mkForce (
          if isNixOS then
            "nixpkgs=/etc/nix/inputs/nixpkgs"
          else
            "nixpkgs=${config.home.homeDirectory}/.config/nix/inputs/nixpkgs"
        );

        nix.settings.experimental-features = [
          "nix-command"
          "flakes"
        ];
      }
    ]
    ++ lib.optional isNixOS {
      nix.channel.enable = false;
      environment.etc = lib.mapAttrs' (name: flake: {
        name = "nix/inputs/${name}";
        value.source = "${flake}";
      }) inputs;
    }
    ++ lib.optional (!isNixOS) {
      nix.package = pkgs.nix;
      home.file = lib.mapAttrs' (name: flake: {
        name = ".config/nix/inputs/${name}";
        value.source = "${flake}";
      }) filteredInputs;
    }
  );
}
