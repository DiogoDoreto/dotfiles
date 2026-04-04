inputs:
{ lib, ... }:
{
  nix.registry = lib.mapAttrs (_: flake: { inherit flake; }) (
    lib.filterAttrs (_: v: v ? outputs) inputs
  );
  nix.channel.enable = false;
  # but NIX_PATH is still used by many useful tools, so we set it to the same value as the one used by this flake.
  # Make `nix repl '<nixpkgs>'` use the same nixpkgs as the one used by this flake.
  environment.etc = lib.mapAttrs' (name: flake: {
    name = "nix/inputs/${name}";
    value.source = "${flake}";
  }) inputs;
  # https://github.com/NixOS/nix/issues/9574
  nix.settings.nix-path = lib.mkForce "nixpkgs=/etc/nix/inputs/nixpkgs";

  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];
}
