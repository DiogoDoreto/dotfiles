{
  description = "Intel IPU7 camera support";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs =
    { ... }:
    {
      overlays = [
        (import ./overlay.nix)
      ];

      nixosModules = [
        ./ipu7-module.nix
      ];
    };
}
