{
  description = "Nix-managed pi-web-access extension for Pi";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { nixpkgs, ... }: {
    overlays.default = final: prev: {
      pi-web-access = final.callPackage ./package.nix { };
    };

    packages.x86_64-linux =
      let
        pkgs = import nixpkgs { system = "x86_64-linux"; };
      in
      rec {
        pi-web-access = pkgs.callPackage ./package.nix { };
        default = pi-web-access;
      };
  };
}
