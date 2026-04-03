{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, utils }: utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      packages = {
        pi-acp = pkgs.callPackage ./pi-acp.nix {};
      };
    in {
      packages = packages // { default = packages.pi-acp; };
      overlays.default = _final: _prev: packages;
    }
  );
}
