{
  inputs = {
    utils.url = "github:numtide/flake-utils";
  };
  outputs =
    {
      self,
      nixpkgs,
      utils,
    }:
    utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        plasma-toggle-tablet-mode = pkgs.callPackage ./nix/package.nix { };
      in
      {
        packages.plasma-toggle-tablet-mode = plasma-toggle-tablet-mode;
        defaultPackage = plasma-toggle-tablet-mode;
        overlays.default = final: prev: {
          inherit plasma-toggle-tablet-mode;
        };
      }
    );
}
