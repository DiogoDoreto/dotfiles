{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, utils }: utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      github-copilot-cli = pkgs.callPackage ./package.nix {};
    in {
      packages = {
        inherit github-copilot-cli;
        default = github-copilot-cli;
      };
    }
  );
}
