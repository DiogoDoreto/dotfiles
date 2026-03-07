{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, utils }: utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      claude-agent-acp = pkgs.callPackage ./package.nix {};
    in {
      packages = {
        inherit claude-agent-acp;
        default = claude-agent-acp;
      };
      overlays.default = _final: _prev: {
        inherit claude-agent-acp;
      };
    }
  );
}
