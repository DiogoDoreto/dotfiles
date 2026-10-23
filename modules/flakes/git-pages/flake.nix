{
  description = "git-pages with a supported Go toolchain";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    upstream = {
      url = "git+https://codeberg.org/git-pages/git-pages";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { nixpkgs, upstream, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ upstream.inputs.gomod2nix.overlays.default ];
      };
      upstreamPackage = upstream.packages.${system}.default;
      git-pages = pkgs.buildGoApplication {
        inherit (upstreamPackage)
          pname
          version
          src
          buildInputs
          ldflags
          ;
        modules = upstream.outPath + "/gomod2nix.toml";
        # Upstream hardcodes removed go_1_25; go.mod also needs a newer Go than pkgs.go.
        go = pkgs.go_1_27;
      };
    in
    {
      packages.${system} = {
        inherit git-pages;
        default = git-pages;
      };
    };
}
