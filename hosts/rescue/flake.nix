{
  description = "Portable NixOS rescue image";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs =
    { nixpkgs, ... }:
    let
      system = "x86_64-linux";
      rescue = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [ ./configuration.nix ];
      };
    in
    {
      nixosConfigurations.rescue = rescue;
      packages.${system} = {
        default = rescue.config.system.build.isoImage;
        isoImage = rescue.config.system.build.isoImage;
      };
    };
}
