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
      in
      {
        packages.plasma-toggle-tablet-mode = pkgs.python3Packages.buildPythonPackage {
          pname = "plasma-toggle-tablet-mode";
          version = "1.0";
          src = ./.;
          format = "other";

          propagatedBuildInputs =
            with pkgs.python3Packages;
            [
              pygobject3
            ]
            ++ [
              pkgs.gobject-introspection
              pkgs.kdePackages.kde-cli-tools
            ];

          nativeBuildInputs = with pkgs; [
            makeWrapper
            wrapGAppsHook
            kdePackages.wrapQtAppsNoGuiHook
          ];

          installPhase = ''
            mkdir -p $out/bin
            cp $src/plasma-toggle-tablet-mode.py $out/bin/plasma-toggle-tablet-mode
            chmod +x $out/bin/plasma-toggle-tablet-mode
            wrapProgram $out/bin/plasma-toggle-tablet-mode \
              --set GI_TYPELIB_PATH "${pkgs.gobject-introspection}/lib/girepository-1.0" \
              --prefix PATH : ${pkgs.kdePackages.kde-cli-tools}/bin
          '';
        };

        defaultPackage = self.packages.${system}.plasma-toggle-tablet-mode;
      }
    );
}
