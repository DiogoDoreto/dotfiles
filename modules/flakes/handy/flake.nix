{
  description = "Handy - Speech-to-text transcription app";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
  };
  outputs =
    { nixpkgs, utils, ... }:
    utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        version = "0.7.0";
        src = pkgs.fetchurl {
          url = "https://github.com/cjpais/Handy/releases/download/v${version}/Handy_${version}_amd64.AppImage";
          hash = "sha256-tTswFYLCPGtMbHAb2bQMsklRiRCVXLrtu4pQC8IHdqQ=";
        };
        appimage-contents = pkgs.appimageTools.extract {
          pname = "handy-appimage-contents";
          inherit version src;
        };
        appimage = pkgs.appimageTools.wrapType2 {
          inherit version src;
          pname = "handy";
          extraPkgs = pkgs: [ pkgs.alsa-lib ];
          extraInstallCommands = ''
            install -m 444 -D ${appimage-contents}/usr/share/applications/Handy.desktop \
              $out/share/applications/Handy.desktop
            mkdir -p $out/share/icons
            cp -r ${appimage-contents}/usr/share/icons/* $out/share/icons/
            find $out/share/icons -type f -exec chmod 444 {} +
            substituteInPlace $out/share/applications/Handy.desktop \
              --replace-fail 'Categories=' 'Categories=Utility'
          '';
        };
      in
      {
        packages.handy = appimage;
      }
    );
}
