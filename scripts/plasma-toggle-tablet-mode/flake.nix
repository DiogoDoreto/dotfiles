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
        packages.plasma-toggle-tablet-mode = pkgs.stdenvNoCC.mkDerivation (finalAttrs: {
          pname = "plasma-toggle-tablet-mode";
          version = "1.1.0";
          src = ./.;

          node_modules = pkgs.stdenvNoCC.mkDerivation {
            pname = "${finalAttrs.pname}-node_modules";
            inherit (finalAttrs) version src;

            impureEnvVars = pkgs.lib.fetchers.proxyImpureEnvVars ++ [
              "GIT_PROXY_COMMAND"
              "SOCKS_SERVER"
            ];

            nativeBuildInputs = with pkgs; [
              bun
              writableTmpDirAsHomeHook
            ];

            dontConfigure = true;

            buildPhase = ''
              runHook preBuild

              export BUN_INSTALL_CACHE_DIR=$(mktemp -d)

              bun install \
                --frozen-lockfile \
                --no-progress \
                --production

              runHook postBuild
            '';

            installPhase = ''
              runHook preInstall

              mkdir -p $out/node_modules
              cp -R ./node_modules $out

              runHook postInstall
            '';

            # Required else we get errors that our fixed-output derivation references store paths
            dontFixup = true;

            outputHash = "sha256-pCFJNQeJ0FW8j5nmqH2uNBDFVNidi8gLbQ3lhEEJfpo=";
            outputHashAlgo = "sha256";
            outputHashMode = "recursive";
          };

          nativeBuildInputs = with pkgs; [
            bun
            kdePackages.wrapQtAppsNoGuiHook
            makeBinaryWrapper
          ];

          buildInputs = with pkgs; [
            kdePackages.kconfig
            kdePackages.qttools
          ];

          configurePhase = ''
            runHook preConfigure

            cp -R ${finalAttrs.node_modules}/node_modules .

            runHook postConfigure
          '';

          buildPhase = ''
            runHook preBuild

            bun build \
              --compile \
              --outfile=${finalAttrs.pname} \
              ./cli.ts

            runHook postBuild
          '';

          installPhase = ''
            runHook preInstall

            install -Dm755 ${finalAttrs.pname} $out/bin/${finalAttrs.pname}

            runHook postInstall
          '';

          postFixup = ''
            wrapProgram $out/bin/${finalAttrs.pname} \
              --prefix PATH : ${pkgs.lib.makeBinPath finalAttrs.buildInputs}
          '';

          meta = {
            mainProgram = finalAttrs.pname;
          };
        });

        defaultPackage = self.packages.${system}.plasma-toggle-tablet-mode;
      }
    );
}
