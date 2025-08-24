{
  lib,
  stdenvNoCC,
  bun,
  writableTmpDirAsHomeHook,
  kdePackages,
  makeBinaryWrapper,
}:

stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "plasma-toggle-tablet-mode";
  version = "1.2.0";
  src = ../.;

  node_modules = stdenvNoCC.mkDerivation {
    pname = "${finalAttrs.pname}-node_modules";
    inherit (finalAttrs) version src;

    impureEnvVars = lib.fetchers.proxyImpureEnvVars ++ [
      "GIT_PROXY_COMMAND"
      "SOCKS_SERVER"
    ];

    nativeBuildInputs = [
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

  nativeBuildInputs = [
    bun
    kdePackages.wrapQtAppsNoGuiHook
    makeBinaryWrapper
  ];

  buildInputs = [
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
      --prefix PATH : ${lib.makeBinPath finalAttrs.buildInputs}
  '';

  meta = {
    mainProgram = finalAttrs.pname;
  };
})
