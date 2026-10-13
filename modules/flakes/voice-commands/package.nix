{
  lib,
  stdenvNoCC,
  fetchurl,
  unzip,
  python3,
  pipewire,
  playerctl,
  writeShellApplication,
  qt6,
  runCommand,
}:

let
  vosk = python3.pkgs.callPackage ./vosk-package.nix { };
  listenerPython = python3.withPackages (_: [ vosk ]);
  trayPython = python3.withPackages (ps: [ ps.pyside6 ]);

  model = stdenvNoCC.mkDerivation {
    name = "vosk-small-en-us-0.15";
    src = fetchurl {
      url = "https://alphacephei.com/vosk/models/vosk-model-small-en-us-0.15.zip";
      hash = "sha256-MPJiQsTrRJ+UjkLLMC3XpobLKaNCOoNn+Z/0F4CUJJg=";
    };
    nativeBuildInputs = [ unzip ];
    dontConfigure = true;
    dontBuild = true;
    installPhase = ''
      runHook preInstall
      mkdir -p "$out"
      cp -r . "$out/"
      runHook postInstall
    '';
  };

  listener = writeShellApplication {
    name = "voice-commands-listener";
    runtimeInputs = [
      pipewire
      playerctl
    ];
    text = ''
      export OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1
      exec ${listenerPython}/bin/python -u ${./runner.py} --model ${model} "$@"
    '';
  };
in
runCommand "voice-commands"
  {
    nativeBuildInputs = [ qt6.wrapQtAppsHook ];
    buildInputs = [
      qt6.qtbase
      qt6.qtwayland
    ];
    passthru = { inherit listener model; };
    meta = {
      description = "Local wake-phrase media commands with a pause/resume tray icon";
      mainProgram = "voice-commands";
      platforms = [ "x86_64-linux" ];
    };
  }
  ''
    mkdir -p "$out/bin"
    makeQtWrapper ${trayPython}/bin/python "$out/bin/voice-commands" \
      --add-flags "-u ${./tray.py} --listener ${lib.getExe listener}"
  ''
