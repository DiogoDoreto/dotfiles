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
  kdePackages,
  stdenv,
  pkg-config,
  wayland,
  wayland-scanner,
  openssl_3_5,
  espeak-ng,
  runCommand,
}:

let
  vosk = python3.pkgs.callPackage ./vosk-package.nix { };
  listenerPython = python3.withPackages (_: [ vosk ]);
  trayPython = python3.withPackages (ps: [ ps.pyside6 ]);
  qtInputs = [
    qt6.qtbase
    qt6.qtwayland
    qt6.qtdeclarative
    kdePackages.layer-shell-qt
  ];
  testPython = python3.withPackages (ps: [
    vosk
    ps.pyside6
  ]);
  testSpeech = espeak-ng.override { mbrolaSupport = false; };
  source = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.unions [
      ./runner.py
      ./tray.py
      ./listener.py
      ./vocabulary.py
      ./session.py
      ./desktop.py
      ./scroll.py
      ./grid.py
      ./navigation.py
      ./grid.qml
    ];
  };
  testSource = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.unions [
      ./test_grid.py
      ./test_navigation.py
      ./test_scroll.py
      ./test_listener.py
      ./listener_test_child.py
    ];
  };

  pointer = stdenv.mkDerivation {
    pname = "voice-commands-pointer";
    version = "0.1";
    dontUnpack = true;
    nativeBuildInputs = [
      pkg-config
      wayland-scanner
    ];
    buildInputs = [ wayland ];
    buildPhase = ''
      protocol=${kdePackages.plasma-wayland-protocols}/share/plasma-wayland-protocols/fake-input.xml
      wayland-scanner client-header "$protocol" fake-input-client-protocol.h
      wayland-scanner private-code "$protocol" fake-input-protocol.c
      $CC -Wall -Wextra -Werror -I. ${./pointer.c} fake-input-protocol.c \
        $(pkg-config --cflags --libs wayland-client) -o voice-commands-pointer
    '';
    installPhase = ''
      install -Dm755 voice-commands-pointer "$out/bin/voice-commands-pointer"
      mkdir -p "$out/share/applications"
      substitute ${./pointer.desktop} "$out/share/applications/voice-commands-pointer.desktop" \
        --replace-fail @pointer@ "$out/bin/voice-commands-pointer"
    '';
  };

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
      exec ${listenerPython}/bin/python -u ${source}/runner.py --model ${model} "$@"
    '';
  };

  testRunner =
    runCommand "check-voice-grid"
      {
        nativeBuildInputs = [ qt6.wrapQtAppsHook ];
        buildInputs = qtInputs;
      }
      ''
        mkdir -p "$out/bin"
        makeQtWrapper ${testPython}/bin/python "$out/bin/check-voice-grid" \
          --prefix PYTHONPATH : ${source} \
          --set VOICE_TEST_POINTER ${pointer}/bin/voice-commands-pointer \
          --set VOICE_TEST_MODEL ${model} \
          --set VOICE_TEST_SPEECH ${lib.getExe testSpeech} \
          --set VOICE_TEST_STARTUP_PROBE ${./test_startup.py} \
          --set VOICE_TEST_RENDERING_PROBE ${./test_rendering.py} \
          --set VOICE_TEST_OLD_CRYPTO ${lib.getLib openssl_3_5}/lib/libcrypto.so.3 \
          --add-flags "-B ${testSource}/test_grid.py"
        makeQtWrapper ${testPython}/bin/python "$out/bin/check-voice-listener" \
          --prefix PYTHONPATH : ${source} \
          --add-flags "-B ${testSource}/test_listener.py"
        makeWrapper ${python3}/bin/python "$out/bin/check-voice-navigation" \
          --prefix PYTHONPATH : ${source} \
          --add-flags "-B ${testSource}/test_navigation.py"
      '';

  tests = runCommand "voice-commands-tests" { } ''
    export HOME="$TMPDIR" XDG_CACHE_HOME="$TMPDIR/.cache"
    export QT_QPA_PLATFORM=offscreen QT_QUICK_BACKEND=software
    ${testRunner}/bin/check-voice-navigation
    ${testRunner}/bin/check-voice-listener
    ${testRunner}/bin/check-voice-grid
    touch "$out"
  '';
in
runCommand "voice-commands"
  {
    nativeBuildInputs = [ qt6.wrapQtAppsHook ];
    buildInputs = qtInputs;
    passthru = {
      inherit
        listener
        model
        pointer
        tests
        testRunner
        ;
    };
    meta = {
      description = "Local voice media commands, scrolling, and experimental mouse grid with tray control";
      mainProgram = "voice-commands";
      platforms = [ "x86_64-linux" ];
    };
  }
  ''
    mkdir -p "$out/bin"
    mkdir -p "$out/share/applications"
    ln -s ${pointer}/share/applications/voice-commands-pointer.desktop "$out/share/applications/"
    makeQtWrapper ${trayPython}/bin/python "$out/bin/voice-commands" \
      --add-flags "-u ${source}/tray.py --listener ${lib.getExe listener} --pointer ${pointer}/bin/voice-commands-pointer"
  ''
