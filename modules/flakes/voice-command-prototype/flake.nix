{
  description = "Throwaway local voice-command comparison: sherpa-onnx and Vosk";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/e158d9ed9b51c98974c5e66e1ba1c9e0255fecaa";

  outputs =
    { nixpkgs, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      python = pkgs.python3;
      vosk = python.pkgs.callPackage ./vosk-package.nix { };
      sherpa = pkgs.sherpa-onnx.override { python3Packages = python.pkgs; };

      model =
        name: src: nativeBuildInputs:
        pkgs.stdenvNoCC.mkDerivation {
          inherit name src nativeBuildInputs;
          dontConfigure = true;
          dontBuild = true;
          installPhase = ''
            mkdir -p "$out"
            cp -r . "$out/"
          '';
        };

      sherpaModel = model "sherpa-gigaspeech-kws-2024-01-01" (pkgs.fetchurl {
        url = "https://github.com/k2-fsa/sherpa-onnx/releases/download/kws-models/sherpa-onnx-kws-zipformer-gigaspeech-3.3M-2024-01-01.tar.bz2";
        hash = "sha256-8XABO0cW5Btiub/YCWh8IHzveY75vGU01SThevm2Vho=";
      }) [ ];

      voskModel = model "vosk-small-en-us-0.15" (pkgs.fetchurl {
        url = "https://alphacephei.com/vosk/models/vosk-model-small-en-us-0.15.zip";
        hash = "sha256-MPJiQsTrRJ+UjkLLMC3XpobLKaNCOoNn+Z/0F4CUJJg=";
      }) [ pkgs.unzip ];

      runner =
        engine: speechModel:
        let
          env = python.withPackages (
            ps:
            [ ps.numpy ]
            ++ pkgs.lib.optionals (engine == "sherpa") [ ps.sentencepiece ]
            ++ pkgs.lib.optionals (engine == "vosk") [ vosk ]
          );
        in
        pkgs.writeShellApplication {
          name = "voice-command-${engine}-prototype";
          runtimeInputs = [
            pkgs.pipewire
            pkgs.playerctl
          ];
          text = ''
            export OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1
            ${pkgs.lib.optionalString (engine == "sherpa") ''export PYTHONPATH="${sherpa.python}"''}
            exec ${env}/bin/python -u ${./runner.py} --engine ${engine} --model ${speechModel} "$@"
          '';
        };

      voskRunner = runner "vosk" voskModel;
      trayPython = python.withPackages (ps: [ ps.pyside6 ]);
      tray =
        pkgs.runCommand "voice-command-vosk-tray-prototype"
          {
            nativeBuildInputs = [ pkgs.qt6.wrapQtAppsHook ];
            buildInputs = [
              pkgs.qt6.qtbase
              pkgs.qt6.qtwayland
            ];
            meta.mainProgram = "voice-command-vosk-tray-prototype";
          }
          ''
            mkdir -p "$out/bin"
            makeQtWrapper ${trayPython}/bin/python "$out/bin/voice-command-vosk-tray-prototype" \
              --add-flags "-u ${./tray.py} --listener ${pkgs.lib.getExe voskRunner}"
          '';
    in
    {
      packages.${system} = rec {
        sherpa = runner "sherpa" sherpaModel;
        vosk = voskRunner;
        vosk-tray = tray;
        default = sherpa;
        sherpa-model = sherpaModel;
        vosk-model = voskModel;
      };
    };
}
