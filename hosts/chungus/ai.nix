{ config, pkgs-unstable, ... }:

let
  whisper-pkg = pkgs-unstable.openai-whisper-cpp.override {
    config = {
      cudaSupport = true;
      rocmSupport = false;
    };
  };
in {
  home = {
    packages = with pkgs-unstable; [
      whisper-pkg

      (comfyuiPackages.comfyui.override {
        extensions = [
          comfyuiPackages.extensions.acly-inpaint
          comfyuiPackages.extensions.acly-tooling
          comfyuiPackages.extensions.cubiq-ipadapter-plus
          comfyuiPackages.extensions.fannovel16-controlnet-aux
        ];
        commandLineArgs = [ "--preview-method" "auto" ];
      })
      (comfyuiPackages.krita-with-extensions.override {
        krita-ai-diffusion = (comfyuiPackages.krita-ai-diffusion.overrideAttrs {
          src = fetchFromGitHub {
            owner = "Acly";
            repo = "krita-ai-diffusion";
            fetchSubmodules = true;
            rev = "9536fd028ed8b88525268b2c44585cbee50be3e8";
            hash = "sha256-EjMpM2+Cz7nDXF16s//D5XrC0sGOsnLVEmHcsDlvIdo=";
          };
          patches = [];
        });
      })
    ];
  };

  dog.programs.emacs = {
    whisperPackage = whisper-pkg;
    extraConfig = ''
      (setq whisper-model "large-v3-turbo")
    '';
  };

  # ComfyUI
  xdg.desktopEntries.comfyui = {
    name = "ComfyUI";
    categories = [ "Application" "Graphics" ];
    terminal = true;
    exec = toString (pkgs-unstable.writeShellScript "start-comfyui.sh" ''
      cd ${config.xdg.dataHome}/comfyui
      comfyui
    '');
  };
}
