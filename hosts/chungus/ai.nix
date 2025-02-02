{ config, pkgs, pkgs-unstable, ... }:

{
  home = {
    packages = with pkgs; [
      ollama-cuda
      openai-whisper
      # (openai-whisper.override {
      #   torch = python312Packages.torchWithCuda;
      # })

      (pkgs-unstable.callPackage ./aider-chat.nix {})

      (comfyuiPackages.comfyui.override {
        extensions = [
          comfyuiPackages.extensions.acly-inpaint
          comfyuiPackages.extensions.acly-tooling
          comfyuiPackages.extensions.cubiq-ipadapter-plus
          comfyuiPackages.extensions.fannovel16-controlnet-aux
        ];
        commandLineArgs = [ "--preview-method" "auto" ];
      })
      comfyuiPackages.krita-with-extensions
    ];
  };

  # Ollama
  systemd.user.services = {
    ollama = {
      Unit.Description = "Run Ollama";
      Install.WantedBy = [ "default.target" ];
      Service.ExecStart = "${pkgs.ollama-cuda}/bin/ollama serve";
    };
  };

  # Aider Chat
  home.sessionVariables = {
    OLLAMA_API_BASE = "http://127.0.0.1:11434";
  };
  home.file = {
    ".aider.conf.yml" = {
      text = builtins.toJSON {
        architect = true;
        model = "ollama_chat/deepseek-r1:32b";
        editor-model = "ollama_chat/qwen2.5-coder:32b";
        dark-mode = true;
        attribute-author = false;
        attribute-committer = false;
        alias = [
          "r1:ollama_chat/deepseek-r1:32b"
          "qwen:ollama_chat/qwen2.5-coder:32b"
        ];
      };
    };
    ".aider.model.settings.yml" = {
      text = builtins.toJSON [
        {
          name = "ollama_chat/deepseek-r1:32b";
          extra_params = {
            num_ctx = 64000;
          };
          remove_reasoning = "think";
          use_temperature = false;
          use_repo_map = true;
          edit_format = "diff";
        }
        {
          name = "ollama_chat/qwen2.5-coder:32b";
          extra_params = {
            # num_ctx = 8192;
            num_ctx = 32768;
          };
          use_repo_map = true;
          edit_format = "diff";
        }
      ];
    };
  };

  # ComfyUI
  xdg.desktopEntries.comfyui = {
    name = "ComfyUI";
    categories = [ "Application" "Graphics" ];
    terminal = true;
    exec = toString (pkgs.writeShellScript "start-comfyui.sh" ''
      cd ${config.xdg.dataHome}/comfyui
      comfyui
    '');
  };
}
