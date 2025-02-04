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
          "r1-small:ollama_chat/deepseek-r1:14b"
          "qwen:ollama_chat/qwen2.5-coder:32b"
          "qwen-small:ollama_chat/qwen2.5-coder:14b"
        ];
      };
    };
    ".aider.model.settings.yml" = let
        deepseek = {
          name = "ollama_chat/deepseek-r1";
          # remove_reasoning = "think";
          use_temperature = false;
          use_repo_map = true;
          edit_format = "diff";
        };
        qwen = {
          name = "ollama_chat/qwen2.5-coder";
          use_repo_map = true;
          edit_format = "diff";
        };
      in {
      text = builtins.toJSON [
        (deepseek // {
          name = deepseek.name + ":14b";
          extra_params = { num_ctx = 48000; };
        })
        (deepseek // {
          name = deepseek.name + ":32b";
          extra_params = { num_ctx = 10000; };
        })
        (qwen // {
          name = qwen.name + ":14b";
          extra_params = { num_ctx = 32768; };
        })
        (qwen // {
          name = qwen.name + ":32b";
          extra_params = { num_ctx = 10000; };
        })
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
