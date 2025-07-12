{ config, lib, pkgs-unstable, ... }:

with lib;

let
  cfg = config.dog.programs.aider;
in {
  options.dog.programs.aider = {
    enable = mkEnableOption "Aider-Chat";
    ollamaApiBase = mkOption {
      type = types.str;
      default = "http://127.0.0.1:11434";
    };
  };

  config = mkIf cfg.enable {
    home = {
      packages = with pkgs-unstable; [
        aider-chat
      ];

      sessionVariables = {
        OLLAMA_API_BASE = cfg.ollamaApiBase;
      };

      file = {
        ".aider.conf.yml" = {
          text = builtins.toJSON {
            architect = true;
            model = "ollama_chat/qwq:latest";
            editor-model = "ollama_chat/qwen2.5-coder:32b";
            dark-mode = true;
            attribute-author = false;
            attribute-committer = false;
            alias = [
              "qwq:ollama_chat/qwq:latest"
              "qwen:ollama_chat/qwen2.5-coder:32b"
              "qwen-small:ollama_chat/qwen2.5-coder:14b"
            ];
          };
        };
        ".aider.model.settings.yml" = let
          qwen = {
            name = "ollama_chat/qwen2.5-coder";
            use_repo_map = true;
            edit_format = "diff";
          };
        in {
          text = builtins.toJSON [
            (qwen // {
              name = qwen.name + ":14b";
              extra_params = { num_ctx = 32768; };
            })
            (qwen // {
              name = qwen.name + ":32b";
              extra_params = { num_ctx = 10000; };
            })
            {
              name = "ollama_chat/qwq:latest";
              reasoning_tag = "think";
            }
          ];
        };
      };
    };
  };
}
