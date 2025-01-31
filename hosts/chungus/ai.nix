{ pkgs, ... }:

{
  home = {
    packages = with pkgs; [
      ollama-cuda
      openai-whisper
      # (openai-whisper.override {
      #   torch = python312Packages.torchWithCuda;
      # })
      aider-chat
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
  home.file.".aider.conf.yml" = {
    text = builtins.toJSON {
      editor = "emacs";
      model = "ollama_chat/deepseek-r1:32b";
      editor-model = "ollama_chat/qwen2.5-coder:32b";
      dark-mode = true;
      analytics-disable = true;
    };
  };
}
