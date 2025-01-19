{ pkgs, ... }:

{
  imports = [
    ../../programs
  ];

  home = {
    packages = with pkgs; [
      neovim
      ollama-cuda
    ];

    stateVersion = "24.11";
  };

  programs = {
    bash = {
      initExtra = ''
        if [[ $TERM == "dumb" ]]; then
          export PS1="$ "
        fi
      '';
    };

    starship = {
      enableBashIntegration = false;
      settings = {
        username.show_always = true;
      };
    };
  };

  dog.programs = {
    cli-tools.enable = true;
    git.enable = true;
  };
}
