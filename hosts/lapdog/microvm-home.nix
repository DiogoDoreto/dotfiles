# Home Manager configuration for the lapdog coding-agent MicroVM.
{ pkgs, dog-lib, ... }:

let
  inherit (dog-lib) dotfilesSymlink;
in
{
  home = {
    username = "dog";
    homeDirectory = "/home/dog";

    packages = with pkgs; [
      llm-agents.claude-code
      claude-agent-acp
    ];

    file = {
      ".agents".source = dotfilesSymlink ".config/agents";
    };
  };

  targets.genericLinux.enable = true;

  dog.dotfilesPath = /home/dog/projects/dotfiles;

  dog.programs = {
    git.enable = true;
    cli-tools.enable = true;
  };
}
