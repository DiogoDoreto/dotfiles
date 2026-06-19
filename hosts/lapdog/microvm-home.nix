# Home Manager configuration for the lapdog coding-agent MicroVM.
{ pkgs, dog-lib, ... }:

let
  inherit (dog-lib) dotfilesSymlink;
  inherit (pkgs.lib) filterAttrs listToAttrs nameValuePair;

  agentSkillNames = builtins.attrNames (
    filterAttrs (_: type: type == "directory") (builtins.readDir ../../.config/agents/skills)
  );
  agentSkillFiles = listToAttrs (
    map (
      name:
      nameValuePair ".agents/skills/${name}" { source = dotfilesSymlink ".config/agents/skills/${name}"; }
    ) agentSkillNames
  );
in
{
  home = {
    username = "dog";
    homeDirectory = "/home/dog";

    packages = with pkgs; [
      llm-agents.claude-code
      claude-agent-acp
    ];

    file = agentSkillFiles;
  };

  targets.genericLinux.enable = true;

  dog.dotfilesPath = /home/dog/projects/dotfiles;

  dog.programs = {
    git.enable = true;
    cli-tools.enable = true;
    claude-code.enable = true;
  };
}
