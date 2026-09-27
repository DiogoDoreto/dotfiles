{
  config,
  lib,
  dog-lib,
  ...
}:

let
  inherit (lib)
    filterAttrs
    listToAttrs
    mkEnableOption
    mkIf
    nameValuePair
    ;
  inherit (dog-lib) dotfilesSymlink;

  skillNames = builtins.attrNames (
    filterAttrs (_: type: type == "directory") (builtins.readDir ../../../.config/agents/skills)
  );
in
{
  options.dog.programs.agent-skills.enable = mkEnableOption "repo-managed agent skills";

  config = mkIf config.dog.programs.agent-skills.enable {
    home.file = listToAttrs (
      map (
        name:
        nameValuePair ".agents/skills/${name}" { source = dotfilesSymlink ".config/agents/skills/${name}"; }
      ) skillNames
    );
  };
}
