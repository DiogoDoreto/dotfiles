{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.dog.programs.git;
in
{
  options.dog.programs.git = {
    enable = mkEnableOption "git";
  };

  config = mkIf cfg.enable {
    programs.git = {
      enable = true;
      settings = {
        user.name = "Diogo Doreto";
        alias = {
          co = "checkout";
          st = "status -sb";
          lg = "log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit";
          fm = "fetch origin master:master";
          pf = "push --force-with-lease";
          undo = "reset --soft HEAD~1";
          ri = "rebase -i";
          rc = "rebase --continue";
          ra = "rebase --abort";
        };
        core.autocrlf = false;
        init.defaultBranch = "main";
        pull.ff = "only";
        rebase.autoSquash = true;
      };
      includes = [
        { path = "~/.config/git/config.private"; } # set user.email here
      ];
      ignores = [
        ".private/"
        # emacs
        ".dir-locals.el"
        # direnv
        ".direnv"
        ".envrc"
        # npm/yarn
        "npm-debug.log*"
        "yarn-debug.log*"
        "yarn-error.log*"
        # AI tools
        ".agent-shell/"
        "AGENTS.md"
        "CRUSH.md"
      ];
    };
  };
}
