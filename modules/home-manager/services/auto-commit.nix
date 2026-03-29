{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.dog.services.auto-commit;

  script = pkgs.writeShellScript "auto-commit" (
    ''
      set -euo pipefail

      process_dir() {
        local dir="$1"
        if [ ! -d "$dir/.git" ]; then
          echo "Skipping $dir: not a git repository"
          return
        fi
        cd "$dir"
        if git diff --quiet && \
           git diff --cached --quiet && \
           [ -z "$(git ls-files --others --exclude-standard)" ]; then
          echo "Skipping $dir: working tree is clean"
          return
        fi
        git add -A
        msg=$(${pkgs.llm-agents.opencode}/bin/opencode run --command git-commit-title --format json | ${pkgs.jq}/bin/jq -r 'first(inputs | select(.type == "text")) | .part.text')
        git commit -m "$msg"
        echo "Committed in $dir: $msg"
      }

    ''
    + concatMapStrings (dir: ''
      process_dir ${escapeShellArg dir}
    '') cfg.watchedDirectories
  );
in
{
  options.dog.services.auto-commit = {
    enable = mkEnableOption "auto-commit";

    watchedDirectories = mkOption {
      type = types.listOf types.str;
      default = [ ];
      description = "List of git repository paths to automatically commit every 15 minutes.";
    };
  };

  config = mkIf cfg.enable {
    systemd.user.services.auto-commit = {
      Unit.Description = "Auto-commit changes in watched git repositories";
      Service = {
        Type = "oneshot";
        ExecStart = "${script}";
        Environment = "PATH=${pkgs.git}/bin:${config.home.homeDirectory}/.nix-profile/bin:/run/current-system/sw/bin:/nix/var/nix/profiles/default/bin";
      };
    };

    systemd.user.timers.auto-commit = {
      Unit.Description = "Periodic auto-commit timer";
      Timer = {
        OnBootSec = "15min";
        OnUnitActiveSec = "15min";
      };
      Install.WantedBy = [ "timers.target" ];
    };
  };
}
