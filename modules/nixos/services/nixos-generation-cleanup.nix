{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.dog.services.nixos-generation-cleanup;
  cleanup = pkgs.writeShellApplication {
    name = "nixos-generation-cleanup";
    runtimeInputs = [
      pkgs.coreutils
      pkgs.findutils
      pkgs.nix
      pkgs.util-linux
    ];
    text = ''
      set -euo pipefail

      exec 9>/run/lock/nixos-generation-cleanup.lock
      flock -n 9 || { echo 'Generation cleanup is already running' >&2; exit 1; }

      system_profile=/nix/var/nix/profiles/system
      cutoff=$(date -d '40 days ago' +%s)
      selected=$(readlink -f "$system_profile")
      booted=$(readlink -f /run/booted-system)

      # Keep the newest five, the selected generation and the booted generation.
      # nix-env takes the profile lock for each deletion.
      mapfile -t generations < <(
        find /nix/var/nix/profiles -maxdepth 1 -type l \
          -name 'system-[0-9]*-link' -printf '%f\n' |
          sort -t - -k 2,2nr
      )
      for i in "''${!generations[@]}"; do
        generation="''${generations[$i]}"
        link="/nix/var/nix/profiles/$generation"
        target=$(readlink -f "$link")
        if (( i < 5 )) || [[ "$target" == "$selected" || "$target" == "$booted" ]]; then
          continue
        fi
        if (( $(stat -c %Y "$link") < cutoff )); then
          number="''${generation#system-}"
          number="''${number%-link}"
          nix-env --profile "$system_profile" --delete-generations "$number"
        fi
      done

      # Age-prune other conventional Nix profiles explicitly. The global
      # nix-collect-garbage -d/--delete-older-than would bypass the system floor.
      while IFS= read -r -d "" profile; do
        [[ "$profile" == "$system_profile" ]] && continue
        nix-env --profile "$profile" --delete-generations 40d
      done < <(
        find /nix/var/nix/profiles -maxdepth 1 -type l ! -name '*-[0-9]*-link' -print0
        find /nix/var/nix/profiles/per-user -mindepth 2 -maxdepth 2 -type l ! -name '*-[0-9]*-link' -print0
      )

      "$system_profile/bin/switch-to-configuration" boot
      nix-store --gc
      nix store optimise
    '';
  };
in
{
  options.dog.services.nixos-generation-cleanup = {
    enable = lib.mkEnableOption "NixOS system generation cleanup";
    timer.enable = lib.mkEnableOption "Friday 04:00 generation cleanup timer";
  };

  config = lib.mkIf cfg.enable {
    systemd.services.nixos-generation-cleanup = {
      description = "Prune old NixOS generations while keeping rollback options";
      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${cleanup}/bin/nixos-generation-cleanup";
        TimeoutStartSec = "infinity";
      };
    };

    systemd.timers.nixos-generation-cleanup = lib.mkIf cfg.timer.enable {
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnCalendar = "Fri *-*-* 04:00:00";
        Persistent = true;
      };
    };
  };
}
