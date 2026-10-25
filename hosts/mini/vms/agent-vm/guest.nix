{ inputs, pkgs, ... }:

{
  imports = [
    inputs.microvm.nixosModules.microvm
    ../../../../modules/nixos/vms/agent-vm/guest.nix
  ];

  dog.services.agent-vm = (import ./settings.nix) // {
    workingDirectory = "/home/agent";
    guest.tailscale.enable = true;
  };

  nix.gc = {
    automatic = true;
    dates = "*-*-* 03:00:00 Europe/Madrid";
    persistent = false;
  };

  systemd.services.nix-gc = {
    unitConfig.RequiresMountsFor = [ "/nix/.rw-store" ];
    serviceConfig.ExecCondition = pkgs.writeShellScript "agent-vm-gc-low-space" ''
      set -euo pipefail
      stats="$(${pkgs.coreutils}/bin/stat -f -c '%a %S' /nix/.rw-store)"
      read -r available_blocks block_size <<< "$stats"
      (( available_blocks * block_size < 15 * 1024 * 1024 * 1024 ))
    '';
  };
}
