{ inputs, ... }:

{
  imports = [
    inputs.microvm.nixosModules.microvm
    ../../../../modules/nixos/vms/agent-vm/guest.nix
  ];

  dog.services.agent-vm = (import ./settings.nix) // {
    workingDirectory = "/home/agent/projects";
  };
}
