{ inputs, ... }:

{
  imports = [
    inputs.microvm.nixosModules.host
    ../../../../modules/nixos/vms/agent-vm/host.nix
  ];

  dog.services.agent-vm = (import ./settings.nix) // {
    autostart = true;
    dnsmasqBindBridgeInterface = false;
    dnsmasqBindInterfaces = false;
    dnsUpstreams = [ ];
    caddy.enable = true;
  };
}
