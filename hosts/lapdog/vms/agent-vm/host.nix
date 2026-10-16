{ inputs, ... }:

{
  imports = [
    inputs.microvm.nixosModules.host
    ../../../../modules/nixos/vms/agent-vm/host.nix
  ];

  dog.services.agent-vm = (import ./settings.nix) // {
    dnsForwardZones = [
      {
        domain = "local.doreto.com.br";
        server = "192.168.0.2";
      }
      {
        domain = "home";
        server = "192.168.0.2";
      }
    ];
  };
}
