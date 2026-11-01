let
  vars = import ../_variables.nix;
in
{
  dog.services.t3code = {
    enable = true;
    user = "dog";
    host = "192.168.0.3";
    port = vars.ports.t3code;
  };

  # Deliberately not a global allowedTCPPorts entry: only the wired LAN may connect.
  networking.firewall.extraCommands = ''
    iptables -A nixos-fw -i enp4s0 -p tcp -s 192.168.0.0/24 --dport ${toString vars.ports.t3code} -j nixos-fw-accept
  '';
  networking.firewall.extraStopCommands = ''
    iptables -D nixos-fw -i enp4s0 -p tcp -s 192.168.0.0/24 --dport ${toString vars.ports.t3code} -j nixos-fw-accept 2>/dev/null || true
  '';
}
