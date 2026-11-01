{ pkgs, ... }:

let
  vars = import ../_variables.nix;
  t3code = pkgs.llm-agents.t3code.override { providerPackages = [ ]; };
in
{
  systemd.services.t3code = {
    description = "T3 Code LAN backend";
    wantedBy = [ "multi-user.target" ];
    wants = [ "network-online.target" ];
    after = [ "network-online.target" ];
    environment = {
      HOME = "/home/dog";
      T3CODE_HOME = "/home/dog/.t3";
    };
    serviceConfig = {
      Type = "simple";
      User = "dog";
      WorkingDirectory = "/home/dog";
      # A login shell includes Home Manager's Pi wrapper and existing agent tools.
      ExecStart = "${pkgs.bash}/bin/bash -lc 'exec ${t3code}/bin/t3 serve --host 192.168.0.3 --port ${toString vars.ports.t3code} --no-browser'";
      Restart = "on-failure";
      RestartSec = "5s";
      UMask = "0077";
    };
  };

  # Deliberately not a global allowedTCPPorts entry: only the wired LAN may connect.
  networking.firewall.extraCommands = ''
    iptables -A nixos-fw -i enp4s0 -p tcp -s 192.168.0.0/24 --dport ${toString vars.ports.t3code} -j nixos-fw-accept
  '';
  networking.firewall.extraStopCommands = ''
    iptables -D nixos-fw -i enp4s0 -p tcp -s 192.168.0.0/24 --dport ${toString vars.ports.t3code} -j nixos-fw-accept 2>/dev/null || true
  '';
}
