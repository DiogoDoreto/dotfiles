{ ... }:

let
  miniVars = import ../../mini/_variables.nix;
  nodeExporterPort = miniVars.ports.nodeExporter;
  systemdExporterPort = miniVars.ports.systemdExporter;
  nvidiaExporterPort = miniVars.ports.nvidiaExporter;
in

{
  services.prometheus.exporters.node = {
    enable = true;
    listenAddress = "0.0.0.0";
    port = nodeExporterPort;
    enabledCollectors = [
      "cpu"
      "diskstats"
      "filesystem"
      "loadavg"
      "meminfo"
      "netdev"
      "systemd"
      "time"
      "uname"
    ];
  };

  services.prometheus.exporters.systemd = {
    enable = true;
    listenAddress = "0.0.0.0";
    port = systemdExporterPort;
  };

  services.prometheus.exporters.nvidia-gpu = {
    enable = true;
    listenAddress = "0.0.0.0";
    port = nvidiaExporterPort;
  };

  # Allow only mini (192.168.0.2) to scrape the exporters on this host.
  networking.firewall.extraCommands = ''
    iptables -A nixos-fw -p tcp -s 192.168.0.2 --dport ${toString nodeExporterPort} -j nixos-fw-accept
    iptables -A nixos-fw -p tcp -s 192.168.0.2 --dport ${toString systemdExporterPort} -j nixos-fw-accept
    iptables -A nixos-fw -p tcp -s 192.168.0.2 --dport ${toString nvidiaExporterPort} -j nixos-fw-accept
  '';
}
