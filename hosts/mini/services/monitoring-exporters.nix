{ ... }:

let
  vars = import ../_variables.nix;
  p = builtins.mapAttrs (_: toString) vars.ports;
in

{
  services.prometheus.exporters.node = {
    enable = true;
    listenAddress = "0.0.0.0";
    port = vars.ports.nodeExporter;
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
    port = vars.ports.systemdExporter;
  };

  # Allow VictoriaMetrics (running on this host) to scrape local exporters.
  # Remote hosts (chungus) also need these ports open — handled in their own configs.
  networking.firewall.allowedTCPPorts = [
    vars.ports.nodeExporter
    vars.ports.systemdExporter
  ];
}
