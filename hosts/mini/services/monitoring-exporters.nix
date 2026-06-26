{ ... }:

let
  vars = import ../_variables.nix;
in

{
  services.prometheus.exporters.node = {
    enable = true;
    listenAddress = "127.0.0.1";
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
    listenAddress = "127.0.0.1";
    port = vars.ports.systemdExporter;
  };
}
