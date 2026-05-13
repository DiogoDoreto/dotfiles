{ ... }:

let
  miniVars = import ../../mini/_variables.nix;
  nodeExporterPort = miniVars.ports.nodeExporter;
  systemdExporterPort = miniVars.ports.systemdExporter;
  nvidiaExporterPort = miniVars.ports.nvidiaExporter;
  victorialogsPort = miniVars.ports.victorialogs;
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

  # Ship chungus journal logs to mini's VictoriaLogs instance
  services.journald.upload = {
    enable = true;
    settings.Upload = {
      URL = "http://192.168.0.2:${toString victorialogsPort}/insert/journald";
    };
  };

  # Allow mini (192.168.0.2) to scrape the exporters on this host
  networking.firewall.allowedTCPPorts = [
    nodeExporterPort
    systemdExporterPort
    nvidiaExporterPort
  ];
}
