{ pkgs, ... }:

let
  vars = import ../_variables.nix;
  p = builtins.mapAttrs (_: toString) vars.ports;

  scrapeConfig = pkgs.writeText "victoriametrics-scrape.yml" ''
    global:
      scrape_interval: 30s
      scrape_timeout: 10s

    scrape_configs:
      - job_name: node
        static_configs:
          - targets: ["localhost:${p.nodeExporter}"]
            labels:
              host: mini
          - targets: ["192.168.0.3:${p.nodeExporter}"]
            labels:
              host: chungus
          # Uncomment and set lapdog's LAN IP when it has a stable address:
          # - targets: ["<lapdog-ip>:${p.nodeExporter}"]
          #   labels:
          #     host: lapdog

      - job_name: systemd
        static_configs:
          - targets: ["localhost:${p.systemdExporter}"]
            labels:
              host: mini
          - targets: ["192.168.0.3:${p.systemdExporter}"]
            labels:
              host: chungus

      - job_name: nvidia_gpu
        static_configs:
          - targets: ["192.168.0.3:${p.nvidiaExporter}"]
            labels:
              host: chungus
  '';
in

{
  services.victoriametrics = {
    enable = true;
    listenAddress = "127.0.0.1:${p.victoriametrics}";
    retentionPeriod = "6"; # months
    extraOptions = [
      "-promscrape.config=${scrapeConfig}"
    ];
  };
}
