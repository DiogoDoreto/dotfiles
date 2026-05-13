{ ... }:

let
  vars = import ../_variables.nix;
  p = builtins.mapAttrs (_: toString) vars.ports;
in

{
  virtualisation.oci-containers.containers.victorialogs = {
    # To manually upgrade:
    #   sudo podman pull docker.io/victoriametrics/victoria-logs:latest
    #   sudo systemctl restart podman-victorialogs.service
    image = "docker.io/victoriametrics/victoria-logs:latest";
    ports = [ "127.0.0.1:${p.victorialogs}:9428" ];
    volumes = [
      "/var/lib/victorialogs:/vlogs"
    ];
    cmd = [
      "--storageDataPath=/vlogs"
      "--retentionPeriod=3"
    ];
  };

  systemd.tmpfiles.rules = [
    "d /var/lib/victorialogs 0750 root root -"
  ];

  # Ship mini's own journal to VictoriaLogs
  services.journald.upload = {
    enable = true;
    settings.Upload = {
      URL = "http://127.0.0.1:${p.victorialogs}/insert/journald";
    };
  };
}
