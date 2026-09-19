{ pkgs, lib, ... }:

# logs are available inside the container on /var/log/studio/access.log
#
# Reset password with:
# sudo podman exec unsloth-studio unsloth studio reset-password --username unsloth

let
  vars = import ../_variables.nix;
in
{
  virtualisation.oci-containers.containers.unsloth-studio = {
    # To upgrade: sudo podman pull docker.io/unsloth/unsloth:latest
    #             sudo systemctl restart podman-unsloth-studio.service
    image = "docker.io/unsloth/unsloth:latest";
    autoStart = false;
    ports = [
      "${toString vars.ports.unslothStudio}:${toString vars.ports.unslothStudio}"
      "${toString vars.ports.unslothApi}:${toString vars.ports.unslothApi}"
    ];
    volumes = [
      "/data/unsloth/host:/workspace/host"
      "/data/huggingface:/workspace/.cache/huggingface"
    ];
    # Create /etc/unsloth/env on chungus (not managed by Nix) with:
    #   JUPYTER_PASSWORD=your-strong-password
    environmentFiles = [ "/etc/unsloth/env" ];
    extraOptions = [
      "--device=nvidia.com/gpu=all"
    ];
  };

  systemd.tmpfiles.rules = [
    "d /data/unsloth 0755 root root -"
    "d /data/unsloth/host 0755 root root -"
    "d /data/huggingface 0755 root root -"
  ];

  networking.firewall.allowedTCPPorts = [
    vars.ports.unslothApi
    vars.ports.unslothStudio
  ];
}
