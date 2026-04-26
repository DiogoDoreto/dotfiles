{ ... }:

let
  vars = import ../_variables.nix;
in
{
  virtualisation.oci-containers.containers.unsloth-studio = {
    # To upgrade: sudo podman pull docker.io/unsloth/unsloth:latest
    #             sudo systemctl restart podman-unsloth-studio.service
    image = "docker.io/unsloth/unsloth:latest";
    ports = [
      "${toString vars.ports.unslothStudio}:${toString vars.ports.unslothStudio}"
      "${toString vars.ports.unslothApi}:${toString vars.ports.unslothApi}"
    ];
    volumes = [
      "/data/unsloth/work:/workspace/work"
    ];
    environment = {
      # Redirect HF cache into the persistent workspace volume so downloaded
      # models survive container restarts without a separate mount.
      HF_HOME = "/workspace/work/.hf";
    };
    # Create /etc/unsloth/env on chungus (not managed by Nix) with:
    #   JUPYTER_PASSWORD=your-strong-password
    environmentFiles = [ "/etc/unsloth/env" ];
    extraOptions = [
      "--device=nvidia.com/gpu=all"
    ];
  };

  systemd.tmpfiles.rules = [
    "d /data/unsloth 0755 root root -"
    "d /data/unsloth/work 0755 root root -"
  ];

  networking.firewall.allowedTCPPorts = [
    vars.ports.unslothApi
    vars.ports.unslothStudio
  ];
}
