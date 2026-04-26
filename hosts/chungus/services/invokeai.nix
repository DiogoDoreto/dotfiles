{ ... }:

let
  vars = import ../_variables.nix;
in
{
  virtualisation.oci-containers.containers.invokeai = {
    # Check latest versions on https://github.com/invoke-ai/InvokeAI/releases
    # The registry does not publish stable version tags beyond v2.3.0, so
    # pinning is not viable. To manually upgrade, run on chungus (sudo
    # required — oci-containers runs as root with a separate image store):
    #   sudo podman pull ghcr.io/invoke-ai/invokeai:latest
    #   sudo systemctl restart podman-invokeai.service
    image = "ghcr.io/invoke-ai/invokeai:latest";
    ports = [ "${toString vars.ports.invokeai}:${toString vars.ports.invokeai}" ];
    volumes = [
      "/var/lib/invokeai:/invokeai"
      "/var/cache/huggingface:/invokeai/.cache/huggingface"
    ];
    environment = {
      INVOKEAI_ROOT = "/invokeai";
      HF_HOME = "/invokeai/.cache/huggingface";
    };
    extraOptions = [
      "--device=nvidia.com/gpu=all"
    ];
  };

  systemd.tmpfiles.rules = [
    "d /var/lib/invokeai 0755 root root -"
    "d /var/cache/huggingface 0755 root root -"
  ];

  networking.firewall.allowedTCPPorts = [ vars.ports.invokeai ];
}
