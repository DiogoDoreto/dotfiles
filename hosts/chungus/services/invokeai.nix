{ ... }:

{
  virtualisation.oci-containers.containers.invokeai = {
    image = "ghcr.io/invoke-ai/invokeai:latest";
    ports = [ "9090:9090" ];
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

  networking.firewall.allowedTCPPorts = [ 9090 ];
}
