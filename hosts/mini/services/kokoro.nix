{ ... }:

let
  vars = import ../_variables.nix;
  p = builtins.mapAttrs (_: toString) vars.ports;
in

{
  virtualisation.oci-containers.containers.kokoro = {
    # To manually upgrade:
    #   sudo podman pull ghcr.io/remsky/kokoro-fastapi-cpu:latest
    #   sudo systemctl restart podman-kokoro.service
    image = "ghcr.io/remsky/kokoro-fastapi-cpu:latest";
    ports = [ "${p.kokoro}:8880" ];
  };
}
