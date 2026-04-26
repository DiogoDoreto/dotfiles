{ pkgs, lib, ... }:

let
  vars = import ../_variables.nix;

  # Unsloth Studio's bundled llama-server is CPU-only. Override it with the
  # CUDA-enabled host binary (from the llama-cpp flake input) and inject
  # -ngl 99 so all layers are offloaded to the RTX 4090.
  # The CUDA llama-server binary uses nix's ld-linux as its ELF interpreter,
  # but the container's conda/supervisor sets LD_LIBRARY_PATH to include Ubuntu
  # 22.04's libs (glibc 2.35, GCC 12 libstdc++), which get picked up before
  # the binary's RUNPATH. Prepend the correct nix store paths so the nix ld
  # finds glibc 2.42 and GCC 15 libstdc++ first.
  llama-server-gpu = pkgs.writeTextFile {
    name = "llama-server";
    executable = true;
    text = ''
      #!/bin/sh
      export LD_LIBRARY_PATH="${pkgs.llama-cpp}/lib:${pkgs.glibc}/lib:${pkgs.stdenv.cc.cc.lib}/lib''${LD_LIBRARY_PATH:+:''${LD_LIBRARY_PATH}}"
      exec ${lib.getExe' pkgs.llama-cpp "llama-server"} -ngl 99 "$@"
    '';
  };
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
      "/nix/store:/nix/store:ro"
      "${llama-server-gpu}:/home/unsloth/.unsloth/llama.cpp/build/bin/llama-server:ro"
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
