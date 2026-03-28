{ pkgs, lib, ... }:
let
  # pkgs.llama-cpp has CUDA support because nixpkgs.config.cudaSupport = true
  # is set in configuration.nix, which feeds the NixOS module system's pkgs.
  llama-server = lib.getExe' pkgs.llama-cpp "llama-server";
  modelDir = "/var/lib/llama-swap/models";
in
{
  services.llama-swap = {
    enable = true;
    listenAddress = "0.0.0.0";
    port = 8080;
    openFirewall = true;

    settings = {
      healthCheckTimeout = 120;
      models = {
        "qwen3.5-35b-a3b" = {
          # \${PORT} uses Nix's \${ escape (Nix manual: "To write a dollar sign
          # followed by {, you can write \${"). This produces literal ${PORT} in
          # the generated YAML, which llama-swap substitutes at runtime with the
          # actual port number.
          cmd = "${llama-server} --port \${PORT} -m ${modelDir}/Qwen3.5-35B-A3B-Q4_K_M.gguf -ngl 99 --flash-attn -c 32768 -b 512 --no-webui";
          aliases = [ "qwen3.5" ];
        };
      };
    };
  };

  # The upstream llama-swap module sets MemoryDenyWriteExecute=true.
  # CUDA requires writable+executable memory mappings for initialization.
  # This override is required for the CUDA-enabled llama-cpp build to start.
  systemd.services.llama-swap.serviceConfig.MemoryDenyWriteExecute = lib.mkForce false;

  # Create world-readable, root-owned model directory before the service starts.
  # /var is not covered by ProtectSystem=strict (only /, /usr, /boot, /etc are
  # made read-only). The DynamicUser can read world-readable files in /var/lib
  # without BindReadOnlyPaths. No StateDirectory is used here to avoid systemd's
  # DynamicUser mechanism fighting with tmpfiles over /var/lib/private ownership.
  systemd.tmpfiles.rules = [
    "d /var/lib/llama-swap 0755 root root -"
    "d /var/lib/llama-swap/models 0755 root root -"
  ];
}
