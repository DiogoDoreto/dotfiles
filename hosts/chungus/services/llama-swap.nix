{ pkgs, lib, ... }:

# Download the model with:
#   hf download unsloth/Qwen3.5-35B-A3B-GGUF \
#     --local-dir /tmp/qwen35 \
#     --include "*UD-Q4_K_XL*"
#   sudo mv /tmp/qwen35/Qwen3.5-35B-A3B-UD-Q4_K_XL.gguf /var/lib/llama-swap/models

# Memory calculator: https://www.kolosal.ai/memory-calculator

let
  llama-cpp = pkgs.llama-cpp.override { cudaSupport = true; };
  llama-server = lib.getExe' llama-cpp "llama-server";
  modelDir = "/var/lib/llama-swap/models";

  qwen35BaseFlags = [
    "--port \${PORT}"
    "-m ${modelDir}/Qwen3.5-35B-A3B-UD-Q4_K_XL.gguf"
    "--mmproj ${modelDir}/mmproj-F16.gguf"
    "-ngl 99"
    "--flash-attn on"
    "-c 32768"
    "-b 512"
    "--cache-type-k q4_0"
    "--cache-type-v q4_0"
    "--top-k 20"
    "--top-p 0.95"
    "--min-p 0.00"
  ];

  mkCmd = flags: "${llama-server} ${lib.concatStringsSep " " flags}";
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
        "qwen3.5-35b-a3b-coding" = {
          # Coding/precise tasks: lower temperature, no presence penalty
          cmd = mkCmd (
            qwen35BaseFlags
            ++ [
              "--temp 0.6"
            ]
          );
          aliases = [
            "qwen3.5"
            "qwen3.5-coding"
          ];
        };
        "qwen3.5-35b-a3b-general" = {
          # General tasks: higher temperature, presence penalty to reduce repetition
          cmd = mkCmd (
            qwen35BaseFlags
            ++ [
              "--temp 1.0"
              "--presence-penalty 1.5"
            ]
          );
          aliases = [ "qwen3.5-general" ];
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
