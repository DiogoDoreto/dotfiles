{ pkgs, lib, ... }:

# Download the model with:
#   sudo nix run nixpkgs#python3Packages.huggingface-hub -- download \
#     unsloth/gemma-4-E4B-it-GGUF \
#     --include "*Q4_K_M* mmproj*" \
#     --local-dir /var/lib/llama-swap/models/gemma4

let
  llama-cpp = pkgs.llama-cpp.override { vulkanSupport = true; };
  llama-server = lib.getExe' llama-cpp "llama-server";
  modelDir = "/var/lib/llama-swap/models";

  mkCmd = flags: "${llama-server} ${lib.concatStringsSep " " flags}";
in
{
  services.llama-swap = {
    enable = true;
    listenAddress = "localhost";
    port = 8080;

    settings = {
      healthCheckTimeout = 120;
      globalTTL = 300; # Unload models after 5 min of inactivity to free RAM
      models = {
        "gemma-4-E4B" = {
          cmd = mkCmd [
            "--port \${PORT}"
            "-m ${modelDir}/gemma4/gemma-4-E4B-it-Q4_K_M.gguf"
            "--mmproj ${modelDir}/gemma4/mmproj-BF16.gguf"
            "-ngl 99"
            "-c 131072"
            "--temp 1.0"
            "--top-p 0.95"
            "--top-k 64"
          ];
          aliases = [
            "gemma-4"
            "gemma"
          ];
        };
      };
    };
  };

  # Grant the DynamicUser access to the Intel GPU render node.
  # CacheDirectory creates /var/cache/llama-swap owned by the DynamicUser;
  # XDG_CACHE_HOME points Mesa there so Vulkan shader cache is writable.
  systemd.services.llama-swap.serviceConfig = {
    SupplementaryGroups = [ "render" "video" ];
    CacheDirectory = "llama-swap";
    Environment = "XDG_CACHE_HOME=/var/cache/llama-swap";
  };

  systemd.tmpfiles.rules = [
    "d /var/lib/llama-swap 0755 root root -"
    "d /var/lib/llama-swap/models 0755 root root -"
    "d /var/lib/llama-swap/models/gemma4 0755 root root -"
  ];
}
