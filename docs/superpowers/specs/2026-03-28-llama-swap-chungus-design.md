# llama-swap Service for chungus — Design Spec

**Date:** 2026-03-28
**Host:** chungus (i9-13900K, RTX 4090 24GB, 62GB RAM, NixOS)
**Model:** Qwen3.5-35B-A3B (Q4_K_M GGUF, ~21.6GB VRAM)

---

## Goal

Add the `llama-swap` NixOS service to chungus to serve Qwen3.5-35B-A3B via an OpenAI-compatible API, accessible from the local network.

---

## File Changes

### 1. `hosts/chungus/configuration.nix`

Two changes:

1. Uncomment `nixpkgs.config.cudaSupport = true`. This is what causes `pkgs.llama-cpp` (and all CUDA-aware packages in the NixOS module system's `pkgs`) to be built with CUDA. The `services.llama-swap` module uses this `pkgs`.

2. Add `./services/llama-swap.nix` to the `imports` list.

Remove the existing TODO comments about `cudaSupport`.

> **Known Risk — Binary cache misses:** Enabling `nixpkgs.config.cudaSupport = true` globally causes many packages to be rebuilt from source (CUDA-enabled variants are not in the public Nix binary cache). Expect a large first-build.

### 2. `hosts/chungus/flake.nix`

Uncomment `cudaSupport = true` in the nixpkgs config block. This enables CUDA for home-manager packages (`pkgs-unstable`). ComfyUI (already installed in `ai.nix`) benefits from CUDA acceleration. This change does **not** affect the NixOS module system's `pkgs` — that is controlled solely by `nixpkgs.config` in `configuration.nix` above.

```nix
config = {
  allowUnfree = true;
  allowUnfreePredicate = _: true;
  cudaSupport = true;  # uncommented
};
```

### 3. `hosts/chungus/services/llama-swap.nix` (new file in new `services/` subdirectory)

```nix
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
```

---

## llama-server Flags Rationale

| Flag | Value | Reason |
|------|-------|--------|
| `-ngl` | `99` | Full GPU offload — Q4_K_M (~21.6GB) fits in RTX 4090's 24GB VRAM |
| `--flash-attn` | — | Flash Attention 2 — significant speedup for MoE architectures |
| `-c` | `32768` | Full native context window for Qwen3.5-35B-A3B |
| `-b` | `512` | Batch size — good default for single-GPU inference |
| `--no-webui` | — | llama-swap handles the proxy; built-in UI not needed |

---

### 4. `hosts/chungus/home.nix`

Add `huggingface-hub` to `home.packages`. This provides the `huggingface-cli` tool for downloading GGUF model files.

```nix
home.packages = with pkgs; [
  ungoogled-chromium
  huggingface-hub
];
```

---

## Model Storage

Models are stored at `/var/lib/llama-swap/models/`. The directories are created by `systemd.tmpfiles.rules` on activation (owned by root, world-readable, mode 0755). The DynamicUser can read world-readable files in `/var/lib` without additional bind mounts.

**The GGUF file must be downloaded manually** — deployment prerequisite:

```bash
sudo mkdir -p /var/lib/llama-swap/models
# Download Qwen3.5-35B-A3B-Q4_K_M.gguf from one of:
#   https://huggingface.co/bartowski/Qwen_Qwen3.5-35B-A3B-GGUF
#   https://huggingface.co/unsloth/Qwen3.5-35B-A3B-GGUF
sudo chmod 644 /var/lib/llama-swap/models/Qwen3.5-35B-A3B-Q4_K_M.gguf
```

The service will start but fail health checks if the model file is absent.

---

## Network

- Listens on `0.0.0.0:8080` — accessible from local network
- Firewall opened via `openFirewall = true`
- No TLS (local network only)

---

## Out of Scope

- TLS/auth
- Additional models beyond Qwen3.5-35B-A3B
- Home-manager LLM CLI tooling
