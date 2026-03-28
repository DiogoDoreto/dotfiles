# llama-swap on chungus Implementation Plan

> **For agentic workers:** REQUIRED: Use superpowers:subagent-driven-development (if subagents available) or superpowers:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add the `llama-swap` NixOS service to chungus to serve Qwen3.5-35B-A3B via an OpenAI-compatible API on the local network, with full CUDA acceleration.

**Architecture:** Four config file edits — enable `cudaSupport` in both `flake.nix` (home-manager pkgs) and `configuration.nix` (NixOS module pkgs), add `huggingface-hub` to home packages, and create a new `services/llama-swap.nix` module that declares the service, a `MemoryDenyWriteExecute` override (required for CUDA), and `tmpfiles` rules for the model directory.

**Tech Stack:** NixOS, nixpkgs-unstable, llama-swap, llama-cpp (CUDA), systemd tmpfiles

---

## File Map

| Action | Path | Responsibility |
|--------|------|----------------|
| Modify | `hosts/chungus/flake.nix` | Enable `cudaSupport` in pkgs-unstable (home-manager / ComfyUI) |
| Modify | `hosts/chungus/configuration.nix` | Enable `cudaSupport` in NixOS module pkgs; add import |
| Modify | `hosts/chungus/home.nix` | Add `huggingface-hub` package |
| Create | `hosts/chungus/services/llama-swap.nix` | Service declaration, tmpfiles, CUDA override |

---

## Chunk 1: NixOS config changes

### Task 1: Enable cudaSupport in flake.nix

**Files:**
- Modify: `hosts/chungus/flake.nix`

- [ ] **Step 1: Uncomment cudaSupport**

In `hosts/chungus/flake.nix`, find the `pkgs` instantiation block (around line 34) and uncomment `cudaSupport = true`:

```nix
pkgs = import nixpkgs {
  inherit system overlays;
  config = {
    allowUnfree = true;
    allowUnfreePredicate = _: true;
    cudaSupport = true;
  };
};
```

- [ ] **Step 2: Verify the flake evaluates**

```bash
cd /home/dog/projects/dotfiles/hosts/chungus
nix flake check --no-build 2>&1 | head -20
```

Expected: no evaluation errors (build errors are OK — we're not building yet).

- [ ] **Step 3: Commit**

```bash
git add hosts/chungus/flake.nix
git commit -m "chungus: enable cudaSupport for home-manager pkgs"
```

---

### Task 2: Enable cudaSupport in configuration.nix and add llama-swap import

**Files:**
- Modify: `hosts/chungus/configuration.nix`

- [ ] **Step 1: Uncomment nixpkgs.config.cudaSupport and add the import**

In `hosts/chungus/configuration.nix`:

1. Uncomment `nixpkgs.config.cudaSupport = true` (currently commented with a TODO around line 25). Remove the surrounding TODO comment.

2. Add `./services/llama-swap.nix` to the `imports` list:

```nix
imports = [
  ./hardware.nix
  ./services/llama-swap.nix
];
```

- [ ] **Step 2: Verify the flake evaluates**

```bash
cd /home/dog/projects/dotfiles/hosts/chungus
nix flake check --no-build 2>&1 | head -30
```

Expected: evaluation error about `./services/llama-swap.nix` not existing yet (file not found). This confirms the import was added correctly. If you get a different error, investigate before continuing.

- [ ] **Step 3: Commit**

```bash
git add hosts/chungus/configuration.nix
git commit -m "chungus: enable cudaSupport for NixOS pkgs, add llama-swap import"
```

---

### Task 3: Add huggingface-hub to home.nix

**Files:**
- Modify: `hosts/chungus/home.nix`

- [ ] **Step 1: Add huggingface-hub to home.packages**

In `hosts/chungus/home.nix`, add `huggingface-hub` to the packages list:

```nix
home = {
  packages = with pkgs; [
    ungoogled-chromium
    huggingface-hub
  ];
};
```

- [ ] **Step 2: Commit**

```bash
git add hosts/chungus/home.nix
git commit -m "chungus: add huggingface-hub for model downloads"
```

---

### Task 4: Create hosts/chungus/services/llama-swap.nix

**Files:**
- Create: `hosts/chungus/services/llama-swap.nix`

- [ ] **Step 1: Create the services/ directory and file**

Create `hosts/chungus/services/llama-swap.nix` with this exact content:

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

- [ ] **Step 2: Verify the flake evaluates cleanly**

```bash
cd /home/dog/projects/dotfiles/hosts/chungus
nix flake check --no-build 2>&1 | head -30
```

Expected: no evaluation errors. If there are errors, fix them before continuing.

- [ ] **Step 3: Build the NixOS system closure (optional but recommended)**

This verifies the full config builds. It will take a long time on first run due to CUDA builds — skip if you want to build directly on chungus.

```bash
nix build .#nixosConfigurations.chungus.config.system.build.toplevel --no-link 2>&1 | tail -5
```

- [ ] **Step 4: Commit**

```bash
git add hosts/chungus/services/llama-swap.nix
git commit -m "chungus: add llama-swap service for Qwen3.5-35B-A3B"
```

---

## Chunk 2: Deployment and model setup

### Task 5: Deploy to chungus

**Note:** This requires SSH access to chungus or running directly on it.

- [ ] **Step 1: Push the changes**

From your dev machine:

```bash
git push
```

- [ ] **Step 2: Deploy on chungus**

SSH into chungus and run:

```bash
ssh dog@chungus
cd ~/projects/dotfiles/hosts/chungus
git pull
sudo nixos-rebuild switch --flake .#chungus 2>&1 | tail -20
```

Expected: `activating the configuration...` followed by success. The first run will take a long time (CUDA builds from source).

- [ ] **Step 3: Verify the service is running**

```bash
systemctl status llama-swap
```

Expected: `active (running)`. The service will be running but not yet serving models (no GGUF file yet).

---

### Task 6: Download the model

- [ ] **Step 1: Download Qwen3.5-35B-A3B-Q4_K_M.gguf**

On chungus, using the now-available `huggingface-cli`:

```bash
huggingface-cli download bartowski/Qwen_Qwen3.5-35B-A3B-GGUF \
  Qwen3.5-35B-A3B-Q4_K_M.gguf \
  --local-dir /tmp/hf-download
```

- [ ] **Step 2: Move to model directory and set permissions**

```bash
sudo mv /tmp/hf-download/Qwen3.5-35B-A3B-Q4_K_M.gguf /var/lib/llama-swap/models/
sudo chmod 644 /var/lib/llama-swap/models/Qwen3.5-35B-A3B-Q4_K_M.gguf
```

- [ ] **Step 3: Restart the service to pick up the model**

```bash
sudo systemctl restart llama-swap
systemctl status llama-swap
```

Expected: `active (running)`.

---

### Task 7: Verify end-to-end

- [ ] **Step 1: Check the service health**

```bash
curl -s http://localhost:8080/v1/models | python3 -m json.tool
```

Expected: JSON response listing `qwen3.5-35b-a3b` and its alias `qwen3.5`.

- [ ] **Step 2: Send a test inference request**

```bash
curl -s http://localhost:8080/v1/chat/completions \
  -H "Content-Type: application/json" \
  -d '{
    "model": "qwen3.5-35b-a3b",
    "messages": [{"role": "user", "content": "Say hello in one word"}],
    "max_tokens": 10
  }' | python3 -m json.tool
```

Expected: JSON response with a short reply in `choices[0].message.content`.

- [ ] **Step 3: Verify GPU utilization**

```bash
nvidia-smi
```

Expected: `llama-server` process visible with ~21-22GB VRAM usage on the RTX 4090.

- [ ] **Step 4: Verify accessibility from another machine (e.g. lapdog)**

From lapdog (replace `chungus` with the actual IP if DNS isn't set up):

```bash
curl -s http://chungus:8080/v1/models
```

Expected: same JSON model listing.

---

## Troubleshooting

**Service fails to start with `EPERM`:**
The `MemoryDenyWriteExecute = lib.mkForce false` override in `llama-swap.nix` should prevent this. If it still occurs, check `journalctl -u llama-swap -n 50` for the exact error.

**VRAM OOM:**
Q4_K_M at 32K context uses ~21.6GB. If the RTX 4090 is already partially occupied (e.g. by a display), reduce context with `-c 16384` in the `cmd` string.

**Health check timeout:**
The default `healthCheckTimeout = 120` seconds should be enough for the model to load. If llama-server takes longer, increase this value.

**Model not found:**
Confirm the filename matches exactly: `Qwen3.5-35B-A3B-Q4_K_M.gguf`. Case-sensitive.
