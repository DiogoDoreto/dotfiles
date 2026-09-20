# PSVR2 on chungus — community path A

PlayStation VR2 on this machine via the community stack: **native Linux
SteamVR + Ignition + PSVR2 Toolkit**. This document is the persistent
reference for the setup; the machine-side configuration lives in
`psvr2.nix` (same directory).

Status: **Wayland direct mode verified** as of 2026-09-20. Nothing here is
Sony- or Valve-supported; the stack is explicitly experimental upstream.

Research background: `../_kb/research/psvr2-linux-desktop.org` (2026-09-20).

## Why this works on chungus

- **GPU**: RTX 4090 (PCI `1:0:0`) far exceeds Sony's PC baseline. The desktop
  monitor (HDMI) is on the Intel UHD 770, while the headset is on the RTX 4090.
  Plasma runs on the iGPU and Steam uses PRIME offload to the 4090. This keeps
  ordinary desktop work off the compute GPU without preventing KWin from
  leasing the 4090's HMD connector to SteamVR.
  - The PSVR2 adapter's DisplayPort plugs into the **4090**, not the iGPU
    (an iGPU-fed DP output is not equivalent — the Framework 16 reference
    states this explicitly).
  - **VR sessions run in Plasma Wayland.** KWin leases the HMD connector to
    SteamVR through `wp_drm_lease_device_v1`; `vrcompositor.txt` should show
    `LeaseConnector_Done`, `Acquired drm display!`, and the RTX 4090 as the GPU
    vendor. Steam and Ignition must receive the complete NVIDIA PRIME offload
    environment and Steam's Vulkan ICD list must contain only NVIDIA. Renderer
    selection by itself is insufficient. When
    the environment was incomplete, SteamVR identified the HMD with Intel PCI
    IDs while KWin leased its NVIDIA connector, producing
    `vkGetDrmDisplay failed` and `CannotDRMLeaseDisplay`. The Steam package in
    `configuration.nix` supplies the complete environment. The split-GPU
    topology was verified working on Plasma Wayland on 2026-09-20. Exposing
    both Intel and NVIDIA ICDs made the dashboard dim the scene but remain
    invisible; `vrwebhelper_main.txt` repeatedly reported
    `VROverlayError_InvalidTexture`. Restricting `VK_DRIVER_FILES` and
    `VK_ICD_FILENAMES` to NVIDIA fixed cross-process overlay texture sharing.
  - VR sessions render directly on the 4090; the desktop stays on the iGPU.
- **NVIDIA display link**: the 4000x2040 @ 119.88 Hz panel historically
  needed DSC/FEC workarounds, but only for **pre-PC firmware**. On
  PC-compatible firmware (≥ 5.00) the DP link negotiates natively and the
  proprietary NVIDIA driver handles DSC internally. NVIDIA remains the least
  documented GPU for this sink (all complete references are AMD) — this is
  the main technical unknown until the headset is actually on the card.
- **Kernel**: `linuxPackages_latest` (7.2.4 at setup time). No kernel or Mesa
  patches are needed for path A on PC firmware.
- **Mesa**: 26.2.x — not affected by the Mesa 25.3.0/25.3.1 SteamVR
  "Error 497" bug noted in the Toolkit wiki.

## Wiring

| Item | Requirement |
|---|---|
| Adapter DisplayPort | **RTX 4090** DisplayPort output (DP 1.4 cable, not included by Sony) |
| Adapter USB | USB 3.0 Type-A, **directly** — no hubs or extension cables |
| Adapter power | AC (included supply) |
| Headset | into the adapter's USB-C port |
| Controllers | Bluetooth 4.0+ (this box's `wlp5s0` combo card); pair in KDE Bluetooth: left = **PS + Create**, right = **PS + Options** |
| Audio | headset audio comes over the DP link — SteamVR/Toolkit "GPU Audio Output" = the 4090's DP endpoint (wiki says "HDMI 2", sometimes "HDMI 6") |

## What `psvr2.nix` provides

| Piece | Detail |
|---|---|
| `services.udev.packages` → `70-xrhardware.rules` | udev rules for the headset (`054c:0cde`) and both Sense controllers (`0e45`/`0e46`). Pinned to xr-hardware **main** (`143de7a`), because the nixpkgs package (1.1.1, 2024) predates the **left** controller rule added in 1.1.2 (2026-07-31) — that is the Toolkit wiki's "left controller not working" fix. The `70-` prefix matters: it must sort before the uaccess rules. |
| `environment.etc` → `vulkan/implicit_layer.d/VkLayer_steamvr_linux_fixes.json` | **SteamVRLinuxFixes** implicit Vulkan layer (v0.2.0). Patches SteamVR's vrcompositor: enables **120 Hz** on the PSVR2, fixes present-wait view lag, enables `VK_PRESENT_MODE_FIFO_LATEST_READY_EXT` (NVIDIA), fixes the Mesa 26 swapchain usage-bit no-image bug. Manifest ships with a relative `library_path`; we rewrite it to the absolute store path. |
| Pinned artifacts (in the Nix store) | Ignition **v1.1.0**, PSVR2 Toolkit **v1.0.0-experimental-2** (+ CAPIApps-Linux), SteamVRLinuxFixes **v0.2.0**, PSVR2Updater **v1.0**, PSVR2Toolkit.UnitySetup **v1.1.0** — see "Updating pinned versions". |
| `psvr2-setup` | one-shot installer: extracts Ignition to `~/.local/share/psvr2/ignition`, rewrites `#!/bin/bash` shebangs to `#!/usr/bin/env bash` (NixOS has no `/bin`) — in the Ignition scripts (the `bin/linux64` copies of `proton` and `launch_serverhelper.sh` are executed by the driver shim on every SteamVR launch) **and in Steam's own `SteamVR/bin/*.sh` scripts** (`vrpathreg.sh` et al., which `driver_install.sh` execs on the host, outside Steam's FHS sandbox), runs `install_ignition.sh` against the PSVR2 App's `SteamVR_Plug-In`, swaps the official driver for the Toolkit experimental build (renames the original to `driver_playstation_vr2_orig.dll`), repairs the shipped Sense compositor binding (PS opens the dashboard; hold right Options to recenter), registers the driver via `vrpathreg` **inside Steam's FHS sandbox** (`steam-run` — NixOS's dynamic-loader stub refuses to run Steam's native Linux binaries on the bare host), merges the Toolkit's `steamvr.vrsettings` keys, applies the `setcap` RT-priority fix, and extracts the Unity room-setup app to `~/.local/share/psvr2/unitysetup`. Re-runnable. |
| `psvr2-room-setup` | launches the Toolkit Unity room-setup app through `steam-run` with the same NVIDIA-only environment as SteamVR. A valid saved play area is required for hold-Options recentering. |
| `psvr2-firmware` | `check` prints firmware; `update` flashes V6.00 via DepotDownloader (depot 2580191, manifest 8448590652442674786) + PSVR2Updater. PSVR2Updater is a generic Linux ELF, so it runs inside the Steam FHS (`steam-run`) with its work dir under `~/.local/share/psvr2/` (the FHS shares `$HOME` but not `/tmp`). Needs `STEAM_USER`/`STEAM_PASS`. |
| `psvr2-status` | diagnostics: BT adapter + service, udev rules, Steam depot/SteamVR/Proton, Ignition shim, Toolkit driver, Sense recenter binding, saved play area, `openvrpaths.vrpath`, `steamvr.vrsettings`, NVIDIA-only Steam environment, Vulkan layer, and whether any connected DP output offers the 4000x2040 mode (i.e. the headset is on the 4090). |
| `hardware.bluetooth` (in `configuration.nix`) | BlueZ enabled, power on boot. |

## Manual steps (not automatable here)

1. **Steam side**: install in Steam — SteamVR (app 250820), the PlayStation
   VR2 App (app 2580190), Proton Experimental (newer Steam installs Proton
   into `steamapps/common/` rather than `compatibilitytools.d/`;
   `psvr2-status` checks both locations). If the Linux client refuses the
   Windows-only PSVR2 App, choose the Windows platform in the install
   dropdown (or pull the depot with DepotDownloader).
2. **Run `psvr2-setup`** (after `nixos-rebuild switch` of this generation).
3. **Firmware**: `psvr2-firmware check`. The headset must be on
   PC-compatible firmware ≥ 5.00; the community target is **V6.00
   (`0x06000102`)** for the full Toolkit feature set. If it is below, use
   `psvr2-firmware update` (non-zero brick risk) or do a one-time
   Windows/PS5 update.
4. **Pair controllers** in KDE Bluetooth (left: PS + Create, right:
   PS + Options). In SteamVR, disable "Gamepad Support" (manage add-ons) —
   the Toolkit wiki lists this under "controllers not showing up".
5. **Compositor RT priority** (as root, once — lost if SteamVR updates the
   binary): `sudo setcap CAP_SYS_NICE=eip
   ~/.local/share/Steam/steamapps/common/SteamVR/bin/linux64/vrcompositor-launcher`.
6. **Launch SteamVR once from the normal Plasma Wayland session.** Fully exit
   and restart Steam after deploying this configuration so its process receives
   the NVIDIA PRIME offload environment. Then re-run `psvr2-setup` (SteamVR
   rewrites `steamvr.vrsettings` on first launch; the re-run re-merges the
   Toolkit keys).
7. **Room setup**: in the desktop Steam client, choose **Games → Add a
   Non-Steam Game to My Library → Browse**, select
   `/run/current-system/sw/bin/psvr2-room-setup`, and add it. Open the new
   shortcut's **Properties**, rename it **PSVR2 Room Setup**, and enable
   **Include in VR Library**. It can then be launched from SteamVR's Library;
   save the play area there. Running `psvr2-room-setup` from a desktop terminal
   remains an option. Position tracking and hold-Options recentering require a
   valid saved room universe.
8. `psvr2-status` to verify, then play.

## Updating pinned versions

All versions live in `psvr2.nix`. To bump one (e.g. a new Toolkit
experimental release):

1. Get the new asset URL from the GitHub releases page.
2. Download it and compute the hash with `nix hash file <zip>` (Nix ≥ 2.28 wants
   SRI base64 hashes, not `sha256sum` hex).
3. Replace the `url` and `hash` in the corresponding `fetchurl`.
4. `nixos-rebuild switch`, then re-run `psvr2-setup`.

For the xr-hardware udev rules: `git ls-remote
https://gitlab.freedesktop.org/monado/utilities/xr-hardware.git`, then bump
`rev` and get the new `fetchFromGitLab` hash by evaluating with a dummy
hash and reading the "got" line of the error.

If SteamVRLinuxFixes moves on, re-check the manifest's `library_path`
handling in `psvr2.nix` (the absolute-path rewrite).

## Troubleshooting

Check logs first: `~/.local/share/Steam/logs/vrserver.txt`,
`vrcompositor.txt` (and `vrcompositor-linux.txt` for the layer's output).

| Symptom | Likely cause / fix |
|---|---|
| Headset not detected in SteamVR | DP not on the 4090 (`psvr2-status` shows the 4000x2040 sink); firmware < 5.00; headset not powered; check `vrserver.txt`. This is the untested NVIDIA path — if the link never negotiates, that is the thing to debug. |
| 90 Hz instead of 120 | SteamVRLinuxFixes layer not loaded — `psvr2-status` layer section; manifest at `/etc/vulkan/implicit_layer.d/`; disable switch: `DISABLE_STEAMVR_LINUX_FIXES=1`. |
| Controllers not showing | BT adapter powered + paired; xr-hardware udev rules present; SteamVR "Gamepad Support" disabled; disconnect/reconnect controllers. |
| Left controller not working | xr-hardware too old — this repo pins main, which includes the 1.1.2 left-controller rule. |
| PS button dims the scene but the dashboard is invisible | SteamVR dashboard texture sharing crossed Vulkan devices. `vrwebhelper_main.txt` shows `VROverlayError_InvalidTexture`. Fully restart Steam after deploying this configuration; its wrapper restricts both `VK_DRIVER_FILES` and `VK_ICD_FILENAMES` to the NVIDIA ICD. |
| Hold Options shows recenter feedback but does not move the origin | the PSVR2 play area is absent/empty. Run `psvr2-room-setup`, save the play area, then restart SteamVR. `psvr2-status` verifies both the binding and a non-zero play area. |
| Controller tracking jitter / drops | BlueZ poll-rate limitation (kernel has no BT "sniff mode"). Known limitation of every distro; a community BlueZ patch exists but is not upstreamed. Lower poll expectations until it lands. |
| SteamVR Error 497 (failed to connect to window manager) | usually the Mesa 25.3.0/25.3.1 bug (not our case) or a compositor without DRM leasing — KWin on Plasma 6 supports it. |
| Compositor priority / stutter | re-apply `sudo setcap CAP_SYS_NICE=eip ~/.local/share/Steam/steamapps/common/SteamVR/bin/linux64/vrcompositor-launcher` (lost if SteamVR updates the binary). |
| SteamVR asks "requires superuser access to finish setup" / "setup is incomplete" | SteamVR's root helper is trying to apply `CAP_SYS_NICE` to `vrcompositor-launcher` itself, and its setuid helper doesn't work on NixOS. Apply the capability manually (manual step 5) — the prompt goes away once the capability is in place. |
| Compositor dies with `vkGetDrmDisplay failed` / `VRInitError_Compositor_CannotDRMLeaseDisplay` | the compositor could not match the leased PSVR2 connector to its Vulkan GPU. On this dual-GPU machine, first run `psvr2-status`: Steam must have `__NV_PRIME_RENDER_OFFLOAD=1` and NVIDIA-only `VK_DRIVER_FILES`, the session must be Wayland, and the sink must be on the 4090. Fully exit and restart Steam after a system rebuild. A failing run showed `HMD deviceUUID is 4a7808086` (Intel `8086:a780`) despite the sink being on NVIDIA; the complete offload environment changed this to `Acquired drm display!` and direct mode. Also verify firmware ≥ 5.00 (`psvr2-firmware check`; this box had `0x07000102` on 2026-09-20) and that `vulkaninfo --summary` lists the 4090. |
| "bad interpreter: /bin/bash" from `psvr2-setup` or at driver launch | the Ignition and SteamVR scripts' shebangs point at `/bin/bash`, absent on NixOS. `psvr2-setup` rewrites them to `#!/usr/bin/env bash` — re-run it after redeploying (also after Steam updates the PSVR2 App depot or SteamVR itself, which re-extract those files). |
| "Could not start dynamically linked executable" / "NixOS cannot run dynamically linked executables" from `vrpathreg`, `vrcompositor`, … | Steam's native Linux binaries need the FHS environment Steam's sandbox provides. Run them via `steam-run <cmd>` (what `psvr2-setup` does for driver registration) or through the Steam client — never directly from a bare shell. |
| No image in headset on Mesa | the layer fixes the Mesa 26 swapchain usage-bit bug — make sure the layer loads (NVIDIA is affected less, but keep the layer on). |
| No headset audio | Toolkit "GPU Audio Output Profile" → the 4090's DP endpoint ("HDMI 2", sometimes "HDMI 6"). |
| SteamVR async reprojection | **Does not work on NixOS + NVIDIA** — the enabling kernel patch is AMD-only. Expect the pacing ceiling; there is no NixOS-side fix. |

## Known limitations (accept, don't fight)

- **Experimental stack**: Toolkit v1.0.0-experimental-2 labels itself
  experimental ("things will break"); Ignition 1.1.0 is weeks old.
- **No Sony app**: firmware, settings, play-area management via the official
  app are unavailable on Linux (hence PSVR2Updater + UnitySetup).
- **SteamVR Room View**: unsupported or unstable under Ignition.
- **SteamVR on Linux** is a Valve development release with limited hardware
  support in general.
- **Async reprojection** unavailable on NixOS+NVIDIA (above).
- **Toolkit extra features** (eye-tracking camera feed, headset haptics)
  need the `vr2jb` jailbreak, which is **not persistent** (re-run after
  every power-off) and carries bricking risk. Not required for basic use.
- **VR vs AI workloads**: the 4090 is also the box's compute card — unload
  big models before VR sessions (autosuspend's GpuActivity check already
  keeps the box awake while the GPU is busy).

## Sources

- Toolkit Linux guide (edited 2026-09-05): <https://github.com/BnuuySolutions/PSVR2Toolkit/wiki/Linux-support>
- Ignition + architecture: <https://github.com/BnuuySolutions/Ignition>
- SteamVRLinuxFixes: <https://github.com/BnuuySolutions/SteamVRLinuxFixes>
- PSVR2Updater: <https://github.com/RealSupremium/PSVR2Updater>
- Linux VR Adventures wiki — PSVR2 + NixOS pages (2026-08-22): <https://wiki.vronlinux.org/docs/hardware/psvr2>, <https://wiki.vronlinux.org/docs/distros/nixos>
- Framework 16 worked example (AMD reference): <https://github.com/Villagers654/framework-16-linux-psvr2>
- Native driver display notes (DSC/FEC, NVIDIA status): <https://github.com/unterschall/psvr2-linux-adapter/blob/main/docs/display.md>
- xr-hardware repo: <https://gitlab.freedesktop.org/monado/utilities/xr-hardware>
