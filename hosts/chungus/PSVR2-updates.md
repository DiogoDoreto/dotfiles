# PSVR2 upstream update research — 2026-10-27

**Subsequent configuration change (2026-10-27):** the user accepted the CI
artifact expiry risk, and `psvr2.nix` now pins the matched Windows/Linux CI
artifacts at `a34894a`, retaining the experimental-2 Wine USB bridge. See
[`PSVR2.md`](PSVR2.md#current-toolkit-ci-pin-2026-10-27) for the current setup.
The release comparison and recommendation below are the original research
snapshot, not a statement that the configuration still uses experimental-2.

Scope: community artifacts pinned in `psvr2.nix` and their compatibility with
the stack documented in `PSVR2.md`. Research only; no pins changed, no deployment
or hardware testing performed. The xr-hardware, nixpkgs and Steam-managed
component findings below were independently verified by the coordinating agent
and supplied for inclusion; they were not re-researched here.

## Freshness and method

Queried each upstream GitHub **releases list**, including prereleases (not just
`releases/latest`), and compared its newest relevant tag with the default branch.
Each list returned fewer than the requested 100 releases, so pagination was not
needed. Dates below are GitHub `published_at` in UTC unless stated otherwise.
GitHub API responses reported `Date: Tue, 27 Oct 2026 20:42:03 GMT`: **the live
service date agrees with the environment date, 2026-10-27**. There is no observed
date discrepancy to correct or extrapolate from.

Primary release-list API endpoints:

- [Ignition](https://api.github.com/repos/BnuuySolutions/Ignition/releases?per_page=100)
- [Toolkit](https://api.github.com/repos/BnuuySolutions/PSVR2Toolkit/releases?per_page=100)
- [SteamVRLinuxFixes](https://api.github.com/repos/BnuuySolutions/SteamVRLinuxFixes/releases?per_page=100)
- [PSVR2Updater](https://api.github.com/repos/RealSupremium/PSVR2Updater/releases?per_page=100)
- [UnitySetup](https://api.github.com/repos/BnuuySolutions/PSVR2Toolkit.UnitySetup/releases?per_page=100)

## Released versions: one available upgrade

| Component | Local pin | Newest relevant release, publication time | Exact Linux-stack assets | Result |
|---|---|---|---|---|
| Ignition | `v1.1.0` | [`v1.1.0`](https://github.com/BnuuySolutions/Ignition/releases/tag/v1.1.0), 2026-09-11 02:02:19Z, not prerelease | `Ignition-Linux-Windows.zip` | Already newest release |
| PSVR2Toolkit | `v1.0.0-experimental-2` | [`v1.0.0-experimental-2`](https://github.com/BnuuySolutions/PSVR2Toolkit/releases/tag/v1.0.0-experimental-2), 2026-09-13 19:19:39Z, **prerelease** | `PSVR2TK-win64-Ignition.zip`, `CAPIApps-Linux.zip` | Already newest Linux-capable release |
| SteamVRLinuxFixes | `v0.2.0` | [`v0.2.1`](https://github.com/BnuuySolutions/SteamVRLinuxFixes/releases/tag/v0.2.1), 2026-10-06 02:21:44Z, not prerelease | `VK_LAYER_BNUUY_steamvr_linux_fixes.zip` | **Released upgrade available** |
| PSVR2Updater | `v1.0` | [`v1.0`](https://github.com/RealSupremium/PSVR2Updater/releases/tag/v1.0), 2026-03-29 22:04:40Z, not prerelease | `build-release-ubuntu-latest.zip` | Already newest release |
| PSVR2Toolkit.UnitySetup | `v1.1.0` | [`v1.1.0`](https://github.com/BnuuySolutions/PSVR2Toolkit.UnitySetup/releases/tag/v1.1.0), 2026-08-02 20:46:33Z, not prerelease | `PSVR2Toolkit.UnitySetup-Linux.zip` | Already newest release |

The pinned SteamVRLinuxFixes [`v0.2.0`](https://github.com/BnuuySolutions/SteamVRLinuxFixes/releases/tag/v0.2.0)
was published 2026-09-12 19:48:23Z. Toolkit's newest non-prerelease is
[`v0.2.1`](https://github.com/BnuuySolutions/PSVR2Toolkit/releases/tag/v0.2.1)
(2026-01-11 18:51:33Z); it is **not** a replacement for the experimental Linux
driver. Linux support arrived in
[`v1.0.0-experimental-1`](https://github.com/BnuuySolutions/PSVR2Toolkit/releases/tag/v1.0.0-experimental-1)
(2026-07-29 22:18:35Z), and the current Ignition README explicitly requires an
[experimental Toolkit release for PSVR2 under Wine](https://github.com/BnuuySolutions/Ignition#supported-drivers).

### SteamVRLinuxFixes v0.2.1: useful, narrowly scoped update

[Release notes](https://github.com/BnuuySolutions/SteamVRLinuxFixes/releases/tag/v0.2.1):
user-local installation support (root still installs system-wide), logging the
Git commit hash, disabling funchook tests, and a **potential fix for binaries
built with an executable stack** on some distributions. The
[tagged build change](https://github.com/BnuuySolutions/SteamVRLinuxFixes/commit/b95aa0d558ffece98362ca2ff1607307d817b2dd)
adds assembler/linker no-executable-stack flags. These are not claims of new
tracking, reprojection, or frame-rate fixes.

Downloaded and inspected the v0.2.1 release archive without executing it:

- Top-level files remain `install.sh`, `libsteamvr_linux_fixes.so`, and
  `VkLayer_steamvr_linux_fixes.json`; the module's current library extraction
  path still matches.
- Manifest keeps the same layer name, API version `1.3.200`, implementation
  version `1`, and `DISABLE_STEAMVR_LINUX_FIXES` switch. Its `library_path` is
  `../../../lib/libsteamvr_linux_fixes.so`. **Keep the existing absolute
  Nix-store-path rewrite** rather than copying that relative path into `/etc`.
- Observed archive SHA-256 SRI:
  `sha256-r2SaWqRbcyqimYnEAdI9kIjwE8EFwdlIk9dxkq3bbQU=`.
  This is research evidence, not an applied pin; reverify when updating.
- Asset `updated_at`: 2026-10-06 02:20:46Z.

Sources: [exact archive](https://github.com/BnuuySolutions/SteamVRLinuxFixes/releases/download/v0.2.1/VK_LAYER_BNUUY_steamvr_linux_fixes.zip),
[tagged installer](https://github.com/BnuuySolutions/SteamVRLinuxFixes/blob/v0.2.1/install.sh).
The installer change itself has little effect here because Nix installs the
library and manifest declaratively. Avoid an additional user-local installation
which could leave another layer manifest/version in the loader's search paths.
Hardware validation remains necessary after an upgrade: verify layer logs,
120 Hz, compositor stability and dashboard rendering. The upstream
[README](https://github.com/BnuuySolutions/SteamVRLinuxFixes#what-this-does-not-fix)
explicitly does not promise general frame-rate, ALVR, or Steam Link encoding fixes.

### Important fixes already present in the pins

- [Ignition v1.1.0](https://github.com/BnuuySolutions/Ignition/releases/tag/v1.1.0):
  futex signaling/direct worker notification to reduce latency; shared-memory
  cleanup; startup RPC-registration race fix (random startup crashes); null
  property-buffer handling; distortion-grid batching; room-view timestamp
  conversion; generic `wine_hidraw.reg` replacing `wine_psvr2_hidraw.reg`.
  Room-view work does not override the Toolkit wiki's instability warning.
- [Toolkit experimental-2](https://github.com/BnuuySolutions/PSVR2Toolkit/releases/tag/v1.0.0-experimental-2):
  mitigation for Sony SLAM top-camera pose errors, per-headset Room View
  calibration, chaperone fade-distance reset fix, controller-loop sleep removal
  to reduce jitter, and CAPI result/early-initialization changes. Upstream warns
  that this is experimental and CAPI remains WIP with possible breaking changes.
  Keep the Ignition driver and Linux CAPI assets coordinated rather than
  assuming arbitrary combinations work.
- [SteamVRLinuxFixes v0.2.0](https://github.com/BnuuySolutions/SteamVRLinuxFixes/releases/tag/v0.2.0):
  Facet-renderer event queries and a Steam Link crash fix in
  `WaitForPendingPresent` patching. The existing 120 Hz, present-wait,
  NVIDIA FIFO-latest-ready, Mesa 26 usage-bit, and zero-sized texture fixes
  originated in [v0.1.0](https://github.com/BnuuySolutions/SteamVRLinuxFixes/releases/tag/v0.1.0).
- [UnitySetup v1.1.0](https://github.com/BnuuySolutions/PSVR2Toolkit.UnitySetup/releases/tag/v1.1.0):
  tracking instructions/map score, refine-map without clearing play area,
  preventing false saves when tracking is unavailable, Clear Play Area and Quit
  buttons. Its [initial release](https://github.com/BnuuySolutions/PSVR2Toolkit.UnitySetup/releases/tag/v1.0.0)
  explicitly supports Linux with the driver running through Ignition.
- [Updater v1.0](https://github.com/RealSupremium/PSVR2Updater/releases/tag/v1.0)
  has no detailed release changelog. Its [README](https://github.com/RealSupremium/PSVR2Updater#usage)
  documents no-argument firmware querying and flashing a supplied `.cup` file
  with target-version display and confirmation. Updating this utility is not
  the same as updating headset firmware.

## Newer source commits: not new releases

Comparisons were made against `main`; counts refer to commits ahead of the
specified tag, not a claim that release ZIPs were rebuilt from that branch.

### Ignition: six commits after v1.1.0

[Compare](https://github.com/BnuuySolutions/Ignition/compare/v1.1.0...main),
head `6af5d9ae36a5385161d7e0cc0d0b7b08b8bc6bcc` (2026-10-09 21:08:12Z).
September 11–12 build changes force LTO off for `ignition_bridge`, remove an
assembly-source setting, then revert that removal because generated Wine
symbols failed to link. The remaining September 28/October 9 commits are README
formatting and Oasis-driver instructions, not PSVR2 runtime fixes. No compelling
released PSVR2 upgrade is hidden here; source builds must account for those
build/linking corrections.

### Toolkit: six commits after experimental-2

[Compare](https://github.com/BnuuySolutions/PSVR2Toolkit/compare/v1.0.0-experimental-2...main),
head `a34894a61c3866a880e2c85c3cc99a4216ce86cb` (2026-10-17 21:22:44Z).
Commit dates here are `commit.committer.date`, UTC. Inspected the actual patches:

| Date | Commit | Change and relevance |
|---|---|---|
| 2026-09-13 20:30:13Z | [`cf33e0d`](https://github.com/BnuuySolutions/PSVR2Toolkit/commit/cf33e0d113f336793cccad8463689d86d38cfc9b) | Fix libcrossipc ABI: apply `__cdecl` only on the Wine side, not native Linux. |
| 2026-09-13 22:55:59Z | [`5daea38`](https://github.com/BnuuySolutions/PSVR2Toolkit/commit/5daea3856fda4399bbe61b7f9bc872ea7b321a43) | Correct `libpadResendDeviceSettings` signature and pass host timestamp. |
| 2026-10-13 01:13:35Z | [`09f14e7`](https://github.com/BnuuySolutions/PSVR2Toolkit/commit/09f14e74dfd545ab044897ff3396038e11376bd8) | Simplify controller timestamp-offset filtering and use consistent host timestamps. |
| 2026-10-14 00:22:42Z | [`0ef56b0`](https://github.com/BnuuySolutions/PSVR2Toolkit/commit/0ef56b05405ac5c37c50cc9cd9c633597e8c6149) | Keep firmware-check initialization/shutdown hooks installed when gaze is disabled; only conditionally start gaze thread. |
| 2026-10-16 23:48:36Z | [`5cf1242`](https://github.com/BnuuySolutions/PSVR2Toolkit/commit/5cf1242eb3293befc73fdbf70b6d6da02c66fcdd) | Fix controllers appearing slightly left of HMD view by correcting camera-calibration focal centers. |
| 2026-10-17 21:22:44Z | [`a34894a`](https://github.com/BnuuySolutions/PSVR2Toolkit/commit/a34894a61c3866a880e2c85c3cc99a4216ce86cb) | Replace the Wine controller-poll-rate no-op stub with a hook allowing up to eight update attempts, reset on connection changes. This is an attempt to improve behavior, **not proof the BlueZ limitation is solved**. |

**Release-asset ambiguity:** the experimental-2 tag predates the first two
fixes, but `PSVR2TK-win64-Ignition.zip` was updated at **2026-09-13 23:03:12Z**,
after both; `CAPIApps-Linux.zip` was updated at **19:15:14Z**, before both.
The API timestamps alone do **not** establish which commits the binaries
contain. Do not assert that all six fixes are missing from the pinned ZIP, or
that the same-day fixes are definitely included, without binary/build provenance.
The October fixes postdate all those asset timestamps and have no newer
published release in the queried list. A source-build experiment would be a
separate, higher-risk change (ABI, controller timing, Wine/Bluetooth behavior,
and Sony-driver-offset hooks), not a routine released-artifact bump.

### No commits beyond newest release in the other three default branches

The API comparisons were `identical` / zero commits ahead:
[SteamVRLinuxFixes v0.2.1](https://github.com/BnuuySolutions/SteamVRLinuxFixes/compare/v0.2.1...main),
[Updater v1.0](https://github.com/RealSupremium/PSVR2Updater/compare/v1.0...main),
[UnitySetup v1.1.0](https://github.com/BnuuySolutions/PSVR2Toolkit.UnitySetup/compare/v1.1.0...main).
This does not rule out work in other branches or pull requests.

## Current Toolkit Linux guide and compatibility cautions

The live [Linux-support wiki](https://github.com/BnuuySolutions/PSVR2Toolkit/wiki/Linux-support)
still displays **last edited September 5, 2026, 11 revisions**. Its
[raw content](https://raw.githubusercontent.com/wiki/BnuuySolutions/PSVR2Toolkit/Linux-support.md)
was also checked. It still describes the Ignition path, not a replacement native
Linux driver. Key current instructions and limits:

- Native/non-Flatpak Steam, SteamVR, Proton Experimental and the Sony App depot;
  install on the main OS drive as expected by the scripts. Ignition's directory
  must be visible inside Steam Linux Runtime. Upstream recommends non-beta
  SteamVR initially, although troubleshooting separately suggests trying beta.
- Install the experimental driver; the [Installation wiki](https://github.com/BnuuySolutions/PSVR2Toolkit/wiki/Installation)
  explicitly asks Linux users for **both** `PSVR2TK-win64-Ignition.zip` and
  `CAPIApps-Linux.zip`. Preserve the original Sony DLL under the exact
  `driver_playstation_vr2_orig.dll` name. Its older CAPI-app subsection still
  links experimental-1, so follow the current prerequisites/release list rather
  than mixing versions based on that stale link.
- PC-compatible headset firmware **v5.00 or greater** is sufficient for basic
  Linux operation. Run UnitySetup and save a play area for position/6DOF tracking.
- Keep SteamVR `enableLinuxVulkanAsync` and `useFacetRenderer` enabled as the
  guide requests. This is **not** an upstream guarantee of working asynchronous
  reprojection on chungus's particular NixOS/NVIDIA combination.
- Disable SteamVR's Gamepad Support add-on if controllers do not appear. The
  guide still warns about left-controller udev coverage and BlueZ poll-rate
  instability, and links a [community BlueZ patch](https://github.com/RealSupremium/bluez/commit/fadd6455d4e9716b6c724681e18cfb3d7a1704e4).
  Its upstreaming status was not independently established by this research.
- Room View remains unsupported/unstable; official Sony apps remain unavailable.
  Error 497 guidance still mentions Mesa 25.3.0–25.3.1 and DRM-leasing support.
- The guide's Ignition download link still targets **v1.0.0**, older than the
  actual v1.1.0 release already pinned here. Do not downgrade to follow that link.

**Firmware risk important to the existing docs:** the current
[jailbreak wiki](https://github.com/BnuuySolutions/PSVR2Toolkit/wiki/Jailbreaking-your-headset)
requires **exactly 6.00** for jailbreak-only features, not merely PC compatibility.
It warns that recovery firmware `0x0610` (6.10), `0x07` (7.xx), or higher prevents
downgrading to 6.00. The existing `PSVR2.md` records running firmware
`0x07000102`; that alone does not establish its recovery version or downgrade
eligibility. Do not recommend running the current V6.00 flash command as a
routine package update. Query both versions and follow recovery/downgrade
requirements first if jailbreak is actually desired. The jailbreak is
nonpersistent and carries an explicit bricking warning; it is not needed for
basic use of the existing PC-compatible setup.

## Coordinating agent's independently verified findings

### xr-hardware: existing pin current, custom packaging now optional

- Upstream `git ls-remote` HEAD remains
  `143de7a51d307297244769d51d6401897fce2abe` (2026-08-09), already pinned here.
  Latest release is **1.1.2**, commit
  `ace82a207017af66fe4d93216a1f9659f2075c5f` (2026-07-31).
  Diffing `70-xrhardware.rules` from release to HEAD only adds HTC Vive Cosmos:
  **PSVR2 headset and both controllers' USB/Bluetooth rules are identical**.
  Source: [upstream repository](https://gitlab.freedesktop.org/monado/utilities/xr-hardware).
- Chungus's locked nixpkgs revision
  `02c087f3fe673c9e317f206a627cb6b910336dbe` now packages **xr-hardware 1.1.2**:
  [locked package definition](https://github.com/NixOS/nixpkgs/blob/02c087f3fe673c9e317f206a627cb6b910336dbe/pkgs/by-name/xr/xr-hardware/package.nix).
  The update landed September 28, 2026:
  [nixpkgs commit](https://github.com/NixOS/nixpkgs/commit/c9e07f8084a1ab88f6be5284cf0fb29b9960c551).
  Existing `psvr2.nix`/`PSVR2.md` claims that this lock only supplies 1.1.1
  are therefore stale.
- NixOS `services.udev.packages` preserves rule basenames from both
  `lib/udev/rules.d` and `etc/udev/rules.d`; `pkgs.xr-hardware` could replace the
  custom fetch/derivation **without losing the required `70-` prefix**.
  Source: [udev module at the locked revision](https://github.com/NixOS/nixpkgs/blob/02c087f3fe673c9e317f206a627cb6b910336dbe/nixos/modules/services/hardware/udev.nix).
  This is a possible simplification, not a newer PSVR2 ruleset or applied change.

### SteamVR / Proton / Sony App: managed by Steam, not these Nix pins

- **SteamVR stable 2.18**, released **2026-10-21**, reports Linux fixes for
  discrete/integrated GPU selection, AMD asynchronous reprojection, and an
  OpenXR launch crash when SteamVR was not already running; it also removes
  the `VK_KHR_maintenance_9` requirement. Sources:
  [official release news](https://store.steampowered.com/news/app/250820/view/1847286114029913),
  [official news API](https://api.steampowered.com/ISteamNews/GetNewsForApp/v2/?appid=250820&count=3&maxlength=5000).
  The GPU-selection fix is particularly relevant to chungus's split
  Intel/NVIDIA topology, but **do not remove tested NVIDIA-only/offload
  environment overrides without a runtime test**. The AMD reprojection fix
  is not evidence of NVIDIA support.
- [Proton Experimental's changelog](https://github.com/ValveSoftware/Proton/wiki/Changelog),
  dated **2026-10-15**, includes a controller-detection fix involving rare
  prefix corruption, but no explicit PSVR2 fix. This does not establish that
  Toolkit's Bluetooth/poll-rate limitations are resolved.
- The Sony App's latest announcement observed through its news API is
  **3.2.0, September 18, 2025**, not a new update since the documented setup.
  Source: [official Sony App news API](https://api.steampowered.com/ISteamNews/GetNewsForApp/v2/?appid=2580190&count=3&maxlength=5000).
  **No headset firmware-version conclusion follows from this announcement.**

## Recommendation

Consider **only SteamVRLinuxFixes v0.2.0 → v0.2.1** as the straightforward
released-artifact upgrade; preserve Nix's absolute manifest path and validate
on hardware. Keep the other released pins. Track Toolkit's October controller,
timestamp and gaze-disabled fixes for the next experimental release; do not
silently substitute `main` or independently mix CAPI builds. No firmware change
is justified by the release checks above. Separately, the xr-hardware custom
derivation can now be considered for simplification to the locked package, and
SteamVR 2.18 warrants split-GPU runtime testing rather than speculative removal
of working environment overrides.
