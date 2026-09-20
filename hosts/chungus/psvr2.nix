# PSVR2 (PlayStation VR2) on chungus — community path A.
#
# Stack: native Linux SteamVR + Ignition (runs Sony's Windows OpenVR driver
# under Proton/Wine) + the PSVR2 Toolkit experimental driver build.
#
# Persistent documentation, wiring, firmware and troubleshooting: PSVR2.md
# (same directory).
#
# This module provides everything NixOS itself has to supply for the stack:
#   - udev rules for the headset and both Sense controllers (xr-hardware,
#     pinned newer than the nixpkgs package, which lacks the left-controller
#     rule)
#   - the SteamVRLinuxFixes Vulkan layer as a system-wide implicit layer
#     (enables 120 Hz, present-wait, NVIDIA FIFO_LATEST_READY)
#   - the pinned community artifacts (Ignition, Toolkit, CAPI libs, firmware
#     updater, room-setup app)
#   - user commands: psvr2-setup, psvr2-room-setup, psvr2-firmware,
#     psvr2-status
#
# What this module does NOT do (manual / Steam-side, see PSVR2.md):
#   - installing Steam, SteamVR, Proton Experimental and the PSVR2 App depot
#   - pairing the controllers over Bluetooth (hardware.bluetooth is enabled
#     in configuration.nix)
#   - physical wiring: the adapter's DisplayPort must go to the RTX 4090
#     (the GPU that renders VR), USB 3.0 Type-A straight, AC power
#   - headset firmware (target: PC-compatible 6.00)
{
  lib,
  pkgs,
  ...
}:

let
  # ---------------------------------------------------------------------------
  # Pinned community artifacts.
  #
  # Update procedure (bump tag, re-hash, re-run psvr2-setup) in PSVR2.md.
  # ---------------------------------------------------------------------------

  ignition = pkgs.fetchurl {
    url = "https://github.com/BnuuySolutions/Ignition/releases/download/v1.1.0/Ignition-Linux-Windows.zip";
    hash = "sha256-Jje7MsPp1M4vjYr08eqn8q/+TE9USGiYi6T9fang9+w=";
  };

  # Toolkit experimental driver build that runs the PSVR2 driver under
  # Wine/Proton (required for Ignition).
  psvr2tk-ignition = pkgs.fetchurl {
    url = "https://github.com/BnuuySolutions/PSVR2Toolkit/releases/download/v1.0.0-experimental-2/PSVR2TK-win64-Ignition.zip";
    hash = "sha256-QnRiAVeZzyCyL/vXgQfACVVuuLFDm0R6Zo+7e1ZLDdE=";
  };

  # Linux CAPI companion libraries (loader, IPC server, SDL) used by the
  # Toolkit's Linux apps (e.g. the Unity room-setup app).
  psvr2tk-capi-linux = pkgs.fetchurl {
    url = "https://github.com/BnuuySolutions/PSVR2Toolkit/releases/download/v1.0.0-experimental-2/CAPIApps-Linux.zip";
    hash = "sha256-SqEV2yP84p0iPGCH8AVoAoLwQq614/QOpQKl79GmRcc=";
  };

  # Vulkan layer that patches SteamVR's vrcompositor for wired headsets:
  # PSVR2 120 Hz, WaitForPendingPresent fix, NVIDIA FIFO_LATEST_READY,
  # Mesa 26 swapchain usage-bit fix.
  steamvr-fixes = pkgs.fetchurl {
    url = "https://github.com/BnuuySolutions/SteamVRLinuxFixes/releases/download/v0.2.0/VK_LAYER_BNUUY_steamvr_linux_fixes.zip";
    hash = "sha256-LHHDPVu1cy+Q5Mzt0y00sWihDOEXgG4qzGoB/NIWUw4=";
  };

  # Community firmware updater (the official Sony app does not run on Linux).
  psvr2-updater = pkgs.fetchurl {
    url = "https://github.com/RealSupremium/PSVR2Updater/releases/download/v1.0/build-release-ubuntu-latest.zip";
    hash = "sha256-HHi5In5YzPq8xhX0yXlPPQRI6fCuktfBN57pDJ/Lflo=";
  };

  # Unity room-setup app (position tracking requires a saved play area).
  unity-setup = pkgs.fetchurl {
    url = "https://github.com/BnuuySolutions/PSVR2Toolkit.UnitySetup/releases/download/v1.1.0/PSVR2Toolkit.UnitySetup-Linux.zip";
    hash = "sha256-XiI3c+ZhX3G5SWOTrp4ddN9DyrKjDmfhobOJ5MyK19o=";
  };

  # ---------------------------------------------------------------------------
  # udev rules
  #
  # nixpkgs pins xr-hardware 1.1.1 (2024), which predates the left PSVR2
  # Sense controller rule (added in 1.1.2, 2026-07-31). Pin main directly
  # with the same fetch mechanism nixpkgs uses.
  # ---------------------------------------------------------------------------

  xrhardware = pkgs.fetchFromGitLab {
    domain = "gitlab.freedesktop.org";
    owner = "monado/utilities";
    repo = "xr-hardware";
    rev = "143de7a51d307297244769d51d6401897fce2abe";
    hash = "sha256-PiKvm9MlUlHbUcHA9uHuhZ4ixl5TlCQ90ctx73+SWqg=";
  };

  # Package that exposes the rules file under etc/udev/rules.d so that
  # services.udev.packages keeps the canonical 70- prefix — the file's own
  # header requires it to sort before the uaccess rules (number < 73).
  xrhardware-udev = pkgs.stdenvNoCC.mkDerivation {
    pname = "xr-hardware-psvr2";
    version = "143de7a";
    src = xrhardware;
    dontConfigure = true;
    dontBuild = true;
    installPhase = ''
      mkdir -p $out/etc/udev/rules.d
      cp 70-xrhardware.rules $out/etc/udev/rules.d/
    '';
  };

  # ---------------------------------------------------------------------------
  # SteamVRLinuxFixes Vulkan layer
  # ---------------------------------------------------------------------------

  steamvr-linux-fixes = pkgs.stdenvNoCC.mkDerivation {
    pname = "steamvr-linux-fixes";
    version = "0.2.0";
    src = steamvr-fixes;
    nativeBuildInputs = [ pkgs.unzip ];
    # stdenv has no built-in zip unpacker; unzip manually in installPhase.
    dontUnpack = true;
    dontConfigure = true;
    dontBuild = true;
    installPhase = ''
      mkdir -p $out/lib
      unzip -oq "$src" -d stage
      install -m 755 stage/libsteamvr_linux_fixes.so $out/lib/
    '';
  };

  # ---------------------------------------------------------------------------
  # User commands
  #
  # ''\${...} is bash (escaped from Nix interpolation); ${...} is Nix.
  # ---------------------------------------------------------------------------

  scripts = {
    psvr2-setup = pkgs.writeText "psvr2-setup" ''
      #!/usr/bin/env bash
      set -euo pipefail

      # One-shot (re-runnable) installer for the PSVR2 Toolkit + Ignition
      # stack. All pinned artifacts come from the Nix store (psvr2.nix);
      # nothing is downloaded at runtime.

      # Rewrite '#!/bin/bash' shebangs to '#!/usr/bin/env bash': NixOS has
      # no /bin. Applied to the extracted Ignition scripts (the copies in
      # bin/linux64 are executed by the driver shim on every SteamVR launch)
      # and to Steam's own SteamVR scripts, which this script executes on
      # the host, outside Steam's FHS sandbox.
      fix_shebangs() {
        local f
        for f in "$1"/*.sh "$1/proton"; do
          [ -f "$f" ] || continue
          sed -i '1s|^#! */bin/bash.*$|#!/usr/bin/env bash|' "$f"
        done
      }

      STEAM_CANDIDATES=(
        "$HOME/.local/share/Steam"
        "$HOME/.steamroot/.local/share/Steam"
        "$HOME/.steam/steam"
      )

      steam_dir=""
      for d in ''\${STEAM_CANDIDATES[@]}; do
        if [ -d "$d/steamapps/common/PlayStation VR2 App" ]; then
          steam_dir="$d"
          break
        fi
      done
      if [ -z "$steam_dir" ]; then
        echo "error: PlayStation VR2 App depot not found." >&2
        echo "       Install it from Steam (app id 2580190). On a Linux client" >&2
        echo "       you may need to pick the Windows platform in the install dialog." >&2
        exit 1
      fi

      plug_in="$steam_dir/steamapps/common/PlayStation VR2 App/SteamVR_Plug-In"
      if [ ! -f "$plug_in/driver.vrdrivermanifest" ]; then
        echo "error: $plug_in/driver.vrdrivermanifest missing" >&2
        exit 1
      fi
      echo "Steam dir: $steam_dir"
      echo "Driver:    $plug_in"

      # 1. Ignition — must live where the Steam Linux Runtime container can
      #    see it.
      ignition_dir="$HOME/.local/share/psvr2/ignition"
      rm -rf "$ignition_dir"
      mkdir -p "$ignition_dir"
      unzip -oq ${ignition} -d "$ignition_dir"
      fix_shebangs "$ignition_dir"
      "$ignition_dir/install_ignition.sh" "$plug_in"
      # install_ignition.sh copies proton/launch_serverhelper.sh/
      # driver_*.sh into bin/linux64 — fix the copies too.
      fix_shebangs "$plug_in/bin/linux64"

      # 2. Swap the official Sony driver for the PSVR2 Toolkit experimental
      #    build, plus the Linux CAPI companion libraries (both flat into
      #    bin/win64).
      win64="$plug_in/bin/win64"
      if [ -f "$win64/driver_playstation_vr2.dll" ] && [ ! -f "$win64/driver_playstation_vr2_orig.dll" ]; then
        mv "$win64/driver_playstation_vr2.dll" "$win64/driver_playstation_vr2_orig.dll"
      fi
      unzip -oq ${psvr2tk-ignition} -d "$win64"
      unzip -oq ${psvr2tk-capi-linux} -d "$win64"

      # 3. Repair the Toolkit's compositor binding. Its shipped binding uses
      #    an obsolete power-menu action and leaves Options unbound. Match the
      #    current Valve system-button shape and use right Options for quick
      #    recentering.
      sense_binding="$plug_in/resources/input/vrcompositor_binding_playstation_vr2_sense_controller.json"
      if [ ! -f "$sense_binding" ]; then
        echo "error: Sense controller compositor binding missing: $sense_binding" >&2
        exit 1
      fi
      jq '
        .bindings["/actions/system"].sources |= map(
          if (.path == "/user/hand/left/input/left_ps" or .path == "/user/hand/right/input/right_ps") then
            .mode = "button"
            | .inputs = {
                "click": {"output": "/actions/system/in/ToggleDashboard"},
                "held": {"output": "/actions/system/in/SystemButtonChord"}
              }
          else . end
        )
        | .bindings["/actions/quickrecenter"] = {
            "sources": [{
              "inputs": {"long": {"output": "/actions/quickrecenter/in/Recenter"}},
              "parameters": {"long_press_delay": 1.0, "long_press_expiry": 4.0},
              "mode": "button",
              "path": "/user/hand/right/input/options"
            }]
          }
      ' "$sense_binding" > "$sense_binding.tmp"
      mv "$sense_binding.tmp" "$sense_binding"

      # 4. Register the Ignition shim as an external SteamVR driver
      #    (runs vrpathreg adddriver internally).
      # Steam's own SteamVR scripts (vrpathreg.sh et al.) also assume
      # '#!/bin/bash'. driver_install.sh execs them on the host, outside
      # Steam's FHS sandbox, so fix every candidate path it checks (plus
      # this script's own Steam dir, in case the depot tree differs).
      for cand in "$HOME/.steam/steam" \
                 "$HOME/.local/share/Steam" \
                 "$HOME/.steam/root" \
                 "$steam_dir"; do
        fix_shebangs "$cand/steamapps/common/SteamVR/bin"
        fix_shebangs "$cand/steamapps/common/SteamVR/bin/linux64"
      done
      # The vrpathreg *binary* is a generic Linux ELF: NixOS's
      # dynamic-loader stub refuses to run it on the bare host. Run
      # driver_install.sh inside Steam's FHS environment via steam-run
      # (the same mechanism Steam uses to launch its own apps).
      if ! command -v steam-run > /dev/null 2>&1; then
        echo "error: steam-run not found in PATH." >&2
        echo "       vrpathreg must run inside the Steam FHS sandbox" >&2
        echo "       (NixOS cannot run Steam's native Linux binaries directly)." >&2
        exit 1
      fi
      steam-run "$plug_in/bin/linux64/driver_install.sh"

      # 5. Toolkit-recommended SteamVR settings.
      vrsettings="$steam_dir/config/steamvr.vrsettings"
      mkdir -p "$steam_dir/config"
      [ -f "$vrsettings" ] || echo '{}' > "$vrsettings"
      jq '.steamvr.enableLinuxVulkanAsync = true | .steamvr.useFacetRenderer = true' \
        "$vrsettings" > "$vrsettings.tmp"
      mv "$vrsettings.tmp" "$vrsettings"
      echo "steamvr.vrsettings updated (verify with psvr2-status after the first SteamVR launch)"

      # 6. RT priority for the compositor (NixOS: the normal setcap route
      #    does not work, see PSVR2.md).
      launcher="$steam_dir/steamapps/common/SteamVR/bin/linux64/vrcompositor-launcher"
      if [ -f "$launcher" ]; then
        if [ "$(id -u)" = "0" ]; then
          setcap CAP_SYS_NICE=eip "$launcher"
        else
          echo "note: run as root to set compositor RT priority:"
          echo "     sudo setcap CAP_SYS_NICE=eip $launcher"
        fi
      else
        echo "note: SteamVR is not installed yet — install it (app 250820), then re-run psvr2-setup"
      fi

      # 7. Room-setup app (position tracking requires a saved play area).
      unity_dir="$HOME/.local/share/psvr2/unitysetup"
      rm -rf "$unity_dir"
      mkdir -p "$unity_dir"
      unzip -oq ${unity-setup} -d "$unity_dir"
      chmod +x "$unity_dir/PSVR2Toolkit.UnitySetup.x86_64"

      echo
      echo "Setup complete. Next steps:"
      echo "  1. Install SteamVR + Proton Experimental in Steam (if not done)."
      echo "  2. Power on the headset (adapter DP on the RTX 4090, USB 3.0, AC power)."
      echo "  3. Pair both Sense controllers (KDE Bluetooth: PS+Create / PS+Options)."
      echo "  4. psvr2-firmware check   # needs PC-compatible firmware >= 5.00"
      echo "  5. Launch SteamVR once from Plasma Wayland, re-run psvr2-setup (settings re-merge)."
      echo "  6. Add /run/current-system/sw/bin/psvr2-room-setup to Steam as a non-Steam game."
      echo "     Name it PSVR2 Room Setup and enable Include in VR Library."
      echo "  7. Launch it from SteamVR, save the play area, then run psvr2-status."
    '';

    psvr2-room-setup = pkgs.writeText "psvr2-room-setup" ''
      #!/usr/bin/env bash
      set -euo pipefail

      app="$HOME/.local/share/psvr2/unitysetup/PSVR2Toolkit.UnitySetup.x86_64"
      if [ ! -x "$app" ]; then
        echo "error: room-setup app missing — run psvr2-setup first" >&2
        exit 1
      fi

      # A shell-launched Unity app does not inherit Steam's package wrapper.
      # Give it the same GPU and ICD selection as SteamVR so its submitted
      # scene texture is shareable with the NVIDIA compositor.
      export __NV_PRIME_RENDER_OFFLOAD=1
      export __NV_PRIME_RENDER_OFFLOAD_PROVIDER=NVIDIA-G0
      export __GLX_VENDOR_LIBRARY_NAME=nvidia
      export __VK_LAYER_NV_optimus=NVIDIA_only
      export VK_DRIVER_FILES=/run/opengl-driver/share/vulkan/icd.d/nvidia_icd.json
      export VK_ICD_FILENAMES="$VK_DRIVER_FILES"
      exec steam-run "$app"
    '';

    psvr2-firmware = pkgs.writeText "psvr2-firmware" ''
      #!/usr/bin/env bash
      set -euo pipefail

      # PSVR2 headset firmware via the community PSVR2Updater (the official
      # Sony app does not run on Linux). Target: V6.00 (0x06000102).
      #
      #   psvr2-firmware check    print current firmware version
      #   psvr2-firmware update   flash V6.00 — needs STEAM_USER / STEAM_PASS
      #
      # Non-zero risk of bricking the headset; see PSVR2.md.

      # Work under $HOME (not /tmp): the FHS sandbox shares $HOME but not
      # /tmp, and PSVR2Updater runs inside it (below).
      dir="$(mktemp -d "$HOME/.local/share/psvr2/updater.XXXXXX")"
      trap 'rm -rf "$dir"' EXIT
      cd "$dir"
      unzip -oq ${psvr2-updater}
      chmod +x PSVR2Updater

      # PSVR2Updater is a generic Linux ELF: NixOS's dynamic-loader stub
      # refuses to run it on the bare host — run it inside the Steam FHS
      # via steam-run (same as the vrpathreg registration in psvr2-setup).
      if ! command -v steam-run > /dev/null 2>&1; then
        echo "error: steam-run not found in PATH." >&2
        exit 1
      fi

      case ''\${1:-check} in
        check)
          steam-run "$dir/PSVR2Updater"
          ;;
        update)
          user=''\${STEAM_USER:?export STEAM_USER before running psvr2-firmware update}
          pass=''\${STEAM_PASS:?export STEAM_PASS before running psvr2-firmware update}
          depotdownloader -app 2580190 -depot 2580191 -manifest 8448590652442674786 \
            -username "$user" -password "$pass"
          cup="$(find depots -name 'HMD2_FIRMWARE.CUP' | head -n1)"
          if [ -z "$cup" ]; then
            echo "error: HMD2_FIRMWARE.CUP not found in depot download" >&2
            exit 1
          fi
          steam-run "$dir/PSVR2Updater" "$cup"
          ;;
        *)
          echo "usage: psvr2-firmware [check|update]" >&2
          exit 1
          ;;
      esac
    '';

    psvr2-status = pkgs.writeText "psvr2-status" ''
      #!/usr/bin/env bash
      set -u

      # Health check for the PSVR2 stack (community path A). Run after
      # psvr2-setup and whenever something looks wrong.

      fails=0
      ok() { printf '  ok   %s\n' "$1"; }
      bad() { printf '  FAIL %s\n' "$1"; fails=$((fails + 1)); }

      echo "== Bluetooth"
      if [ -d /sys/class/bluetooth ] && [ -n "$(ls -A /sys/class/bluetooth 2>/dev/null)" ]; then
        ok "adapter present: $(ls /sys/class/bluetooth)"
      else
        bad "no BT controller visible under /sys/class/bluetooth"
      fi
      if [ "$(systemctl is-active bluetooth 2>/dev/null)" = "active" ]; then
        ok "bluetooth.service active"
      else
        bad "bluetooth.service not active"
      fi

      echo "== udev rules"
      if grep -q 'idProduct}=="0cde"' /etc/udev/rules.d/70-xrhardware.rules 2>/dev/null; then
        ok "70-xrhardware.rules installed (PSVR2 + Sense entries)"
      else
        bad "70-xrhardware.rules missing or incomplete"
      fi

      echo "== Steam"
      steam_dir=""
      for d in "$HOME/.local/share/Steam" "$HOME/.steamroot/.local/share/Steam" "$HOME/.steam/steam"; do
        if [ -d "$d/steamapps/common" ]; then
          steam_dir="$d"
          break
        fi
      done
      if [ -z "$steam_dir" ]; then
        bad "Steam data dir not found"
      else
        ok "Steam at $steam_dir"
        if [ -d "$steam_dir/steamapps/common/PlayStation VR2 App" ]; then
          ok "PSVR2 App depot installed"
        else
          bad "PSVR2 App depot missing (Steam app 2580190)"
        fi
        if [ -d "$steam_dir/steamapps/common/SteamVR" ]; then
          ok "SteamVR installed"
        else
          bad "SteamVR missing (Steam app 250820)"
        fi
        # Newer Steam installs Proton into steamapps/common; older layouts
        # use compatibilitytools.d. Ignition's proton script auto-detects both.
        proton_dir=""
        for cand in "$steam_dir/steamapps/common/Proton - Experimental" \
                    "$steam_dir/compatibilitytools.d/Proton - Experimental"; do
          if [ -x "$cand/proton" ]; then
            proton_dir="$cand"
            break
          fi
        done
        if [ -n "$proton_dir" ]; then
          ok "Proton Experimental installed"
        else
          bad "Proton Experimental missing (steamapps/common or compatibilitytools.d)"
        fi
        plug_in="$steam_dir/steamapps/common/PlayStation VR2 App/SteamVR_Plug-In"
        if [ -e "$plug_in/bin/linux64/driver_playstation_vr2.so" ]; then
          ok "Ignition shim present (bin/linux64)"
        else
          bad "Ignition not installed — run psvr2-setup"
        fi
        if [ -f "$plug_in/bin/win64/psvr2_toolkit_capi.dll" ]; then
          ok "PSVR2 Toolkit driver in place"
        else
          bad "Toolkit driver missing — run psvr2-setup"
        fi
        if [ -f "$HOME/.config/openvr/openvrpaths.vrpath" ]; then
          if grep -q "PlayStation VR2 App" "$HOME/.config/openvr/openvrpaths.vrpath"; then
            ok "driver registered in openvrpaths.vrpath"
          else
            bad "driver not in openvrpaths.vrpath — run psvr2-setup"
          fi
        else
          bad "openvrpaths.vrpath missing — launch SteamVR once"
        fi
        if [ -f "$steam_dir/config/steamvr.vrsettings" ]; then
          if grep -q '"useFacetRenderer"' "$steam_dir/config/steamvr.vrsettings"; then
            ok "steamvr.vrsettings has the Toolkit keys"
          else
            bad "steamvr.vrsettings missing Toolkit keys — re-run psvr2-setup"
          fi
        else
          bad "steamvr.vrsettings missing — launch SteamVR once, then re-run psvr2-setup"
        fi
        sense_binding="$steam_dir/steamapps/common/PlayStation VR2 App/SteamVR_Plug-In/resources/input/vrcompositor_binding_playstation_vr2_sense_controller.json"
        if [ -f "$sense_binding" ] \
          && jq -e '.bindings["/actions/quickrecenter"].sources[] | select(.path == "/user/hand/right/input/options")' "$sense_binding" > /dev/null; then
          ok "Sense binding maps right Options to quick recenter"
        else
          bad "Sense quick-recenter binding missing — re-run psvr2-setup"
        fi
        chaperone="$steam_dir/config/playstation_vr2/chaperone_info.vrchap"
        if [ -f "$chaperone" ] \
          && jq -e '.universes[] | select(.play_area[0] > 0 and .play_area[1] > 0)' "$chaperone" > /dev/null; then
          ok "saved PSVR2 play area present"
        else
          bad "PSVR2 play area missing or empty — run psvr2-room-setup"
        fi
      fi

      echo "== Vulkan layer (SteamVRLinuxFixes)"
      manifest=/etc/vulkan/implicit_layer.d/VkLayer_steamvr_linux_fixes.json
      if [ -f "$manifest" ]; then
        ok "implicit layer manifest installed"
        layer_so="$(sed -n 's/.*"library_path"[[:space:]]*:[[:space:]]*"\([^"]*\)".*/\1/p' "$manifest")"
        if [ -n "$layer_so" ] && [ -f "$layer_so" ]; then
          ok "layer library present"
        else
          bad "layer library missing ($layer_so)"
        fi
      else
        bad "implicit layer manifest missing"
      fi

      echo "== Wayland / GPU selection"
      if [ "''${XDG_SESSION_TYPE:-}" = "wayland" ]; then
        ok "running in a Wayland session"
      else
        bad "not running in a Wayland session (XDG_SESSION_TYPE=''${XDG_SESSION_TYPE:-unset})"
      fi
      steam_pid="$(pgrep -n -x steam 2>/dev/null || true)"
      if [ -n "$steam_pid" ]; then
        steam_env="$(tr '\0' '\n' < "/proc/$steam_pid/environ" 2>/dev/null || true)"
        if echo "$steam_env" | grep -qx '__NV_PRIME_RENDER_OFFLOAD=1' \
          && echo "$steam_env" | grep -q '^VK_DRIVER_FILES=.*nvidia_icd.json' \
          && ! echo "$steam_env" | grep '^VK_DRIVER_FILES=' | grep -Eq 'intel|mesa|radeon|nouveau'; then
          ok "Steam has NVIDIA PRIME offload and NVIDIA-only Vulkan ICDs"
        else
          bad "Steam lacks the NVIDIA offload environment — fully restart Steam"
        fi
      else
        ok "Steam is stopped; the next launch will receive NVIDIA offload selection"
      fi

      compositor_log="$HOME/.local/share/Steam/logs/vrcompositor.txt"
      if [ -f "$compositor_log" ]; then
        last_start="$(grep -n 'vrcompositor .* startup' "$compositor_log" | tail -n 1 | cut -d: -f1)"
        if [ -n "$last_start" ]; then
          last_run="$(tail -n "+$last_start" "$compositor_log")"
          if echo "$last_run" | grep -q 'Acquired drm display!' \
            && echo "$last_run" | grep -q 'GPU Vendor: "NVIDIA GeForce RTX 4090"'; then
            ok "latest SteamVR run acquired the HMD lease on the RTX 4090"
          elif echo "$last_run" | grep -q 'CannotDRMLeaseDisplay'; then
            bad "latest SteamVR run failed to acquire the HMD lease"
          fi
        fi
      fi

      echo "== Display (headset DP must be plugged into the RTX 4090)"
      found_sink=0
      for conn in /sys/class/drm/card[0-9]*-*; do
        [ -e "$conn/status" ] || continue
        state="$(cat "$conn/status" 2>/dev/null || true)"
        [ "$state" = "connected" ] || continue
        modes="$(cat "$conn/modes" 2>/dev/null || true)"
        if echo "$modes" | grep -q '^4000x2040'; then
          found_sink=1
          drv="$(basename "$(readlink -f "$conn/device")" 2>/dev/null || echo '?')"
          ok "PSVR2 sink on $(basename "$conn") (driver: $drv), 4000x2040 mode offered"
        fi
      done
      if [ "$found_sink" != 1 ]; then
        bad "no connected output offering 4000x2040 (headset off, or DP not on the 4090)"
      fi

      echo
      if [ "$fails" -eq 0 ]; then
        echo "All checks passed. Power on the headset and launch SteamVR."
      else
        echo "$fails check(s) failed — see hosts/chungus/PSVR2.md for troubleshooting."
      fi
      [ "$fails" -eq 0 ]
    '';
  };

  psvr2-scripts = pkgs.runCommand "psvr2-scripts" { } ''
    mkdir -p $out/bin
    install -m 755 ${scripts."psvr2-setup"} $out/bin/psvr2-setup
    install -m 755 ${scripts."psvr2-room-setup"} $out/bin/psvr2-room-setup
    install -m 755 ${scripts."psvr2-firmware"} $out/bin/psvr2-firmware
    install -m 755 ${scripts."psvr2-status"} $out/bin/psvr2-status
  '';
in
{
  # udev rules for the headset (054c:0cde) and both Sense controllers
  # (054c:0e45 left / 054c:0e46 right, over BT virtual-USB).
  services.udev.packages = [ xrhardware-udev ];

  # Implicit Vulkan layer manifest on the standard system loader search
  # path. The release manifest uses a relative library path, which only
  # resolves next to the manifest itself, hence the absolute rewrite.
  environment.etc."vulkan/implicit_layer.d/VkLayer_steamvr_linux_fixes.json".text = ''
    {
        "file_format_version": "1.1.0",
        "layer": {
            "name": "VK_LAYER_BNUUY_steamvr_linux_fixes",
            "type": "GLOBAL",
            "library_path": "${steamvr-linux-fixes}/lib/libsteamvr_linux_fixes.so",
            "api_version": "1.3.200",
            "implementation_version": "1",
            "description": "Fixes SteamVR's WaitForPendingPresent implementation and other various issues on Linux.",
            "disable_environment": {
                "DISABLE_STEAMVR_LINUX_FIXES": "Disable the SteamVR Linux Fixes layer"
            }
        }
    }
  '';

  environment.systemPackages = [
    psvr2-scripts
    pkgs.jq
    pkgs.unzip
    pkgs.depotdownloader
    pkgs.vulkan-tools
  ];
}
