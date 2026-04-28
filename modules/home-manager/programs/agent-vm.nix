# Home-Manager module for user-space agent-VM lifecycle management.
#
# Provides the `agent-vm` dispatcher command, user systemd units, and SSH
# config stanzas.  No root or system-level changes are required at runtime
# (one-time host setup of kvm module + kvm group is done manually once).
#
# See dotfiles-dog/modules/nixos/microvm-agent/README.md for the full picture.
{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.dog.programs.agent-vm;
  vmNames = builtins.attrNames cfg.vms;

  # Wrapper that fixes two issues with bare `nix run <flake>#<runner>`:
  #
  # 1. CWD fix: some flake inputs use relative paths in the lock file, which
  #    nix resolves relative to the current working directory.  Systemd user
  #    services default to $HOME as CWD, so those inputs resolve incorrectly.
  #    cd-ing to the flake directory first gives nix the right base path.
  #
  # 2. virtiofsd: `nix run .#<vm>-runner` only invokes `microvm-run` (QEMU).
  #    The runner package also ships `virtiofsd-run` (supervisord managing one
  #    virtiofsd instance per share); QEMU cannot start until those sockets
  #    exist.  This wrapper starts virtiofsd-run first, waits for the ro-store
  #    socket to appear, then launches QEMU.
  mkVmRunnerScript =
    { runnerAttr, sockName }:
    pkgs.writeShellScript "agent-${sockName}-run" ''
      set -euo pipefail
      cd ${escapeShellArg (toString cfg.flake)}
      RUNNER=$(${pkgs.nix}/bin/nix build ".?submodules=1#${runnerAttr}" --no-link --print-out-paths)
      WORKDIR="$XDG_RUNTIME_DIR/agent-vm-sockets/${sockName}"
      mkdir -p "$WORKDIR"
      cd "$WORKDIR"
      "$RUNNER/bin/virtiofsd-run" &
      VIRTIOFSD_PID=$!
      _cleanup() { kill "$VIRTIOFSD_PID" 2>/dev/null || true; }
      trap _cleanup EXIT
      i=0
      while [ "$i" -lt 150 ]; do
        [ -S "${sockName}-virtiofs-ro-store.sock" ] && break
        sleep 0.2
        i=$(( i + 1 ))
      done
      if [ "$i" -ge 150 ]; then
        echo "virtiofsd socket '${sockName}-virtiofs-ro-store.sock' did not appear after 30s" >&2
        exit 1
      fi
      "$RUNNER/bin/microvm-run"
    '';

  # Generic VM runner: takes the VM name as $1 (injected via systemd %i).
  # Cannot be generated per-VM because the template unit resolves %i at
  # runtime, not at Nix evaluation time.
  vmRunnerScript = pkgs.writeShellScript "agent-vm-run" ''
    set -euo pipefail
    NAME="$1"
    cd ${escapeShellArg (toString cfg.flake)}
    RUNNER=$(${pkgs.nix}/bin/nix build ".?submodules=1#agent-$NAME-runner" --no-link --print-out-paths)
    WORKDIR="$XDG_RUNTIME_DIR/agent-vm-sockets/$NAME"
    mkdir -p "$WORKDIR"
    cd "$WORKDIR"
    "$RUNNER/bin/virtiofsd-run" &
    VIRTIOFSD_PID=$!
    _cleanup() { kill "$VIRTIOFSD_PID" 2>/dev/null || true; }
    trap _cleanup EXIT
    i=0
    while [ "$i" -lt 150 ]; do
      [ -S "agent-$NAME-virtiofs-ro-store.sock" ] && break
      sleep 0.2
      i=$(( i + 1 ))
    done
    if [ "$i" -ge 150 ]; then
      echo "virtiofsd socket 'agent-$NAME-virtiofs-ro-store.sock' did not appear after 30s" >&2
      exit 1
    fi
    "$RUNNER/bin/microvm-run"
  '';

  routerRunnerScript = mkVmRunnerScript {
    runnerAttr = "agent-router-runner";
    sockName   = "agent-router";
  };

  agentVmScript = pkgs.writeShellScriptBin "agent-vm" ''
    CMD="''${1:-help}"
    NAME="''${2:-}"

    _start_switch() {
      systemctl --user start agent-vm-switch.service
    }

    _start_router() {
      _start_switch
      systemctl --user start agent-vm-router.service
    }

    case "$CMD" in
      start)
        [ -z "$NAME" ] && { echo "Usage: agent-vm start <name>"; exit 1; }
        _start_router
        systemctl --user start "agent-vm@''${NAME}.service"
        echo "agent-''${NAME} starting."
        echo "SSH in once ready:  ssh agent-''${NAME}"
        echo "Audit:              agent-vm logs --audit"
        echo "Stop:               agent-vm stop ''${NAME}"
        ;;

      stop)
        if [ "$NAME" = "--all" ] || [ -z "$NAME" ]; then
          systemctl --user list-units 'agent-vm@*.service' --no-legend \
            | awk '{print $1}' \
            | xargs -r systemctl --user stop 2>/dev/null || true
          systemctl --user stop agent-vm-router.service 2>/dev/null || true
          systemctl --user stop agent-vm-switch.service 2>/dev/null || true
        else
          systemctl --user stop "agent-vm@''${NAME}.service"
        fi
        ;;

      ssh)
        [ -z "$NAME" ] && { echo "Usage: agent-vm ssh <name>"; exit 1; }
        exec ssh "agent-''${NAME}"
        ;;

      status)
        systemctl --user status agent-vm-switch.service agent-vm-router.service --no-pager || true
        for name in ${escapeShellArgs vmNames}; do
          systemctl --user status "agent-vm@''${name}.service" --no-pager || true
        done
        ;;

      logs)
        LOGFLAG="''${2:-}"
        if [ "$NAME" = "--audit" ] || [ "$LOGFLAG" = "--audit" ]; then
          echo "==> DNS queries (router VM dnsmasq):"
          ssh agent-router "journalctl -u dnsmasq -g 'query' --no-pager -n 50" 2>/dev/null \
            || echo "  (router not reachable)"
          echo ""
          echo "==> Connection audit (router VM nftables [vm-agent]):"
          ssh agent-router "journalctl -k -g '\[vm-agent\]' --no-pager -n 50" 2>/dev/null \
            || echo "  (router not reachable)"
        elif [ -n "$NAME" ]; then
          journalctl --user -u "agent-vm@''${NAME}.service" -f
        else
          journalctl --user -u agent-vm-router.service -f
        fi
        ;;

      nuke)
        [ -z "$NAME" ] && { echo "Usage: agent-vm nuke <name>"; exit 1; }
        systemctl --user stop "agent-vm@''${NAME}.service" 2>/dev/null || true
        home_dir="''${XDG_DATA_HOME:-$HOME/.local/share}/agent-vm/''${NAME}/home"
        rm -rf "$home_dir"
        mkdir -p "$home_dir"
        echo "Persistent home for ''${NAME} wiped and recreated."
        ;;

      *)
        echo "agent-vm — manage isolated coding-agent VMs"
        echo ""
        echo "Usage: agent-vm <command> [name]"
        echo ""
        echo "Commands:"
        echo "  start <name>            Start switch + router (if needed) + named VM"
        echo "  stop [<name>|--all]     Stop one or all VMs"
        echo "  ssh <name>              Open SSH session (ProxyJump via router)"
        echo "  status                  Show systemd status for all units"
        echo "  logs [<name>|--audit]   Stream logs; --audit shows DNS + connections"
        echo "  nuke <name>             Wipe the VM's persistent home for a clean slate"
        echo ""
        echo "Known VMs: ${escapeShellArgs vmNames}"
        ;;
    esac
  '';
in
{
  options.dog.programs.agent-vm = {
    enable = mkEnableOption "agent-vm manager";

    flake = mkOption {
      type = types.path;
      description = "Absolute path to the repo root that contains the nixosConfigurations for the VMs.";
    };

    router = {
      enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to manage a router VM that provides NAT, DNS, and audit logging.";
      };
      sshPort = mkOption {
        type = types.port;
        default = 2200;
        description = "Host-side port forwarded to the router VM's SSH.  Must match the value set in the router VM's dog.microvm-agent.router.sshPort option.";
      };
    };

    vms = mkOption {
      type = types.attrsOf (
        types.submodule (
          { name, ... }:
          {
            options = {
              guestIp = mkOption {
                type = types.str;
                description = "Agent VM's static IP on the vde network, without CIDR (e.g. \"10.0.0.2\"). Must match net.address in the VM's config.nix (stripped of the /24 suffix).";
              };
              identityFile = mkOption {
                type = types.str;
                default = "~/.ssh/id_agent_vm";
                description = "SSH identity file for passwordless login to this VM.";
              };
            };
          }
        )
      );
      default = { };
      description = "Set of agent VMs to manage.  Each entry generates SSH config and a systemd unit.";
    };
  };

  config = mkIf cfg.enable {
    home.packages = [
      agentVmScript
      pkgs.vde2
    ];

    # Ensure SSH config is active so the matchBlocks below take effect.
    programs.ssh.enable = mkDefault true;

    # SSH stanzas:
    #   agent-router — direct via SLIRP hostfwd on localhost:<sshPort>
    #   agent-<name> — via ProxyJump through agent-router
    programs.ssh.matchBlocks =
      (optionalAttrs cfg.router.enable {
        "agent-router" = {
          hostname = "127.0.0.1";
          port = cfg.router.sshPort;
          user = "dog";
          extraOptions = {
            StrictHostKeyChecking = "no";
            UserKnownHostsFile = "/dev/null";
            IdentityFile =
              let
                keys = mapAttrsToList (_: v: v.identityFile) cfg.vms;
              in
              if keys == [ ] then "~/.ssh/id_agent_vm" else head keys;
          };
        };
      })
      // mapAttrs' (
        name: vmCfg:
        nameValuePair "agent-${name}" {
          hostname = vmCfg.guestIp;
          user = "dog";
          proxyJump = optionalString cfg.router.enable "agent-router";
          extraOptions = {
            StrictHostKeyChecking = "no";
            UserKnownHostsFile = "/dev/null";
            IdentityFile = vmCfg.identityFile;
          };
        }
      ) cfg.vms;

    # Per-VM persistent home directories (virtiofs source on the host side).
    systemd.user.tmpfiles.rules = map (
      name: "d %h/.local/share/agent-vm/${name}/home 0755 - - -"
    ) vmNames;

    systemd.user.services = {
      # Userspace L2 switch shared by all VMs.
      agent-vm-switch = {
        Unit.Description = "VDE2 userspace switch for agent VM network";
        Service = {
          ExecStart = "${pkgs.vde2}/bin/vde_switch -s %t/agent-vm-net.sock";
          Restart = "on-failure";
        };
      };

      # Template unit: `systemctl --user start agent-vm@work.service`
      # The %i specifier expands to the instance name at runtime.
      "agent-vm@" = {
        Unit = {
          Description = "Agent VM %i";
          After = if cfg.router.enable then [ "agent-vm-router.service" ] else [ "agent-vm-switch.service" ];
          Requires =
            if cfg.router.enable then [ "agent-vm-router.service" ] else [ "agent-vm-switch.service" ];
        };
        Service = {
          ExecStart = "${vmRunnerScript} %i";
          Restart = "no";
        };
      };
    }
    # Router VM service (conditional on router.enable).
    // optionalAttrs cfg.router.enable {
      agent-vm-router = {
        Unit = {
          Description = "Agent VM router (NAT gateway, DNS, audit)";
          After = [ "agent-vm-switch.service" ];
          Requires = [ "agent-vm-switch.service" ];
        };
        Service = {
          ExecStart = toString routerRunnerScript;
          Restart = "no";
        };
      };
    };
  };
}
