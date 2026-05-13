# Home-Manager module for user-space agent-VM lifecycle management.
#
# Provides the `agent-vm` dispatcher command, user systemd units, and SSH
# config stanzas.  No root or system-level changes are required at runtime
# (one-time host setup of kvm module + kvm group is done manually once).
#
# Runners are built ahead of time and passed in via the `runner` option per
# VM (and `router.runner`).  This avoids `nix build` at service start, gives
# us proper GC roots via the home-manager generation, and lets the systemd
# ExecStart point to a fully-resolved store path.
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

  # Wrap a microvm.declaredRunner so the embedded supervisord conf no longer
  # contains `user=root` — that line is injected by microvm.nix and makes
  # supervisord refuse to start when run from a non-root user systemd
  # service.  All other binaries are passed through untouched, so callers
  # use $out/bin/virtiofsd-run and $out/bin/microvm-run as usual.
  mkPatchedRunner =
    name: runner:
    pkgs.runCommand "agent-vm-${name}-runner" { } ''
      mkdir -p $out/bin
      for f in ${runner}/bin/*; do
        ln -s "$f" "$out/bin/$(basename "$f")"
      done
      rm "$out/bin/virtiofsd-run"

      orig=${runner}/bin/virtiofsd-run
      bin=$(sed -n '2s/^exec \([^ ]*\).*/\1/p' "$orig")
      conf=$(sed -n '2s/.* --configuration \([^ ]*\).*/\1/p' "$orig")
      sed '/^user=/d' "$conf" > "$out/virtiofsd-supervisord.conf"

      cat > $out/bin/virtiofsd-run <<EOF
      #!${pkgs.runtimeShell}
      exec "$bin" --configuration "$out/virtiofsd-supervisord.conf" "\$@"
      EOF
      chmod +x $out/bin/virtiofsd-run
    '';

  vmRunners = mapAttrs (name: vm: mkPatchedRunner name vm.runner) cfg.vms;
  routerRunner = mkPatchedRunner "router" (cfg.router.runner or null);

  agentVmScript = pkgs.writeShellScriptBin "agent-vm" ''
    set -euo pipefail
    CMD="''${1:-help}"
    NAME="''${2:-}"

    list_vm_units() {
      printf '%s\n' ${concatStringsSep " " (map (n: "agent-vm-${n}.service") vmNames)}
    }

    start_infra() {
      systemctl --user start agent-vm-switch.service
      ${optionalString cfg.router.enable ''
        systemctl --user start agent-vm-router.service
      ''}
    }

    case "$CMD" in
      start)
        [ -z "$NAME" ] && { echo "Usage: agent-vm start <name>"; exit 1; }
        start_infra
        if ! systemctl --user start "agent-vm-''${NAME}.service"; then
          echo "Error: agent-''${NAME} failed to start."
          echo "Logs:  agent-vm logs ''${NAME}"
          exit 1
        fi
        echo "agent-''${NAME} starting."
        echo "SSH:    ssh agent-''${NAME}"
        echo "Logs:   agent-vm logs ''${NAME}"
        ${optionalString cfg.router.enable ''echo "Audit:  agent-vm logs --audit"''}
        echo "Stop:   agent-vm stop ''${NAME}"
        ;;

      stop)
        if [ -z "$NAME" ] || [ "$NAME" = "--all" ]; then
          list_vm_units | xargs -r systemctl --user stop 2>/dev/null || true
          ${optionalString cfg.router.enable ''
            systemctl --user stop agent-vm-router.service 2>/dev/null || true
          ''}
          systemctl --user stop agent-vm-switch.service 2>/dev/null || true
        else
          systemctl --user stop "agent-vm-''${NAME}.service"
        fi
        ;;

      ssh)
        [ -z "$NAME" ] && { echo "Usage: agent-vm ssh <name>"; exit 1; }
        shift 2 || true
        exec ssh "agent-''${NAME}" "$@"
        ;;

      status)
        units=( agent-vm-switch.service )
        ${optionalString cfg.router.enable "units+=( agent-vm-router.service )"}
        while IFS= read -r u; do units+=( "$u" ); done < <(list_vm_units)
        systemctl --user status "''${units[@]}" --no-pager || true
        ;;

      logs)
        if [ "$NAME" = "--audit" ]; then
          ${
            if cfg.router.enable then
              ''
                echo "==> DNS queries (router VM dnsmasq):"
                ssh agent-router "journalctl -u dnsmasq -g 'query' --no-pager -n 50" 2>/dev/null \
                  || echo "  (router not reachable)"
                echo ""
                echo "==> Connection audit (router VM nftables [vm-agent]):"
                ssh agent-router "journalctl -k -g '\\[vm-agent\\]' --no-pager -n 50" 2>/dev/null \
                  || echo "  (router not reachable)"
              ''
            else
              ''
                echo "router VM is disabled (set dog.programs.agent-vm.router.enable = true)"
                exit 1
              ''
          }
        elif [ -n "$NAME" ]; then
          journalctl --user -u "agent-vm-''${NAME}.service" -f
        else
          echo "Usage: agent-vm logs <name|--audit>"
          exit 1
        fi
        ;;

      nuke)
        [ -z "$NAME" ] && { echo "Usage: agent-vm nuke <name>"; exit 1; }
        systemctl --user stop "agent-vm-''${NAME}.service" 2>/dev/null || true
        home_dir="$HOME/.local/share/agent-vm/''${NAME}/home"
        rm -rf "$home_dir"
        mkdir -p "$home_dir"
        echo "Persistent home for ''${NAME} wiped and recreated."
        ;;

      *)
        cat <<'USAGE'
    agent-vm — manage isolated coding-agent VMs

    Usage: agent-vm <command> [name]

    Commands:
      start <name>            Start switch + router (if enabled) + named VM
      stop [<name>|--all]     Stop one VM, or all VMs + infra
      ssh <name> [args]       SSH into the VM (ProxyJump via router if enabled)
      status                  Show systemd status for all units
      logs <name>             Tail systemd logs for the VM
      logs --audit            Show DNS queries + connection audit (router only)
      nuke <name>             Wipe the VM's persistent home

    USAGE
        echo "Known VMs: ${concatStringsSep " " vmNames}"
        ;;
    esac
  '';
in
{
  options.dog.programs.agent-vm = {
    enable = mkEnableOption "agent-vm manager";

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
      identityFile = mkOption {
        type = types.str;
        default = "~/.ssh/id_agent_vm";
        description = "SSH identity file used to log in to the router VM (and via ProxyJump for the agent VMs).";
      };
      runner = mkOption {
        type = types.nullOr types.package;
        default = null;
        description = "microvm.declaredRunner package for the router VM.  Required when router.enable = true.";
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
              runner = mkOption {
                type = types.package;
                description = "microvm.declaredRunner package for this VM (built ahead of time by the flake).";
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
    assertions = [
      {
        assertion = !cfg.router.enable || cfg.router.runner != null;
        message = "dog.programs.agent-vm.router.runner must be set when router.enable = true.";
      }
    ];

    home.packages = [
      agentVmScript
      pkgs.vde2
    ];

    # Ensure SSH config is active so the matchBlocks below take effect.
    programs.ssh.enable = mkDefault true;

    # Opt out of home-manager's legacy programs.ssh defaults (deprecated);
    # mirror the upstream-recommended values into matchBlocks."*" below.
    programs.ssh.enableDefaultConfig = false;

    # SSH stanzas:
    #   *             — replicates the previous home-manager defaults
    #   agent-router  — direct via SLIRP hostfwd on localhost:<sshPort>
    #   agent-<name>  — via ProxyJump through agent-router
    programs.ssh.matchBlocks =
      {
        "*" = {
          forwardAgent = false;
          addKeysToAgent = "no";
          compression = false;
          serverAliveInterval = 0;
          serverAliveCountMax = 3;
          hashKnownHosts = false;
          userKnownHostsFile = "~/.ssh/known_hosts";
          controlMaster = "no";
          controlPath = "~/.ssh/master-%r@%n:%p";
          controlPersist = "no";
        };
      }
      // (optionalAttrs cfg.router.enable {
        "agent-router" = {
          hostname = "127.0.0.1";
          port = cfg.router.sshPort;
          user = "dog";
          extraOptions = {
            StrictHostKeyChecking = "no";
            UserKnownHostsFile = "~/.ssh/known_hosts_agent_vms";
            IdentityFile = cfg.router.identityFile;
          };
        };
      })
      // mapAttrs' (
        name: vmCfg:
        nameValuePair "agent-${name}" {
          hostname = vmCfg.guestIp;
          user = "dog";
          proxyJump = if cfg.router.enable then "agent-router" else null;
          extraOptions = {
            StrictHostKeyChecking = "no";
            UserKnownHostsFile = "~/.ssh/known_hosts_agent_vms";
            IdentityFile = vmCfg.identityFile;
          };
        }
      ) cfg.vms;

    # Per-VM persistent home directories (virtiofs source on the host side).
    systemd.user.tmpfiles.rules = map (
      name: "d %h/.local/share/agent-vm/${name}/home 0755 - - -"
    ) vmNames;

    systemd.user.services =
      let
        # virtiofsd unit for one VM.  Type=notify works because the
        # supervisord event handler embedded in the runner sends READY=1
        # once all virtiofsd processes are RUNNING.  PartOf makes this unit
        # stop whenever the VM unit stops.
        mkVirtiofsdUnit =
          {
            name,
            runner,
            vmUnit,
          }:
          {
            Unit = {
              Description = "virtiofsd for agent VM ${name}";
              After = [ "agent-vm-switch.service" ];
              Requires = [ "agent-vm-switch.service" ];
              PartOf = [ vmUnit ];
            };
            Service = {
              Type = "notify";
              NotifyAccess = "all";
              ExecStart = "${runner}/bin/virtiofsd-run";
              RuntimeDirectory = "agent-vm-sockets/${name}";
              WorkingDirectory = "%t/agent-vm-sockets/${name}";
              Restart = "no";
            };
          };

        # QEMU unit for one VM.  The runner's microvm-run uses *relative*
        # virtiofs socket paths, so WorkingDirectory must match the
        # virtiofsd unit's RuntimeDirectory.  BindsTo causes the VM to be
        # stopped if virtiofsd dies, avoiding zombie VMs with stale shares.
        mkVmUnit =
          {
            name,
            description,
            runner,
            extraAfter ? [ ],
            extraRequires ? [ ],
            extraBindsTo ? [ ],
          }:
          let
            virtiofsdUnit = "agent-vm-virtiofsd-${name}.service";
          in
          {
            Unit = {
              Description = description;
              After = [
                "agent-vm-switch.service"
                virtiofsdUnit
              ]
              ++ extraAfter;
              Requires = [ "agent-vm-switch.service" ] ++ extraRequires;
              BindsTo = [ virtiofsdUnit ] ++ extraBindsTo;
            };
            Service = {
              ExecStart = "${runner}/bin/microvm-run";
              WorkingDirectory = "%t/agent-vm-sockets/${name}";
              Restart = "no";
            };
          };

        switchUnit = {
          agent-vm-switch = {
            Unit.Description = "VDE2 userspace switch for agent VM network";
            Service = {
              # vde_switch runs in the foreground by default (no -d flag).
              # --nostdin prevents it from reading stdin without daemonizing.
              ExecStart = "${pkgs.vde2}/bin/vde_switch --nostdin -s %t/agent-vm-net.sock";
              # VDE2 creates a *directory* named agent-vm-net.sock/ containing
              # the actual sockets (ctl, etc.).  Wait for the ctl socket inside
              # that directory before letting dependent units start.
              ExecStartPost = "${pkgs.bash}/bin/bash -c 'until [ -S %t/agent-vm-net.sock/ctl ]; do sleep 0.05; done'";
              Restart = "on-failure";
            };
          };
        };

        agentVmUnits = lib.foldl' (
          acc: name:
          let
            runner = vmRunners.${name};
            extraAfter = optional cfg.router.enable "agent-vm-router.service";
            extraRequires = optional cfg.router.enable "agent-vm-router.service";
          in
          acc
          // {
            "agent-vm-virtiofsd-${name}" = mkVirtiofsdUnit {
              inherit name runner;
              vmUnit = "agent-vm-${name}.service";
            };
            "agent-vm-${name}" = mkVmUnit {
              inherit
                name
                runner
                extraAfter
                extraRequires
                ;
              description = "Agent VM ${name}";
            };
          }
        ) { } vmNames;

        routerUnits = optionalAttrs cfg.router.enable {
          agent-vm-virtiofsd-router = mkVirtiofsdUnit {
            name = "router";
            runner = routerRunner;
            vmUnit = "agent-vm-router.service";
          };
          agent-vm-router = mkVmUnit {
            name = "router";
            description = "Agent VM router (NAT gateway, DNS, audit)";
            runner = routerRunner;
          };
        };
      in
      switchUnit // agentVmUnits // routerUnits;
  };
}
