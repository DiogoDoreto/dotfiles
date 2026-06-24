# Keep ./opencode-agent-vm.md up to date when changing this module's behavior.
{
  config,
  lib,
  options,
  pkgs,
  self ? null,
  ...
}:

with lib;

let
  cfg = config.dog.services.opencode-agent-vm;

  shareType = types.submodule {
    options = {
      tag = mkOption {
        type = types.str;
        description = "Unique virtiofs tag for the share.";
      };

      source = mkOption {
        type = types.str;
        description = "Host path to share with the guest.";
      };

      mountPoint = mkOption {
        type = types.str;
        description = "Absolute guest path where the share is mounted.";
      };

      readOnly = mkOption {
        type = types.bool;
        default = false;
        description = "Mount the share read-only inside the guest.";
      };
    };
  };

  dnsForwardZoneType = types.submodule {
    options = {
      domain = mkOption {
        type = types.str;
        description = "DNS domain suffix forwarded to a specific upstream.";
      };

      server = mkOption {
        type = types.str;
        description = "DNS server used for the domain suffix.";
      };
    };
  };

  dnsServers = (map (zone: "/${zone.domain}/${zone.server}") cfg.dnsForwardZones) ++ cfg.dnsUpstreams;

  hasHostMicrovmModule = options ? microvm && options.microvm ? vms;
  hasGuestMicrovmModule = options ? microvm && options.microvm ? shares;

  startCommand = "${pkgs.systemd}/bin/systemctl start microvm@${cfg.vmName}.service";
  stopCommand = "${pkgs.systemd}/bin/systemctl stop microvm@${cfg.vmName}.service";
  initCommand = "${pkgs.systemd}/bin/systemctl start ${cfg.vmName}-init.service";

  startScript = pkgs.writeShellScriptBin "start-opencode-agent-vm" ''
    set -euo pipefail
    sudo ${initCommand}
    sudo ${startCommand}
    echo "OpenCode agent VM starting: microvm@${cfg.vmName}.service"
    echo ""
    echo "Web UI:              http://${cfg.hostLocalAddress}:${toString cfg.hostLocalPort}"
    echo "SSH:                 opencode-agent-vm-ssh"
    echo "VM public key:       opencode-agent-vm-public-key"
    echo "DNS audit:           journalctl -u dnsmasq -g 'query'"
    echo "Connection audit:    journalctl -k -g '[${cfg.vmName}]'"
    echo "Stop:                stop-opencode-agent-vm"
  '';

  stopScript = pkgs.writeShellScriptBin "stop-opencode-agent-vm" ''
    set -euo pipefail
    sudo ${stopCommand}
    echo "OpenCode agent VM stopped."
  '';

  sshScript = pkgs.writeShellScriptBin "opencode-agent-vm-ssh" ''
    set -euo pipefail
    exec ${pkgs.openssh}/bin/ssh \
      -i ${cfg.stateDir}/ssh/host/host_to_vm \
      -o StrictHostKeyChecking=no \
      -o UserKnownHostsFile=/dev/null \
      ${cfg.guestUser}@${cfg.guestAddress} "$@"
  '';

  publicKeyScript = pkgs.writeShellScriptBin "opencode-agent-vm-public-key" ''
    set -euo pipefail
    if [ ! -f ${cfg.stateDir}/ssh/host/vm_outbound.pub ]; then
      sudo ${initCommand}
    fi
    ${pkgs.coreutils}/bin/cat ${cfg.stateDir}/ssh/host/vm_outbound.pub
  '';

  logsScript = pkgs.writeShellScriptBin "opencode-agent-vm-logs" ''
    exec ${pkgs.systemd}/bin/journalctl \
      -u ${cfg.vmName}-init.service \
      -u microvm@${cfg.vmName}.service \
      -u ${cfg.vmName}-proxy.service \
      "$@"
  '';

  persistentHomeShare = {
    proto = "virtiofs";
    tag = "agent-home";
    source = "${cfg.stateDir}/home";
    mountPoint = "/home/${cfg.guestUser}";
  };

  persistentVarLibShare = {
    proto = "virtiofs";
    tag = "agent-var-lib";
    source = "${cfg.stateDir}/var/lib";
    mountPoint = "/var/lib";
  };

  persistentVarLogShare = {
    proto = "virtiofs";
    tag = "agent-var-log";
    source = "${cfg.stateDir}/var/log";
    mountPoint = "/var/log";
  };

  guestSshShare = {
    proto = "virtiofs";
    tag = "agent-ssh";
    source = "${cfg.stateDir}/ssh/guest";
    mountPoint = "/run/opencode-agent-vm/ssh";
    readOnly = true;
  };

  userShareToMicrovmShare = share: {
    proto = "virtiofs";
    inherit (share)
      tag
      source
      mountPoint
      readOnly
      ;
  };
in
{
  options.dog.services.opencode-agent-vm = {
    enable = mkEnableOption "OpenCode agent MicroVM host integration";

    guest = {
      enable = mkEnableOption "OpenCode agent MicroVM guest runtime";

      tailscale.enable = mkEnableOption "Tailscale inside the OpenCode agent MicroVM guest";
    };

    vmName = mkOption {
      type = types.str;
      default = "opencode-agent-vm";
      description = "Name of the declarative microVM and systemd microvm@ unit.";
    };

    autostart = mkOption {
      type = types.bool;
      default = false;
      description = "Whether to start the MicroVM automatically at boot.";
    };

    flake = mkOption {
      type = types.nullOr types.unspecified;
      default = self;
      defaultText = literalExpression "self";
      description = "Flake containing nixosConfigurations.<vmName> for the guest.";
    };

    stateDir = mkOption {
      type = types.str;
      default = "/var/lib/opencode-agent-vm";
      description = "Host directory for VM-owned persistent state and credentials.";
    };

    controlUser = mkOption {
      type = types.str;
      default = "dog";
      description = "Host user allowed to start, stop, and SSH into the VM through helper commands.";
    };

    guestUser = mkOption {
      type = types.str;
      default = "agent";
      description = "Normal user inside the guest VM.";
    };

    guestUid = mkOption {
      type = types.ints.positive;
      default = 1000;
      description = "UID for the guest user.";
    };

    guestGid = mkOption {
      type = types.ints.positive;
      default = 1000;
      description = "GID for the guest user primary group.";
    };

    guestHostname = mkOption {
      type = types.str;
      default = "opencode-agent-vm";
      description = "Hostname assigned inside the guest VM.";
    };

    bridgeName = mkOption {
      type = types.str;
      default = "opencode-vm0";
      description = "Host bridge used by the VM TAP interface.";
    };

    tapName = mkOption {
      type = types.str;
      default = "opencode-agent0";
      description = "Host TAP interface created by microvm.nix for this VM.";
    };

    hostAddress = mkOption {
      type = types.str;
      default = "10.0.101.1";
      description = "IPv4 address assigned to the host bridge.";
    };

    guestAddress = mkOption {
      type = types.str;
      default = "10.0.101.2";
      description = "Static IPv4 address assigned to the guest VM.";
    };

    networkCidr = mkOption {
      type = types.str;
      default = "10.0.101.0/24";
      description = "IPv4 CIDR for NAT and audit rules covering the VM network.";
    };

    prefixLength = mkOption {
      type = types.ints.between 1 32;
      default = 24;
      description = "IPv4 prefix length for the VM network.";
    };

    guestMac = mkOption {
      type = types.str;
      default = "02:00:00:00:01:02";
      description = "MAC address assigned to the guest TAP interface.";
    };

    vcpu = mkOption {
      type = types.ints.positive;
      default = 4;
      description = "Number of virtual CPUs assigned to the VM.";
    };

    mem = mkOption {
      type = types.ints.positive;
      default = 16384;
      description = "Memory assigned to the VM, in megabytes.";
    };

    opencodePort = mkOption {
      type = types.port;
      default = 32859;
      description = "Port where opencode serve listens inside the guest.";
    };

    hostLocalAddress = mkOption {
      type = types.str;
      default = "127.0.0.1";
      description = "Host address for the local-only proxy to the guest OpenCode server.";
    };

    hostLocalPort = mkOption {
      type = types.port;
      default = 32860;
      description = "Host port for local browser access to the guest OpenCode server.";
    };

    workingDirectory = mkOption {
      type = types.str;
      default = "$HOME";
      description = "Working directory for the opencode serve systemd service inside the guest.";
    };

    shares = mkOption {
      type = types.listOf shareType;
      default = [ ];
      description = "Additional explicit host directories shared into the guest, beyond the VM-owned persistent home and SSH key shares.";
    };

    dnsUpstreams = mkOption {
      type = types.listOf types.str;
      default = [
        "1.1.1.1"
        "1.0.0.1"
      ];
      description = "Default upstream DNS servers for VM DNS queries.";
    };

    dnsForwardZones = mkOption {
      type = types.listOf dnsForwardZoneType;
      default = [ ];
      description = "Domain-specific DNS forwarding rules for VM DNS queries.";
    };

    dnsmasqResolveLocalQueries = mkOption {
      type = types.bool;
      default = false;
      description = "Whether this module should let dnsmasq alter host local DNS resolution.";
    };

    dnsmasqBindBridgeInterface = mkOption {
      type = types.bool;
      default = true;
      description = "Whether to add the VM bridge as an explicit dnsmasq interface.";
    };

    dnsmasqBindInterfaces = mkOption {
      type = types.bool;
      default = true;
      description = "Whether to make dnsmasq bind only explicitly configured interfaces and listen addresses.";
    };

    audit.enable = mkOption {
      type = types.bool;
      default = true;
      description = "Log VM DNS queries and new outbound TCP/UDP connections on the host.";
    };

    caddy = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Expose the host-local OpenCode proxy through Caddy with Authentik forward_auth.";
      };

      hostName = mkOption {
        type = types.str;
        default = "opencode.local.doreto.com.br";
        description = "Caddy virtual host for remote OpenCode access.";
      };

      authentikUrl = mkOption {
        type = types.str;
        default = "http://127.0.0.1:9000";
        description = "Base URL for the Authentik outpost used by Caddy forward_auth.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable (
      {
        assertions = [
          {
            assertion = cfg.flake != null;
            message = "dog.services.opencode-agent-vm.flake must be set when the host side is enabled.";
          }
          {
            assertion = hasHostMicrovmModule;
            message = "dog.services.opencode-agent-vm.enable requires microvm.nixosModules.host.";
          }
        ];

        environment.systemPackages = [
          startScript
          stopScript
          sshScript
          publicKeyScript
          logsScript
        ];

        boot.kernel.sysctl."net.ipv4.ip_forward" = mkDefault 1;

        networking = {
          networkmanager.unmanaged = mkAfter [
            "interface-name:${cfg.bridgeName}"
            "interface-name:${cfg.tapName}"
          ];

          bridges.${cfg.bridgeName}.interfaces = [ ];

          interfaces.${cfg.bridgeName}.ipv4.addresses = [
            {
              address = cfg.hostAddress;
              prefixLength = cfg.prefixLength;
            }
          ];

          firewall.interfaces.${cfg.bridgeName} = {
            allowedUDPPorts = [ 53 ];
            allowedTCPPorts = [ 53 ];
          };

          firewall.extraForwardRules = ''
            iifname "${cfg.bridgeName}" accept
            oifname "${cfg.bridgeName}" ct state related,established accept
          '';

          nftables.enable = true;
          nftables.tables."${cfg.vmName}-nat" = {
            family = "ip";
            content = ''
              chain postrouting {
                type nat hook postrouting priority srcnat;
                ip saddr ${cfg.networkCidr} oifname != "${cfg.bridgeName}" masquerade
              }
            '';
          };

          nftables.tables."${cfg.vmName}-audit" = mkIf cfg.audit.enable {
            family = "inet";
            content = ''
              chain vm-forward-log {
                type filter hook forward priority filter - 1;
                iifname "${cfg.bridgeName}" ip protocol tcp ct state new log prefix "[${cfg.vmName}] " level info
                iifname "${cfg.bridgeName}" ip protocol udp ct state new log prefix "[${cfg.vmName}] " level info
              }
            '';
          };
        };

        services.dnsmasq = {
          enable = true;
          resolveLocalQueries = cfg.dnsmasqResolveLocalQueries;
          settings = {
            listen-address = mkAfter [ cfg.hostAddress ];
            log-queries = cfg.audit.enable;
            no-resolv = true;
            server = mkAfter dnsServers;
          }
          // optionalAttrs cfg.dnsmasqBindInterfaces {
            bind-interfaces = true;
          }
          // optionalAttrs cfg.dnsmasqBindBridgeInterface {
            interface = mkAfter [ cfg.bridgeName ];
          };
        };

        systemd.services."${cfg.vmName}-init" = {
          description = "Initialize OpenCode agent VM state and SSH keys";
          before = [
            "microvm@${cfg.vmName}.service"
            "microvm-virtiofsd@${cfg.vmName}.service"
          ];
          requiredBy = [
            "microvm@${cfg.vmName}.service"
            "microvm-virtiofsd@${cfg.vmName}.service"
          ];
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
          };
          script = ''
            set -euo pipefail

            host_group="$(${pkgs.coreutils}/bin/id -gn ${cfg.controlUser})"

            ${pkgs.coreutils}/bin/install -d -m 0775 -o root -g kvm ${cfg.stateDir}
            ${pkgs.coreutils}/bin/install -d -m 0755 -o ${toString cfg.guestUid} -g ${toString cfg.guestGid} ${cfg.stateDir}/home
            ${pkgs.coreutils}/bin/install -d -m 0755 -o root -g root ${cfg.stateDir}/var/lib
            ${pkgs.coreutils}/bin/install -d -m 0755 -o root -g systemd-journal ${cfg.stateDir}/var/log
            ${pkgs.coreutils}/bin/install -d -m 0700 -o ${cfg.controlUser} -g "$host_group" ${cfg.stateDir}/ssh/host
            ${pkgs.coreutils}/bin/install -d -m 0700 -o ${toString cfg.guestUid} -g ${toString cfg.guestGid} ${cfg.stateDir}/ssh/guest

            if [ ! -f ${cfg.stateDir}/ssh/host/host_to_vm ]; then
              ${pkgs.openssh}/bin/ssh-keygen -t ed25519 -N "" -C "${cfg.vmName}-host-to-vm" -f ${cfg.stateDir}/ssh/host/host_to_vm
            fi

            ${pkgs.coreutils}/bin/chown ${cfg.controlUser}:"$host_group" ${cfg.stateDir}/ssh/host/host_to_vm ${cfg.stateDir}/ssh/host/host_to_vm.pub
            ${pkgs.coreutils}/bin/chmod 0600 ${cfg.stateDir}/ssh/host/host_to_vm
            ${pkgs.coreutils}/bin/chmod 0644 ${cfg.stateDir}/ssh/host/host_to_vm.pub

            ${pkgs.coreutils}/bin/install -m 0644 -o ${toString cfg.guestUid} -g ${toString cfg.guestGid} \
              ${cfg.stateDir}/ssh/host/host_to_vm.pub \
              ${cfg.stateDir}/ssh/guest/host_to_vm.pub

            if [ ! -f ${cfg.stateDir}/ssh/guest/vm_outbound ]; then
              ${pkgs.openssh}/bin/ssh-keygen -t ed25519 -N "" -C "${cfg.vmName}-outbound" -f ${cfg.stateDir}/ssh/guest/vm_outbound
            fi

            ${pkgs.coreutils}/bin/chown ${toString cfg.guestUid}:${toString cfg.guestGid} ${cfg.stateDir}/ssh/guest/vm_outbound ${cfg.stateDir}/ssh/guest/vm_outbound.pub
            ${pkgs.coreutils}/bin/chmod 0600 ${cfg.stateDir}/ssh/guest/vm_outbound
            ${pkgs.coreutils}/bin/chmod 0644 ${cfg.stateDir}/ssh/guest/vm_outbound.pub ${cfg.stateDir}/ssh/guest/host_to_vm.pub
            ${pkgs.coreutils}/bin/install -m 0644 -o ${cfg.controlUser} -g "$host_group" \
              ${cfg.stateDir}/ssh/guest/vm_outbound.pub \
              ${cfg.stateDir}/ssh/host/vm_outbound.pub
          '';
        };

        systemd.services."microvm-bridge-${cfg.vmName}" = {
          description = "Attach ${cfg.tapName} TAP to ${cfg.bridgeName} bridge for ${cfg.vmName}";
          after = [ "microvm-tap-interfaces@${cfg.vmName}.service" ];
          before = [ "microvm@${cfg.vmName}.service" ];
          partOf = [ "microvm@${cfg.vmName}.service" ];
          wantedBy = [ "microvm@${cfg.vmName}.service" ];
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
          };
          script = ''
            set -euo pipefail
            ${pkgs.iproute2}/bin/ip link set ${cfg.tapName} master ${cfg.bridgeName}
          '';
          preStop = ''
            ${pkgs.iproute2}/bin/ip link set ${cfg.tapName} nomaster 2>/dev/null || true
          '';
        };

        systemd.services."${cfg.vmName}-proxy" = {
          description = "Local-only proxy to OpenCode inside ${cfg.vmName}";
          after = [ "microvm@${cfg.vmName}.service" ];
          partOf = [ "microvm@${cfg.vmName}.service" ];
          wantedBy = [ "microvm@${cfg.vmName}.service" ];
          serviceConfig = {
            Type = "simple";
            Restart = "always";
            RestartSec = "2s";
            ExecStart = "${pkgs.socat}/bin/socat TCP-LISTEN:${toString cfg.hostLocalPort},bind=${cfg.hostLocalAddress},fork,reuseaddr TCP:${cfg.guestAddress}:${toString cfg.opencodePort}";
          };
        };

        security.sudo.extraRules = [
          {
            users = [ cfg.controlUser ];
            commands = [
              {
                command = initCommand;
                options = [ "NOPASSWD" ];
              }
              {
                command = startCommand;
                options = [ "NOPASSWD" ];
              }
              {
                command = stopCommand;
                options = [ "NOPASSWD" ];
              }
            ];
          }
        ];

        services.caddy.virtualHosts.${cfg.caddy.hostName} = mkIf cfg.caddy.enable {
          extraConfig = ''
            request_header X-Forwarded-Host {http.request.host}

            @outpost path /outpost.goauthentik.io/*
            reverse_proxy @outpost ${cfg.caddy.authentikUrl}

            @protected not path /outpost.goauthentik.io/*
            forward_auth @protected ${cfg.caddy.authentikUrl} {
              uri /outpost.goauthentik.io/auth/caddy
              copy_headers X-Authentik-Username
              trusted_proxies private_ranges
            }

            reverse_proxy ${cfg.hostLocalAddress}:${toString cfg.hostLocalPort}
          '';
        };
      }
      // optionalAttrs hasHostMicrovmModule {
        microvm.vms.${cfg.vmName} = {
          autostart = cfg.autostart;
          flake = cfg.flake;
        };
      }
    ))

    (mkIf cfg.guest.enable (
      {
        assertions = [
          {
            assertion = hasGuestMicrovmModule;
            message = "dog.services.opencode-agent-vm.guest.enable requires microvm.nixosModules.microvm.";
          }
        ];

        system.stateVersion = "25.05";

        networking = {
          hostName = cfg.guestHostname;
          useNetworkd = true;
          firewall = {
            enable = true;
            allowedTCPPorts = [
              22
              cfg.opencodePort
            ];
          };
        };

        systemd.network = {
          enable = true;
          networks."10-opencode-agent-eth" = {
            matchConfig.Type = "ether";
            networkConfig = {
              Address = "${cfg.guestAddress}/${toString cfg.prefixLength}";
              Gateway = cfg.hostAddress;
              DNS = cfg.hostAddress;
            };
          };
        };

        services.openssh = {
          enable = true;
          settings = {
            PasswordAuthentication = false;
            PermitRootLogin = "no";
          };
        };

        services.tailscale.enable = cfg.guest.tailscale.enable;

        users.groups.${cfg.guestUser}.gid = cfg.guestGid;
        users.users.${cfg.guestUser} = {
          isNormalUser = true;
          uid = cfg.guestUid;
          group = cfg.guestUser;
          home = "/home/${cfg.guestUser}";
          createHome = false;
          extraGroups = [ "wheel" ];
        };

        security.sudo.wheelNeedsPassword = false;

        security.pki.certificateFiles = [ ../../../hosts/mini/home-caddy.crt ];

        environment.systemPackages = with pkgs; [
          curl
          fd
          gh
          git
          jq
          neovim
          openssh
          ripgrep
          wget
        ];

        nix.settings.experimental-features = [
          "nix-command"
          "flakes"
        ];

        systemd.services."${cfg.vmName}-guest-ssh-setup" = {
          description = "Configure SSH material for ${cfg.guestUser} inside ${cfg.vmName}";
          wantedBy = [ "multi-user.target" ];
          before = [
            "sshd.service"
            "${cfg.vmName}-opencode.service"
          ];
          after = [ "local-fs.target" ];
          serviceConfig.Type = "oneshot";
          script = ''
            set -euo pipefail
            ${pkgs.coreutils}/bin/install -d -m 0700 -o ${cfg.guestUser} -g ${cfg.guestUser} /home/${cfg.guestUser}/.ssh
            ${pkgs.coreutils}/bin/install -m 0600 -o ${cfg.guestUser} -g ${cfg.guestUser} \
              /run/opencode-agent-vm/ssh/host_to_vm.pub \
              /home/${cfg.guestUser}/.ssh/authorized_keys
            ${pkgs.coreutils}/bin/ln -sfn /run/opencode-agent-vm/ssh/vm_outbound /home/${cfg.guestUser}/.ssh/id_ed25519
            ${pkgs.coreutils}/bin/ln -sfn /run/opencode-agent-vm/ssh/vm_outbound.pub /home/${cfg.guestUser}/.ssh/id_ed25519.pub
            ${pkgs.coreutils}/bin/chown -h ${cfg.guestUser}:${cfg.guestUser} \
              /home/${cfg.guestUser}/.ssh/id_ed25519 \
              /home/${cfg.guestUser}/.ssh/id_ed25519.pub
          '';
        };

        systemd.services."${cfg.vmName}-opencode" = {
          description = "OpenCode web UI inside ${cfg.vmName}";
          wantedBy = [ "multi-user.target" ];
          wants = [ "network-online.target" ];
          after = [
            "network-online.target"
            "${cfg.vmName}-guest-ssh-setup.service"
          ];
          serviceConfig = {
            Type = "simple";
            User = cfg.guestUser;
            WorkingDirectory = cfg.workingDirectory;
            ExecStart = "${pkgs.bash}/bin/bash -lc 'exec ${pkgs.llm-agents.opencode}/bin/opencode serve --hostname ${cfg.guestAddress} --port ${toString cfg.opencodePort}'";
            Restart = "on-failure";
            RestartSec = "2s";
          };
        };

        home-manager = {
          useGlobalPkgs = true;
          useUserPackages = true;
          users.${cfg.guestUser} = { pkgs, ... }: {
            home = {
              username = cfg.guestUser;
              homeDirectory = "/home/${cfg.guestUser}";
              stateVersion = "25.05";
              packages = with pkgs; [
                llm-agents.opencode
              ];
            };

            targets.genericLinux.enable = true;

            dog.programs = {
              cli-tools.enable = true;
              git.enable = true;
              forgejo-cli = {
                enable = true;
                host = "https://git.local.doreto.com.br";
              };
              playwright-cli = {
                enable = true;
                installAgentsSkill = true;
              };
            };

            programs = {
              ssh.enable = true;
            };
          };
        };

      }
      // optionalAttrs hasGuestMicrovmModule {
        microvm = {
          hypervisor = "qemu";
          vcpu = cfg.vcpu;
          mem = cfg.mem;
          writableStoreOverlay = "/nix/.rw-store";
          interfaces = [
            {
              type = "tap";
              id = cfg.tapName;
              mac = cfg.guestMac;
            }
          ];
          volumes = [
            {
              image = "${cfg.stateDir}/rw-store.img";
              label = "nix-store";
              mountPoint = "/nix/.rw-store";
              size = 65536;
            }
          ];
          shares = [
            {
              proto = "virtiofs";
              tag = "ro-store";
              source = "/nix/store";
              mountPoint = "/nix/.ro-store";
              readOnly = true;
            }
            persistentHomeShare
            persistentVarLibShare
            persistentVarLogShare
            guestSshShare
          ]
          ++ map userShareToMicrovmShare cfg.shares;
        };
      }
    ))
  ];
}
