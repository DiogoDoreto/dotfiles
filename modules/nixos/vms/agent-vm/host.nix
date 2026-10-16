# Keep ./README.md up to date when changing this module's behavior.
{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.dog.services.agent-vm;
  dnsServers = (map (zone: "/${zone.domain}/${zone.server}") cfg.dnsForwardZones) ++ cfg.dnsUpstreams;
  startCommand = "${pkgs.systemd}/bin/systemctl start microvm@${cfg.vmName}.service";
  stopCommand = "${pkgs.systemd}/bin/systemctl stop microvm@${cfg.vmName}.service";
  initCommand = "${pkgs.systemd}/bin/systemctl start ${cfg.vmName}-init.service";

  startScript = pkgs.writeShellScriptBin "start-agent-vm" ''
    set -euo pipefail
    sudo ${initCommand}
    sudo ${startCommand}
    echo "Agent VM starting: microvm@${cfg.vmName}.service"
    echo ""
    echo "Web UI:              http://${cfg.hostLocalAddress}:${toString cfg.hostLocalPort}"
    echo "SSH:                 agent-vm-ssh"
    echo "VM public key:       agent-vm-public-key"
    echo "DNS audit:           journalctl -u dnsmasq -g 'query'"
    echo "Connection audit:    journalctl -k -g '[${cfg.vmName}]'"
    echo "Stop:                stop-agent-vm"
  '';

  stopScript = pkgs.writeShellScriptBin "stop-agent-vm" ''
    set -euo pipefail
    sudo ${stopCommand}
    echo "Agent VM stopped."
  '';

  sshScript = pkgs.writeShellScriptBin "agent-vm-ssh" ''
    set -euo pipefail
    exec ${pkgs.openssh}/bin/ssh \
      -i ${cfg.stateDir}/ssh/host/host_to_vm \
      -o StrictHostKeyChecking=no \
      -o UserKnownHostsFile=/dev/null \
      ${cfg.guestUser}@${cfg.guestAddress} "$@"
  '';

  publicKeyScript = pkgs.writeShellScriptBin "agent-vm-public-key" ''
    set -euo pipefail
    if [ ! -f ${cfg.stateDir}/ssh/host/vm_outbound.pub ]; then
      sudo ${initCommand}
    fi
    ${pkgs.coreutils}/bin/cat ${cfg.stateDir}/ssh/host/vm_outbound.pub
  '';

  logsScript = pkgs.writeShellScriptBin "agent-vm-logs" ''
    exec ${pkgs.systemd}/bin/journalctl \
      -u ${cfg.vmName}-init.service \
      -u microvm@${cfg.vmName}.service \
      -u ${cfg.vmName}-proxy.service \
      ${optionalString cfg.paseo.enable "-u ${cfg.vmName}-paseo-proxy.service \\"}
      "$@"
  '';
in
{
  imports = [ ./options.nix ];

  assertions = [
    {
      assertion = cfg.flake != null;
      message = "dog.services.agent-vm.flake must be set when importing the host module.";
    }
    {
      assertion = !cfg.paseo.enable || cfg.paseo.hostLocalPort != cfg.hostLocalPort;
      message = "dog.services.agent-vm.paseo.hostLocalPort must differ from hostLocalPort.";
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
    // optionalAttrs cfg.dnsmasqBindInterfaces { bind-interfaces = true; }
    // optionalAttrs cfg.dnsmasqBindBridgeInterface { interface = mkAfter [ cfg.bridgeName ]; };
  };

  systemd.services."${cfg.vmName}-init" = {
    description = "Initialize Agent VM state and SSH keys";
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

  systemd.services."${cfg.vmName}-paseo-proxy" = mkIf cfg.paseo.enable {
    description = "Local-only proxy to Paseo inside ${cfg.vmName}";
    after = [ "microvm@${cfg.vmName}.service" ];
    partOf = [ "microvm@${cfg.vmName}.service" ];
    wantedBy = [ "microvm@${cfg.vmName}.service" ];
    serviceConfig = {
      Type = "simple";
      Restart = "always";
      RestartSec = "2s";
      ExecStart = "${pkgs.socat}/bin/socat TCP-LISTEN:${toString cfg.paseo.hostLocalPort},bind=${cfg.hostLocalAddress},fork,reuseaddr TCP:${cfg.guestAddress}:${toString cfg.paseo.guestPort}";
    };
  };

  security.sudo.extraRules = [
    {
      users = [ cfg.controlUser ];
      commands =
        map
          (command: {
            inherit command;
            options = [ "NOPASSWD" ];
          })
          [
            initCommand
            startCommand
            stopCommand
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

  services.caddy.virtualHosts.${cfg.paseo.hostName} = mkIf (cfg.caddy.enable && cfg.paseo.enable) {
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

      reverse_proxy ${cfg.hostLocalAddress}:${toString cfg.paseo.hostLocalPort}
    '';
  };

  microvm.vms.${cfg.vmName} = {
    autostart = cfg.autostart;
    flake = cfg.flake;
  };
}
