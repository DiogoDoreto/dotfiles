# Extension module for the router VM role.
#
# Import alongside guest.nix when building the agent-router VM.  Adds:
#   - A SLIRP (QEMU user-mode NAT) uplink for internet access
#   - SSH host-forward so the host can reach the router at localhost:<sshPort>
#   - nftables NAT (masquerade) + audit logging (every new TCP/UDP connection)
#   - MSS clamping to prevent hangs on VPN paths with reduced MTU
#   - dnsmasq on the internal vde interface, forwarding to the host's resolver
#     via SLIRP's 10.0.2.3 alias — so VPN-aware DNS is honoured transparently
#
# Agent VMs reach the internet by routing their default gateway to this VM
# (10.0.0.1).  SSH to agent VMs goes via ProxyJump through this router.
#
# Audit:
#   journalctl -u dnsmasq -g 'query'          # DNS lookups
#   journalctl -k -g '\[vm-agent\]'            # new connections
{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.dog.microvm-agent;
in
{
  options.dog.microvm-agent.router = {
    sshPort = mkOption {
      type        = types.port;
      default     = 2200;
      description = "Host-side TCP port forwarded to the router VM's SSH (22). Used as the ProxyJump gateway for all agent VMs.";
    };

    slirpMac = mkOption {
      type        = types.str;
      default     = "02:00:00:ff:ff:02";
      description = "MAC address for the SLIRP (internet uplink) NIC inside the router VM.";
    };

    dnsForwarders = mkOption {
      type        = types.listOf types.str;
      # 10.0.2.3 is QEMU SLIRP's built-in alias for the host's resolver.
      # Forwarding here means the guest uses whatever DNS the host has
      # configured — including VPN-pushed nameservers.
      default     = [ "10.0.2.3" ];
      description = "Upstream DNS servers for dnsmasq. Default proxies the host's resolver via SLIRP so VPN DNS is honoured automatically.";
    };
  };

  config = {
    # SLIRP NIC: provides internet egress via QEMU user-mode networking.
    # hostfwd maps host-localhost:<sshPort> → router:22 so the host can SSH
    # in directly, and agent VMs use this router as a ProxyJump target.
    microvm.qemu.extraArgs = [
      "-netdev"
      "user,id=slirp0,hostfwd=tcp:127.0.0.1:${toString cfg.router.sshPort}-:22"
      "-device"
      "virtio-net-pci,netdev=slirp0,mac=${cfg.router.slirpMac}"
    ];

    # Rename the SLIRP NIC for predictable nftables references.
    systemd.network.links."10-vm-slirp" = {
      matchConfig.MACAddress = cfg.router.slirpMac;
      linkConfig.Name        = "vm-slirp";
    };

    # vm-slirp gets an address from QEMU's built-in DHCP (10.0.2.0/24 range).
    systemd.network.networks."20-vm-slirp" = {
      matchConfig.Name   = "vm-slirp";
      networkConfig.DHCP = "yes";
    };

    boot.kernel.sysctl."net.ipv4.ip_forward" = 1;

    networking.nftables.enable = true;

    # NAT: masquerade traffic from agent VMs out through the SLIRP uplink.
    # MSS clamping: many VPN tunnels run at MTU ~1400 and silently drop
    # path-MTU-discovery ICMP; clamping the TCP MSS prevents the resulting
    # hangs without requiring PMTUD to work end-to-end.
    networking.nftables.tables."vm-agent-nat" = {
      family = "ip";
      content = ''
        chain postrouting {
          type nat hook postrouting priority srcnat;
          ip saddr 10.0.0.0/24 oifname "vm-slirp" masquerade
          ip saddr 10.0.0.0/24 oifname "vm-slirp" tcp flags syn tcp option maxseg size set rt mtu
        }
      '';
    };

    # Audit: log every new outbound TCP/UDP connection from agent VMs.
    # Read with: journalctl -k -g '\[vm-agent\]'
    networking.nftables.tables."vm-agent-audit" = {
      family = "inet";
      content = ''
        chain forward-log {
          type filter hook forward priority filter - 1;
          iifname "vm-int" ip protocol tcp ct state new log prefix "[vm-agent] " level info
          iifname "vm-int" ip protocol udp ct state new log prefix "[vm-agent] " level info
        }
      '';
    };

    # DNS resolver for agent VMs.  Listens only on the internal vde interface
    # so it doesn't interfere with the SLIRP-side name resolution.
    services.dnsmasq = {
      enable              = true;
      resolveLocalQueries = false;
      settings = {
        interface       = "vm-int";
        listen-address  = "10.0.0.1";
        bind-interfaces = true;
        log-queries     = true;
        no-resolv       = true;
        server          = cfg.router.dnsForwarders;
      };
    };

    environment.systemPackages = with pkgs; [ nftables iproute2 tcpdump ];
  };
}
