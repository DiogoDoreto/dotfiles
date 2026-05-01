{ config, pkgs, ... }:

let
  vars = import ./_variables.nix;
  chungusProxyIp = "192.168.0.4"; # dummy interface dedicated to chungus proxy
in

{
  environment.systemPackages = with pkgs; [ openvpn ];

  networking = {
    hostName = "dogdot";
    networkmanager = {
      enable = true;
      unmanaged = [ "chungus-proxy" ];
    };
    firewall = {
      enable = true;
      allowedTCPPorts =
        with vars.ports;
        [
          dns
          http
          https
          forgejoSsh
        ]
        ++ vars.ports.haHomekitBridge;
      allowedUDPPorts = with vars.ports; [
        dns
        mdns
      ];
    };
  };

  security.pki.certificateFiles = [ ./home-caddy.crt ];

  services.tailscale.enable = true;

  services.openvpn.servers = {
    vpn-es = {
      config = "config /home/dog/.vpn/es-mad.prod.surfshark.comsurfshark_openvpn_udp.ovpn";
      autoStart = true;
    };
    vpn-br = {
      config = "config /home/dog/.vpn/br-sao.prod.surfshark.comsurfshark_openvpn_udp.ovpn";
      autoStart = false;
    };
  };

  services.dnsmasq = {
    enable = true;
    alwaysKeepRunning = true;
    settings =
      let
        static-ip = "192.168.0.2"; # needs to be set manually in networkmanager
        tailscale-ip = "100.117.142.110";
      in
      {
        # interface = "wlo1";
        # bind-interfaces = true;
        listen-address = "::1,127.0.0.1,${static-ip},${chungusProxyIp},${tailscale-ip}";
        address = [
          "/${config.networking.hostName}.home/${static-ip}"
          "/${config.networking.hostName}.home/${tailscale-ip}"
          "/.local.doreto.com.br/${static-ip}"
          "/.local.doreto.com.br/${tailscale-ip}"
          "/chungus.home/192.168.0.3"
          "/chungus-proxy.home/${chungusProxyIp}"
        ];

        # Accept DNS queries only from hosts whose address is on a local subnet
        # local-service = true;
        # Do not read system files
        no-hosts = true;
        no-resolv = true;
        # Do not send private addresses to upstream servers
        bogus-priv = true;
        # Do not send addresses without dot to upstream servers
        domain-needed = true;
        # Upstream DNS servers
        server = [
          "1.1.1.1"
          "1.0.0.1"
        ];
        # Log DNS queries
        log-queries = true;

        # Enable DNSSEC
        # dnssec = true;
        # DNSSEC trust anchor. Source: https://data.iana.org/root-anchors/root-anchors.xml
        # trust-anchor = ".,20326,8,2,E06D44B80B8F1D39A95C0B0D7C65D08458E880409BBC683457104237C7F8EC8D";
      };
  };

  systemd.services.chungus-proxy-iface = {
    description = "dummy interface + proxy ARP for chungus proxy (${chungusProxyIp})";
    wantedBy = [ "network-online.target" ];
    wants = [ "network-online.target" ];
    after = [ "network-online.target" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = pkgs.writeShellScript "chungus-proxy-iface-up" ''
        ${pkgs.iproute2}/bin/ip link add chungus-proxy type dummy 2>/dev/null || true
        ${pkgs.iproute2}/bin/ip addr add ${chungusProxyIp}/32 dev chungus-proxy 2>/dev/null || true
        ${pkgs.iproute2}/bin/ip link set chungus-proxy up
        ${pkgs.procps}/bin/sysctl -w net.ipv4.conf.wlo1.proxy_arp=1
      '';
      ExecStop = pkgs.writeShellScript "chungus-proxy-iface-down" ''
        ${pkgs.iproute2}/bin/ip link del chungus-proxy 2>/dev/null || true
        ${pkgs.procps}/bin/sysctl -w net.ipv4.conf.wlo1.proxy_arp=0
      '';
    };
  };
}
