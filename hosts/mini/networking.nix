{
  config,
  lib,
  pkgs,
  ...
}:

let
  vars = import ./_variables.nix;
  agentVm = config.dog.services.agent-vm or null;
  lanGateway = "192.168.0.1";
  lanInterface = "wlo1";
  staticIp = "192.168.0.2"; # needs to be set manually in Network Manager
  tailscaleIp = "100.117.142.110";
  dnsUpstreams = [
    "1.1.1.1"
    "1.0.0.1"
  ];
  dnsmasqStateDir = "/var/lib/dnsmasq";
  dnsmasqLanLeaseFile = "${dnsmasqStateDir}/dnsmasq-lan.leases";
  dnsmasqTailscaleLeaseFile = "${dnsmasqStateDir}/dnsmasq-tailscale.leases";

  dnsmasqSettingsFormat =
    let
      formatKeyValue =
        name: value:
        if value == true then
          name
        else if value == false then
          "# setting `${name}` explicitly set to false"
        else
          lib.generators.mkKeyValueDefault { } "=" name value;
    in
    pkgs.formats.keyValue {
      mkKeyValue = formatKeyValue;
      listsAsDuplicateKeys = true;
    };

  commonDnsmasqSettings = {
    bind-dynamic = true;
    no-hosts = true;
    no-resolv = true;
    bogus-priv = true;
    domain-needed = true;
    server = dnsUpstreams;
    log-queries = true;
  };

  mkDnsmasqConfig =
    name: leaseFile: settings:
    dnsmasqSettingsFormat.generate "dnsmasq-${name}.conf" (
      commonDnsmasqSettings
      // {
        dhcp-leasefile = leaseFile;
        pid-file = "/run/dnsmasq-${name}.pid";
      }
      // settings
    );

  dnsmasqLanConfig = mkDnsmasqConfig "lan" dnsmasqLanLeaseFile {
    listen-address = [
      "::1"
      "127.0.0.1"
      staticIp
    ]
    ++ lib.optional (agentVm != null) agentVm.hostAddress;
    address = [
      "/${config.networking.hostName}.home/${staticIp}"
      "/local.doreto.com.br/${staticIp}"
      "/chungus.home/${vars.chungusIp}"
    ];
  };

  dnsmasqTailscaleConfig = mkDnsmasqConfig "tailscale" dnsmasqTailscaleLeaseFile {
    listen-address = [ tailscaleIp ];
    address = [ "/local.doreto.com.br/${tailscaleIp}" ];
  };

  mkDnsmasqService =
    {
      description,
      configFile,
      leaseFile,
      after ? [ ],
      wants ? [ ],
    }:
    {
      inherit description wants;
      wantedBy = [ "multi-user.target" ];
      after = after ++ [
        "network.target"
        "systemd-resolved.service"
      ];
      path = [ pkgs.dnsmasq ];
      preStart = ''
        mkdir -m 755 -p ${dnsmasqStateDir}
        touch ${leaseFile}
        chown -R dnsmasq ${dnsmasqStateDir}
        dnsmasq --test -C ${configFile}
      '';
      serviceConfig = {
        ExecStart = "${pkgs.dnsmasq}/bin/dnsmasq -k --user=dnsmasq -C ${configFile}";
        ExecReload = "${pkgs.coreutils}/bin/kill -HUP $MAINPID";
        PrivateTmp = true;
        ProtectSystem = true;
        ProtectHome = true;
        Restart = "always";
      };
    };

  keepDnsOffVpn = pkgs.writeShellScript "keep-dns-off-vpn" ''
    set -eu

    action="$1"
    for upstream in ${builtins.concatStringsSep " " dnsUpstreams}; do
      case "$action" in
        up)
          ${pkgs.iproute2}/bin/ip route replace "$upstream/32" via ${lanGateway} dev ${lanInterface}
          ;;
        down)
          ${pkgs.iproute2}/bin/ip route del "$upstream/32" via ${lanGateway} dev ${lanInterface} 2>/dev/null || true
          ;;
      esac
    done
  '';
in

{
  environment.systemPackages = with pkgs; [ openvpn ];

  networking = {
    hostName = "dogdot";
    nameservers = [ "127.0.0.1" ];
    networkmanager = {
      enable = true;
    };
    firewall = {
      enable = true;
      # The Ubuntu runner's cache server is on the host; only job containers
      # on Podman's bridges need to connect to it.
      # nftables uses a quoted glob; iptables uses a trailing + for prefix matching.
      extraInputRules = lib.mkIf config.networking.nftables.enable ''
        iifname "podman*" tcp dport ${toString vars.ports.forgejoRunnerCache} accept
      '';
      interfaces."podman+".allowedTCPPorts = lib.mkIf (!config.networking.nftables.enable) [
        vars.ports.forgejoRunnerCache
      ];
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
      up = "${keepDnsOffVpn} up";
      down = "${keepDnsOffVpn} down";
    };
    vpn-br = {
      config = "config /home/dog/.vpn/br-sao.prod.surfshark.comsurfshark_openvpn_udp.ovpn";
      autoStart = false;
      up = "${keepDnsOffVpn} up";
      down = "${keepDnsOffVpn} down";
    };
  };

  services.dnsmasq.enable = lib.mkForce false;

  users.users.dnsmasq = {
    isSystemUser = true;
    group = "dnsmasq";
    description = "Dnsmasq daemon user";
  };
  users.groups.dnsmasq = { };

  systemd.services.dnsmasq-lan = mkDnsmasqService {
    description = "Dnsmasq LAN DNS";
    configFile = dnsmasqLanConfig;
    leaseFile = dnsmasqLanLeaseFile;
  };

  systemd.services.dnsmasq-tailscale = mkDnsmasqService {
    description = "Dnsmasq Tailscale DNS";
    configFile = dnsmasqTailscaleConfig;
    leaseFile = dnsmasqTailscaleLeaseFile;
    after = [ "tailscaled.service" ];
    wants = [
      "network-online.target"
      "tailscaled.service"
    ];
  };

}
