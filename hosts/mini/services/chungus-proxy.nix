# chungus-proxy: transparent Wake-on-LAN proxy for chungus AI services
#
# chungus (192.168.0.3) auto-suspends after 10 min of inactivity. This module
# creates systemd socket units on a dedicated dummy interface (192.168.0.4)
# that proxy connections to chungus, sending a WoL magic packet first if it is
# asleep.
#
# Architecture:
#   client → systemd socket (192.168.0.4:PORT, via chungus-proxy.home)
#          → waker script: sends WoL, polls until port is up, socat-proxies
#          → chungus (192.168.0.3):PORT
#
# DNS (see networking.nix):
#   chungus.home       → 192.168.0.3  (real machine; SSH and direct access)
#   chungus-proxy.home → 192.168.0.4  (this proxy; for service-to-service use)
{ pkgs, ... }:

let
  chungusIp = "192.168.0.3";
  chungusMac = "74:56:3c:41:57:85"; # enp4s0 Ethernet MAC

  mkWakerScript =
    port:
    pkgs.writeShellScript "chungus-proxy-${toString port}" ''
      set -euo pipefail
      TARGET_PORT="${toString port}"
      TIMEOUT=180
      POLL=3

      ${pkgs.wol}/bin/wol -h 192.168.0.255 "${chungusMac}" >/dev/null 2>&1 || true

      elapsed=0
      until ${pkgs.netcat-gnu}/bin/nc -z -w 2 "${chungusIp}" "$TARGET_PORT" 2>/dev/null; do
        [ "$elapsed" -ge "$TIMEOUT" ] && { echo "timeout waiting for chungus" >&2; exit 1; }
        ${pkgs.coreutils}/bin/sleep "$POLL"
        elapsed=$((elapsed + POLL))
      done

      exec ${pkgs.socat}/bin/socat - "TCP:${chungusIp}:$TARGET_PORT"
    '';

  proxied = [
    {
      name = "llama";
      port = 8080;
    }
    {
      name = "invokeai";
      port = 9090;
    }
  ];

in
{
  systemd.sockets = builtins.listToAttrs (
    map (
      { name, port }:
      {
        name = "chungus-proxy-${name}";
        value = {
          description = "WoL proxy socket → chungus:${toString port}";
          wantedBy = [ "sockets.target" ];
          requires = [ "chungus-proxy-iface.service" ];
          after = [ "chungus-proxy-iface.service" ];
          socketConfig = {
            ListenStream = "192.168.0.4:${toString port}";
            Accept = true;
            NoDelay = true;
            KeepAlive = true;
          };
        };
      }
    ) proxied
  );

  systemd.services = builtins.listToAttrs (
    map (
      { name, port }:
      {
        name = "chungus-proxy-${name}@";
        value = {
          description = "WoL waker+proxy for chungus:${toString port}";
          serviceConfig = {
            ExecStart = mkWakerScript port;
            StandardInput = "socket";
            StandardOutput = "socket";
            StandardError = "journal";
            DynamicUser = true;
            PrivateTmp = true;
            RestrictAddressFamilies = [
              "AF_INET"
              "AF_INET6"
            ];
            TimeoutStartSec = "210s";
          };
        };
      }
    ) proxied
  );

  # Open proxied ports so Caddy and containers can reach the macvlan interface.
  networking.firewall.allowedTCPPorts = [ 8080 9090 ];
}
