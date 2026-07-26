{ lib, pkgs, ... }:

let
  vars = import ../_variables.nix;
in

{
  services.cockpit = {
    enable = true;
    port = vars.ports.cockpit;
    openFirewall = false;
    plugins = [ pkgs.cockpit-podman ];
    allowed-origins = [ "https://cockpit.local.doreto.com.br" ];
    settings.WebService.ProtocolHeader = "X-Forwarded-Proto";
  };

  systemd.sockets.cockpit.listenStreams = lib.mkForce [
    "127.0.0.1:${toString vars.ports.cockpit}"
  ];
}
