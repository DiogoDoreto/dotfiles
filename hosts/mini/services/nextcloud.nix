{ config, pkgs, ... }:

let
  vars = import ../_variables.nix;
in

{
  services.nextcloud = {
    enable = true;
    package = pkgs.nextcloud32;
    hostName = "localhost";
    settings = {
      trusted_domains = [
        "nextcloud.local.doreto.com.br"
      ];
      trusted_proxies = [
        "127.0.0.1"
      ];
    };
    config = {
      adminpassFile = "/var/lib/nextcloud-pass.txt";
      dbtype = "sqlite";
    };
    extraApps = { inherit (pkgs) orgnotes; };
    extraAppsEnable = true;
  };
  services.nginx.virtualHosts."${config.services.nextcloud.hostName}".listen = [
    {
      addr = "127.0.0.1";
      port = vars.ports.nextcloud;
    }
  ];
}
