{ config, pkgs, ... }:

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
  };
  services.nginx.virtualHosts."${config.services.nextcloud.hostName}".listen = [
    {
      addr = "127.0.0.1";
      port = 5387;
    }
  ];
}
