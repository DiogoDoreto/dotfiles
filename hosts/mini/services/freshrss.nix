{ pkgs, ... }:

{
  services.freshrss = {
    enable = true;
    extensions = with pkgs.freshrss-extensions; [
      youtube
      reading-time
    ];
    webserver = "caddy";
    virtualHost = "freshrss.local.doreto.com.br";
    baseUrl = "https://freshrss.local.doreto.com.br";
    passwordFile = "/var/lib/freshrss-pass.txt";
  };
}
