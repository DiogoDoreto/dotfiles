{ ... }:

{
  # Group for deploying static files to /var/lib/www.
  # Add any user or service that needs to upload files here.
  users.groups.caddy_www = { };
  users.users.caddy.extraGroups = [ "caddy_www" ];

  # 2775: setgid bit (2) ensures new files/dirs created inside /var/lib/www
  # automatically inherit the caddy_www group, so caddy can always read them.
  systemd.tmpfiles.rules = [
    "d /var/lib/www 2775 root caddy_www -"
  ];

  services.caddy = {
    enable = true;
    virtualHosts = {
      "*.local.doreto.com.br" = {
        # 'tls internal' tells Caddy to self-sign this without ACME/Let's Encrypt
        # Grab and trust /var/lib/caddy/.local/share/caddy/pki/authorities/local/root.crt
        extraConfig = ''
          tls internal
        '';
      };
      "auth.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy https://localhost:9443 {
            transport http {
              tls_insecure_skip_verify
            }
          }
        '';
      };
      "nextcloud.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy localhost:5387
        '';
      };
      "lidarr.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy localhost:8686
        '';
      };
      "radarr.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy localhost:7878
        '';
      };
      "sonarr.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy localhost:8989
        '';
      };
      "prowlarr.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy localhost:9696
        '';
      };
      "qbit.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy localhost:8079
        '';
      };
      "jellyfin.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy localhost:8096
        '';
      };
      "calibre.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy localhost:18083
        '';
      };
      "audiobook.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy localhost:18090
        '';
      };
      "ai.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy localhost:11111
        '';
      };
      "ha.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy 192.168.0.2:8123
        '';
      };
      "home.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy localhost:8082
        '';
      };
      "vite.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy localhost:5173
        '';
      };
      "search.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy localhost:8888
        '';
      };
      "code.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy localhost:32849
        '';
      };
      "opencode.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy localhost:32859
        '';
      };
      "www.local.doreto.com.br" = {
        extraConfig = ''
          root * /var/lib/www
          file_server browse
        '';
      };
    };
  };
}
