{ ... }:

let
  vars = import ./_variables.nix;
  p = builtins.mapAttrs (_: toString) vars.ports;
in

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
          reverse_proxy https://localhost:${p.authentik} {
            transport http {
              tls_insecure_skip_verify
            }
          }
        '';
      };
      "nextcloud.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy localhost:${p.nextcloud}
        '';
      };
      "lidarr.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy localhost:${p.lidarr}
        '';
      };
      "radarr.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy localhost:${p.radarr}
        '';
      };
      "sonarr.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy localhost:${p.sonarr}
        '';
      };
      "prowlarr.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy localhost:${p.prowlarr}
        '';
      };
      "qbit.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy localhost:${p.qbittorrent}
        '';
      };
      "jellyfin.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy localhost:${p.jellyfin}
        '';
      };
      "calibre.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy localhost:${p.calibre}
        '';
      };
      "audiobook.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy localhost:${p.audiobookshelf}
        '';
      };
      "ai.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy localhost:${p.openWebui}
        '';
      };
      "ha.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy 192.168.0.2:${p.home-assistant}
        '';
      };
      "home.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy localhost:${p.homepage}
        '';
      };
      "vite.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy localhost:${p.vite}
        '';
      };
      "search.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy localhost:${p.searx}
        '';
      };
      "code.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy localhost:${p.vscodium}
        '';
      };
      "opencode.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy localhost:${p.opencode}
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
