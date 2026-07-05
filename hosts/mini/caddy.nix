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
          reverse_proxy http://127.0.0.1:${p.authentik}
        '';
      };
      "nextcloud.local.doreto.com.br" = {
        extraConfig = ''
          header Strict-Transport-Security "max-age=15552000; includeSubDomains"
          reverse_proxy 127.0.0.1:${p.nextcloud}
        '';
      };
      "lidarr.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy 127.0.0.1:${p.lidarr}
        '';
      };
      "radarr.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy 127.0.0.1:${p.radarr}
        '';
      };
      "sonarr.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy 127.0.0.1:${p.sonarr}
        '';
      };
      "prowlarr.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy 127.0.0.1:${p.prowlarr}
        '';
      };
      "qbit.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy 127.0.0.1:${p.qbittorrent}
        '';
      };
      "jellyfin.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy 127.0.0.1:${p.jellyfin}
        '';
      };
      "calibre.local.doreto.com.br" = {
        extraConfig = ''
          request_header X-Forwarded-Host {http.request.host}

          @outpost path /outpost.goauthentik.io/*
          reverse_proxy @outpost http://127.0.0.1:${p.authentik}

          @protected not path /opds*
          forward_auth @protected http://127.0.0.1:${p.authentik} {
            uri /outpost.goauthentik.io/auth/caddy
            copy_headers X-Authentik-Username
            trusted_proxies private_ranges
          }

          reverse_proxy 127.0.0.1:${p.calibre}
        '';
      };
      "git.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy 127.0.0.1:${p.forgejo}
        '';
      };
      "*.pages.local.doreto.com.br" = {
        extraConfig = ''
          tls internal
          reverse_proxy 127.0.0.1:${p.gitPages}
        '';
      };
      "*.pages-preview.local.doreto.com.br" = {
        extraConfig = ''
          tls internal
          reverse_proxy 127.0.0.1:${p.gitPages}
        '';
      };
      "apps.doreto.com.br:${p.publicCaddy}" = {
        extraConfig = ''
          bind 127.0.0.1

          redir /app-agenda-escolar /app-agenda-escolar/ 308

          @appAgendaEscolar path /app-agenda-escolar/*
          reverse_proxy @appAgendaEscolar 127.0.0.1:${p.gitPages} {
            header_up Host diogo.pages.local.doreto.com.br
          }

          respond "not found" 404
        '';
      };
      ":${p.publicCaddy}" = {
        extraConfig = ''
          bind 127.0.0.1
          respond "not found" 404
        '';
      };
      "audiobook.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy 127.0.0.1:${p.audiobookshelf}
        '';
      };
      "ai.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy 127.0.0.1:${p.openWebui}
        '';
      };
      "chat.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy 127.0.0.1:${p.libreChat}
        '';
      };
      "ha.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy 192.168.0.2:${p.home-assistant}
        '';
      };
      "home.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy 127.0.0.1:${p.homepage}
        '';
      };
      "search.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy 127.0.0.1:${p.searx}
        '';
      };
      "code.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy 127.0.0.1:${p.vscodium}
        '';
      };
      "tts.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy 127.0.0.1:${p.kokoro}
        '';
      };
      "photos.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy http://${vars.chungusProxyIp}:${p.immich}
        '';
      };
      "llama.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy http://${vars.chungusProxyIp}:${p.llama}
        '';
      };
      "invokeai.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy http://${vars.chungusProxyIp}:${p.invokeai}
        '';
      };
      "www.local.doreto.com.br" = {
        extraConfig = ''
          root * /var/lib/www
          file_server browse
        '';
      };
      "metrics.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy 127.0.0.1:${p.victoriametrics}
        '';
      };
      "logs.local.doreto.com.br" = {
        extraConfig = ''
          reverse_proxy 127.0.0.1:${p.victorialogs}
        '';
      };
    };
  };
}
