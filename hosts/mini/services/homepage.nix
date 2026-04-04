{ pkgs, ... }:

let
  vars = import ../_variables.nix;
in

{
  services.homepage-dashboard = {
    enable = true;
    listenPort = vars.ports.homepage;
    allowedHosts = "home.local.doreto.com.br";
    widgets = [
      {
        resources = {
          cpu = true;
          memory = true;
          disk = "/";
        };
      }
      {
        search = {
          provider = "duckduckgo";
          target = "_blank";
        };
      }
    ];
    services = [
      # search for icons in https://dashboardicons.com
      {
        Media = [
          {
            "FreshRSS" = rec {
              icon = "freshrss.png";
              href = "https://freshrss.local.doreto.com.br";
              ping = href;
            };
          }
          {
            "Jellyfin" = rec {
              icon = "jellyfin.png";
              href = "https://jellyfin.local.doreto.com.br";
              ping = href;
            };
          }
          {
            "NextCloud" = rec {
              icon = "nextcloud.png";
              href = "https://nextcloud.local.doreto.com.br";
              ping = href;
            };
          }
          {
            "Calibre" = rec {
              icon = "calibre.png";
              href = "https://calibre.local.doreto.com.br";
              ping = href;
            };
          }
          {
            "Audiobooks" = rec {
              icon = "audiobookshelf.png";
              href = "https://audiobook.local.doreto.com.br";
              ping = href;
            };
          }
        ];
      }
      {
        Apps = [
          {
            "HomeAssistant" = rec {
              icon = "home-assistant.png";
              href = "https://ha.local.doreto.com.br";
              ping = href;
            };
          }
          {
            "OpenCode" = rec {
              icon = "opencode.png";
              href = "https://opencode.local.doreto.com.br";
              ping = href;
            };
          }
          {
            "Forgejo" = rec {
              icon = "forgejo.png";
              href = "https://git.local.doreto.com.br";
              ping = href;
            };
          }
          {
            "Authentik" = rec {
              icon = "authentik.png";
              href = "https://auth.local.doreto.com.br";
              ping = href;
            };
          }
          {
            "Searx" = rec {
              icon = "searxng.png";
              href = "https://search.local.doreto.com.br";
              ping = href;
            };
          }
          {
            "VSCode" = rec {
              icon = "vscode.png";
              href = "https://code.local.doreto.com.br";
              ping = href;
            };
          }
          {
            "LibreChat" = rec {
              icon = "librechat.png";
              href = "https://chat.local.doreto.com.br";
              ping = href;
            };
          }
          {
            "InvokeAI" = rec {
              icon = "midjourney-light.png";
              href = "https://invokeai.local.doreto.com.br";
              ping = href;
            };
          }
          {
            "Kokoro TTS" = {
              icon = "kokoro-web.png";
              href = "https://tts.local.doreto.com.br/web/";
              ping = "https://tts.local.doreto.com.br";
            };
          }
        ];
      }
      {
        Downloads = [
          {
            "qBittorrent" = rec {
              icon = "qbittorrent.png";
              href = "https://qbit.local.doreto.com.br";
              ping = href;
              widget = {
                type = "qbittorrent";
                url = href;
              };
            };
          }
          {
            "Radarr" = rec {
              icon = "radarr.png";
              description = "Movies downloader";
              href = "https://radarr.local.doreto.com.br";
              ping = href;
              widget = {
                type = "radarr";
                url = href;
                key = "29b5783c14964b3abae3886e4fe3b93b";
              };
            };
          }
          {
            "Sonarr" = rec {
              icon = "sonarr.png";
              description = "TV Shows downloader";
              href = "https://sonarr.local.doreto.com.br";
              ping = href;
              widget = {
                type = "sonarr";
                url = href;
                key = "023e2f0779304b6e9bbb6cd077cf8dc8";
              };
            };
          }
          {
            "Lidarr" = rec {
              icon = "lidarr.png";
              description = "Music downloader";
              href = "https://lidarr.local.doreto.com.br";
              ping = href;
              widget = {
                type = "lidarr";
                url = href;
                key = "d7bc3d65c5334400b6f3338de945915a";
              };
            };
          }
          {
            "Prowlarr " = rec {
              icon = "prowlarr.png";
              description = "Search torrents";
              href = "https://prowlarr.local.doreto.com.br";
              ping = href;
            };
          }
        ];
      }
    ];
  };
  systemd.services.homepage-dashboard.path = [ pkgs.unixtools.ping ];
}
