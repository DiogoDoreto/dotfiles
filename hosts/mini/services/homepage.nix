{ pkgs, ... }:

let
  vars = import ../_variables.nix;
in

{
  services.homepage-dashboard = {
    enable = true;
    listenPort = vars.ports.homepage;
    allowedHosts = "home.local.doreto.com.br";
    settings = {
      headerStyle = "clean";
      fullWidth = true;
      layout = [
        {
          Media = {
            style = "row";
            columns = 4;
          };
        }
        {
          Apps = {
            header = false;
            style = "row";
            columns = 4;
          };
        }
        {
          Downloads = {
            style = "row";
            columns = 4;
          };
        }
      ];
    };
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
            "Photos" = {
              icon = "immich.png";
              href = "https://photos.local.doreto.com.br";
            };
          }
          {
            "FreshRSS" = rec {
              icon = "freshrss.png";
              href = "https://freshrss.local.doreto.com.br";
              siteMonitor = href;
            };
          }
          {
            "Readeck" = rec {
              icon = "readeck.png";
              href = "https://readeck.local.doreto.com.br";
              siteMonitor = href;
            };
          }
          {
            "Jellyfin" = rec {
              icon = "jellyfin.png";
              href = "https://jellyfin.local.doreto.com.br";
              siteMonitor = href;
            };
          }
          {
            "NextCloud" = rec {
              icon = "nextcloud.png";
              href = "https://nextcloud.local.doreto.com.br";
              siteMonitor = href;
            };
          }
          {
            "Calibre" = rec {
              icon = "calibre.png";
              href = "https://calibre.local.doreto.com.br";
              siteMonitor = href;
            };
          }
          {
            "Audiobooks" = rec {
              icon = "audiobookshelf.png";
              href = "https://audiobook.local.doreto.com.br";
              siteMonitor = href;
            };
          }
        ];
      }
      {
        Apps = [
          {
            Home = [
              {
                "HomeAssistant" = rec {
                  icon = "home-assistant.png";
                  href = "https://ha.local.doreto.com.br";
                  siteMonitor = href;
                };
              }
              {
                "Searx" = rec {
                  icon = "searxng.png";
                  href = "https://search.local.doreto.com.br";
                  siteMonitor = href;
                };
              }
            ];
          }
          {
            Code = [
              {
                "OpenCode" = rec {
                  icon = "opencode.png";
                  href = "https://opencode.local.doreto.com.br";
                  siteMonitor = href;
                };
              }
              {
                "Forgejo" = rec {
                  icon = "forgejo.png";
                  href = "https://git.local.doreto.com.br";
                  siteMonitor = href;
                };
              }
              {
                "VSCode" = rec {
                  icon = "vscode.png";
                  href = "https://code.local.doreto.com.br";
                  siteMonitor = href;
                };
              }
            ];
          }
          {
            AI = [
              {
                "LibreChat" = rec {
                  icon = "librechat.png";
                  href = "https://chat.local.doreto.com.br";
                  siteMonitor = href;
                };
              }
              {
                "InvokeAI" = {
                  icon = "midjourney-light.png";
                  href = "https://invokeai.local.doreto.com.br";
                };
              }
              {
                "Kokoro TTS" = rec {
                  icon = "kokoro-web.png";
                  href = "https://tts.local.doreto.com.br/web/";
                  siteMonitor = href;
                };
              }
            ];
          }
          {
            Admin = [
              {
                "Authentik" = rec {
                  icon = "authentik.png";
                  href = "https://auth.local.doreto.com.br";
                  siteMonitor = href;
                };
              }
              {
                "VictoriaLogs" = rec {
                  icon = "victorialogs.png";
                  href = "https://logs.local.doreto.com.br/select/vmui/";
                  siteMonitor = href;
                };
              }
              {
                "VictoriaMetrics" = rec {
                  icon = "victoriametrics.png";
                  href = "https://metrics.local.doreto.com.br/vmui/#/dashboards";
                  siteMonitor = href;
                };
              }
            ];
          }
        ];
      }
      {
        Downloads = [
          {
            "qBittorrent" = rec {
              icon = "qbittorrent.png";
              description = "yarr";
              href = "https://qbit.local.doreto.com.br";
              siteMonitor = href;
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
              siteMonitor = href;
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
              siteMonitor = href;
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
              siteMonitor = href;
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
              siteMonitor = href;
            };
          }
        ];
      }
    ];
  };
  systemd.services.homepage-dashboard.path = [ pkgs.unixtools.ping ];
}
