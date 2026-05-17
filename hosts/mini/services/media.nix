{ pkgs, ... }:

let
  vars = import ../_variables.nix;
in

{
  users = {
    groups.media = {
      members = [
        "dog"
        "audiobookshelf"
        "calibre-web"
        "jellyfin"
        "lidarr"
        "sonarr"
        "radarr"
        "nextcloud"
      ];
    };

    users.qbittorrent = {
      isSystemUser = true;
      group = "media";
    };
  };

  systemd.services.qbittorrent = {
    description = "qBittorrent Service";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      ExecStart = "${pkgs.qbittorrent-nox}/bin/qbittorrent-nox --profile=/var/lib/qbittorrent --webui-port=${toString vars.ports.qbittorrent}";
      Restart = "on-failure";
      RestartSec = 5;
      User = "qbittorrent";
      Group = "media";
      UMask = "007"; # to create files in mode 660
    };
  };

  services.bazarr.enable = true;
  services.bazarr.listenPort = vars.ports.bazarr;

  services.lidarr.enable = true;
  services.lidarr.settings.server.port = vars.ports.lidarr;

  services.prowlarr.enable = true;
  services.prowlarr.settings.server.port = vars.ports.prowlarr;

  services.radarr.enable = true;
  services.radarr.settings.server.port = vars.ports.radarr;

  services.sonarr.enable = true;
  services.sonarr.settings.server.port = vars.ports.sonarr;

  services.jellyfin.enable = true;

  # Authentik setup (manual, one-time):
  # 1. Create a Proxy Provider (Forward auth / Single application) for https://calibre.local.doreto.com.br
  # 2. Create an Application bound to that provider
  # 3. Assign the application to the embedded outpost (localhost:9000)
  # 4. Ensure the calibre-web user account name matches the Authentik username (e.g. "diogo")
  services.calibre-web = {
    enable = true;
    listen.port = vars.ports.calibre;
    group = "media";
    options = {
      calibreLibrary = "/var/lib/dog/media/Books";
      enableBookConversion = true;
      enableKepubify = true;
      enableBookUploading = true;
      reverseProxyAuth = {
        enable = true;
        header = "X-Authentik-Username";
      };
    };
  };

  # Authentik setup:
  # - Setup App with OAuth2/OpenID Provider in Authentik
  # - Go to https://audiobook.local.doreto.com.br/audiobookshelf/config/authentication
  # - Set issuer to https://auth.local.doreto.com.br/application/o/audiobookshelf/ and auto populate the other fields
  # - Then copy client ID and client secret from Authentik
  # ref: https://www.audiobookshelf.org/guides/oidc_authentication/
  services.audiobookshelf = {
    enable = true;
    port = vars.ports.audiobookshelf;
  };

  systemd.tmpfiles.settings.mediaDirs = {
    "/var/lib/qbittorrent".d = {
      mode = "700";
      user = "qbittorrent";
      group = "media";
    };
    "/var/lib/dog/media".d = {
      mode = "750";
      user = "root";
      group = "media";
    };
    "/var/lib/dog/media/downloads".d = {
      mode = "2770";
      user = "root";
      group = "media";
    };
    "/var/lib/dog/media/Media".d = {
      mode = "2770";
      user = "root";
      group = "media";
    };
    "/var/lib/dog/media/AudioBooks".d = {
      mode = "750";
      user = "audiobookshelf";
      group = "media";
    };
    "/var/lib/dog/media/Books".d = {
      mode = "750";
      user = "calibre-web";
      group = "media";
    };
    "/var/lib/dog/media/Movies".d = {
      mode = "750";
      user = "radarr";
      group = "media";
    };
    "/var/lib/dog/media/Music".d = {
      mode = "750";
      user = "lidarr";
      group = "media";
    };
    "/var/lib/dog/media/TV Shows".d = {
      mode = "750";
      user = "sonarr";
      group = "media";
    };
  };

}
