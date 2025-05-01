{ pkgs, ... }:

{
  users = {
    groups.media = {
      members = [
        "dog"
        "jellyfin"
        "lidarr"
        "sonarr"
        "radarr"
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
      ExecStart = "${pkgs.qbittorrent-nox}/bin/qbittorrent-nox --profile=/var/lib/qbittorrent --webui-port=8079";
      Restart = "on-failure";
      RestartSec = 5;
      User = "qbittorrent";
      Group = "media";
      UMask = "007"; # to create files in mode 660
    };
  };

  services.bazarr.enable = true;
  services.lidarr.enable = true;
  services.prowlarr.enable = true;
  services.radarr.enable = true;
  services.sonarr.enable = true;

  services.jellyfin.enable = true;

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
