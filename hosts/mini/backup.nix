{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [ borgbackup ];

  services.borgbackup.jobs.root = {
    repo = "/root/test-bkp";
    doInit = true;
    compression = "auto,lzma";
    startAt = "daily";
    encryption = {
      mode = "repokey-blake2";
      passCommand = "cat /etc/secrets/borgbackup";
    };
    # TODO use when setting up BorgBase remote
    # environment = {
    #   BORG_RSH = "ssh -i /root/.ssh/id_ed25519_borgbase";
    # };
    paths = [
      "/var/lib/"
      "/var/lib/open-webui/data" # it's a symlink
      "/home/dog/projects/home-assistant-config/config/"
    ];
    patterns = [
      "+ /var/lib/dog/media/Books"

      "+ /var/lib/freshrss/config.php"
      "+ /var/lib/freshrss/users/*/config.php"
      "+ /var/lib/freshrss/users/*/db.sqlite"

      "- /var/lib/nextcloud/data/*/cache"
      "- /var/lib/nextcloud/data/*/files"
      "- /var/lib/nextcloud/data/*/files_trashbin"
      "- /var/lib/nextcloud/data/*/files_versions"
      "- /var/lib/nextcloud/data/*/uploads"
      "+ /var/lib/nextcloud/data"
      "+ /var/lib/nextcloud/config"

      "- /var/lib/open-webui/data/audit.log"
      "- /var/lib/open-webui/data/cache"
      "+ /var/lib/open-webui/data"

      "+ /var/lib/radarr/.config/Radarr/config.xml"
      "+ /var/lib/radarr/.config/Radarr/radarr.db"

      "+ /var/lib/sonarr/.config/NzbDrone/config.xml"
      "+ /var/lib/sonarr/.config/NzbDrone/sonarr.db"

      "+ /var/lib/jellyfin/config"
      "+ /var/lib/jellyfin/data/jellyfin.db"
      "+ /var/lib/jellyfin/root"

      "+ /home/dog/projects/home-assistant-config/config/*.yaml"
      "+ /home/dog/projects/home-assistant-config/config/home-assistant_v2.db"

      # exclude everything that has not been explicitly added above
      "- **"
    ];
  };
}
