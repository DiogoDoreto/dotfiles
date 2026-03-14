{
  ports = {
    authentik = 9443; # authentik internal HTTPS; default; not configurable via NixOS option
    audiobookshelf = 18090;
    bazarr = 6767;
    calibre = 18083;
    home-assistant = 8123; # default; not configurable via NixOS option
    homepage = 8082;
    jellyfin = 8096; # default; not configurable via NixOS option
    lidarr = 8686;
    nextcloud = 5387; # nginx listen port (proxied by caddy)
    openWebui = 11111;
    opencode = 32859;
    prowlarr = 9696;
    qbittorrent = 8079;
    radarr = 7878;
    searx = 8888;
    sonarr = 8989;
    vite = 5173; # local dev server
    vscodium = 32849;
  };
}
