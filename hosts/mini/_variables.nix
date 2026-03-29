{
  ports = {
    # standard / well-known
    dns = 53;
    http = 80;
    https = 443;
    mdns = 5353;
    haHomekitBridge = [
      21064
      21065
    ]; # TCP range opened for HomeAssistant HomeKit integration

    # services
    audiobookshelf = 18090;
    authentik = 9000; # authentik internal HTTP; default from authentik-nix module
    bazarr = 6767;
    calibre = 18083;
    forgejo = 3000;
    forgejoSsh = 2222;
    home-assistant = 8123; # default; not configurable via NixOS option
    homepage = 8082;
    invokeai = 9090;
    jellyfin = 8096; # default; not configurable via NixOS option
    lidarr = 8686;
    nextcloud = 5387; # nginx listen port (proxied by caddy)
    openWebui = 11111;
    opencode = 32859;
    openssh = 17098;
    prowlarr = 9696;
    qbittorrent = 8079;
    radarr = 7878;
    searx = 8888;
    sonarr = 8989;
    vscodium = 32849;
  };
}
