{
  chungusProxyIp = "192.168.0.4";

  ports = {
    # standard / well-known
    dns = 53;
    http = 80;
    https = 443;
    publicCaddy = 8088; # localhost-only Cloudflare Tunnel ingress listener
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
    gitPages = 3010;
    gitPagesCaddy = 3011;
    gitPagesMetrics = 3012;
    home-assistant = 8123; # default; not configurable via NixOS option
    homepage = 8082;
    immich = 2283;
    invokeai = 9090;
    llama = 8080;
    jellyfin = 8096; # default; not configurable via NixOS option
    lidarr = 8686;
    nextcloud = 5387; # nginx listen port (proxied by caddy)
    kokoro = 8880;
    libreChat = 11112;
    openWebui = 11111;
    opencode = 32859;
    openssh = 17098;
    prowlarr = 9696;
    qbittorrent = 8079;
    radarr = 7878;
    readeck = 18084;
    searx = 8888;
    sonarr = 8989;
    vscodium = 32849;

    # monitoring
    victoriametrics = 8428;
    victorialogs = 9428;
    nodeExporter = 9100;
    systemdExporter = 9558;
    nvidiaExporter = 9835;
  };
}
