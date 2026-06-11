{ pkgs, ... }:

{
  services.freshrss = {
    enable = true;
    webserver = "caddy";
    virtualHost = "freshrss.local.doreto.com.br";
    baseUrl = "https://freshrss.local.doreto.com.br";
    # Auth is handled by Caddy forward_auth → Authentik (see extraConfig below).
    # passwordFile is not needed for http_auth mode.
    authType = "http_auth";
    defaultUser = "diogo";
  };

  # FreshRSS requires trusted_sources or TRUSTED_PROXY to be set before it will
  # trust HTTP_REMOTE_USER. REMOTE_ADDR in FastCGI is the client IP (Caddy passes it
  # through), so the LAN range must be listed here.
  services.phpfpm.pools.freshrss.phpEnv = {
    TRUSTED_PROXY = "127.0.0.1/32 ::1/128 192.168.0.0/16 10.0.0.0/8 172.16.0.0/12 100.64.0.0/10";
  };

  services.caddy.virtualHosts."freshrss.local.doreto.com.br".extraConfig = ''
    # Authentik's forwardHandleCaddy uses X-Forwarded-Host to match the application URL.
    # Caddy does not set this automatically; must be set before forward_auth runs.
    request_header X-Forwarded-Host {http.request.host}

    # Proxy outpost paths directly to Authentik (sign-out callbacks, login flow).
    # These must be excluded from forward_auth or the Authentik login redirect loops.
    @outpost path /outpost.goauthentik.io/*
    reverse_proxy @outpost http://localhost:9000

    # Gate all non-API, non-outpost requests through Authentik forward auth.
    # /api/* paths (GReader, Fever) use FreshRSS's own API password auth — bypass SSO.
    @needsAuth {
      not path /api/*
      not path /outpost.goauthentik.io/*
    }
    forward_auth @needsAuth http://localhost:9000 {
      uri /outpost.goauthentik.io/auth/caddy
      # Rename X-Authentik-Username → Remote-User so FreshRSS receives HTTP_REMOTE_USER.
      copy_headers X-Authentik-Username>Remote-User
      trusted_proxies private_ranges
    }
  '';
}
