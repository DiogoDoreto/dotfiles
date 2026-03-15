{ pkgs, ... }:

# User provisioning notes:
# - FreshRSS auto-creates accounts on first login (http_auth_auto_register = true by default)
# - The FreshRSS username will equal the Authentik username (X-Authentik-Username)
# - New accounts are regular users. Grant admin via (uses shebang PHP, no php in PATH needed):
#     pkg=$(systemctl show freshrss-config --property=WorkingDirectory --value)
#     sudo -u freshrss DATA_PATH=/var/lib/freshrss "$pkg/cli/update-user.php" --user <name> --admin 1
# - Recovery if Authentik is unavailable: temporarily set authType = "form" and
#   passwordFile = "/var/lib/freshrss-pass.txt", then nixos-rebuild switch

{
  services.freshrss = {
    enable = true;
    extensions = with pkgs.freshrss-extensions; [
      youtube
      reading-time
    ];
    webserver = "caddy";
    virtualHost = "freshrss.local.doreto.com.br";
    baseUrl = "https://freshrss.local.doreto.com.br";
    # Auth is handled by Caddy forward_auth → Authentik (see extraConfig below).
    # passwordFile is not needed for http_auth mode.
    authType = "http_auth";
    defaultUser = "diogo";
  };

  # Allow FreshRSS to trust the Remote-User header from Caddy.
  # FreshRSS checks REMOTE_ADDR against TRUSTED_PROXY before accepting HTTP_REMOTE_USER.
  # In Caddy's php_fastcgi, REMOTE_ADDR is the actual client IP (not Caddy's IP), so we
  # must cover the full private range. This is safe because Caddy strips any client-supplied
  # Remote-User header before adding its own (see request_header -Remote-User above).
  services.phpfpm.pools.freshrss.phpEnv = {
    TRUSTED_PROXY = "127.0.0.1 ::1 10.0.0.0/8 172.16.0.0/12 192.168.0.0/16";
  };

  services.caddy.virtualHosts."freshrss.local.doreto.com.br".extraConfig = ''
    # Strip any client-supplied Remote-User to prevent identity injection
    request_header -Remote-User

    route {
      # Proxy outpost paths back to Authentik (needed for sign-out callbacks)
      reverse_proxy /outpost.goauthentik.io/* http://localhost:9000

      # Gate all non-API requests through Authentik's forward auth.
      # /api/* paths (GReader, Fever) use FreshRSS's own API password auth and bypass SSO.
      # The outpost HTTP listener is at localhost:9000 (authentik-nix Go server default).
      @notApi not path /api/*
      forward_auth @notApi http://localhost:9000 {
        uri /outpost.goauthentik.io/auth/caddy
        copy_headers X-Authentik-Username X-Authentik-Groups X-Authentik-Entitlements X-Authentik-Email X-Authentik-Name X-Authentik-Uid X-Authentik-Jwt X-Authentik-Meta-Jwks X-Authentik-Meta-Outpost X-Authentik-Meta-Provider X-Authentik-Meta-App X-Authentik-Meta-Version
        trusted_proxies private_ranges
      }

      # Map Authentik's username header to what FreshRSS expects ($SERVER['HTTP_REMOTE_USER'])
      @notApiRequest not path /api/*
      request_header @notApiRequest Remote-User {http.request.header.X-Authentik-Username}
    }
  '';
}
