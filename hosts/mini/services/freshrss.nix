{ pkgs, ... }:

# User provisioning notes:
# - FreshRSS auto-creates accounts on first login (http_auth_auto_register = true by default)
# - The FreshRSS username will equal the Authentik username (X-Authentik-Username)
# - New accounts are regular users. Grant admin via:
#     sudo -u freshrss DATA_PATH=/var/lib/freshrss php \
#       /run/current-system/sw/share/freshrss/cli/update-user.php --user <name> --admin 1
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
  };

  # Allow FreshRSS to trust the Remote-User header from Caddy.
  # FreshRSS only trusts this header if REMOTE_ADDR is in the TRUSTED_PROXY list.
  # Caddy → php-fpm via Unix socket sets REMOTE_ADDR=127.0.0.1.
  services.phpfpm.pools.freshrss.phpEnv = {
    TRUSTED_PROXY = "127.0.0.1";
  };

  services.caddy.virtualHosts."freshrss.local.doreto.com.br".extraConfig = ''
    # Strip any client-supplied Remote-User to prevent identity injection
    request_header -Remote-User

    route {
      # Proxy outpost paths back to Authentik (needed for sign-out callbacks)
      reverse_proxy /outpost.goauthentik.io/* http://[::1]:9001

      # Gate all non-API requests through Authentik's forward auth.
      # /api/* paths (GReader, Fever) use FreshRSS's own API password auth and bypass SSO.
      # The outpost HTTP listener is at [::1]:9001 (authentik-nix worker default).
      @notApi not path /api/*
      forward_auth @notApi http://[::1]:9001 {
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
