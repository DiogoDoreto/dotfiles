{ ... }:

let
  vars = import ../_variables.nix;
  p = builtins.mapAttrs (_: toString) vars.ports;
in

{
  # Authentik setup (manual, one-time):
  #   1. In Authentik, create an OAuth2/OpenID provider for open-webui
  #      with redirect URI: https://ai.local.doreto.com.br/oauth/oidc/callback
  #   2. Create an application bound to that provider
  #   3. Store the credentials:
  #        sudo install -m 600 -o root /dev/stdin /etc/secrets/open-webui/oauth
  #        OAUTH_CLIENT_ID=<client id>
  #        OAUTH_CLIENT_SECRET=<client secret>
  #        (then Ctrl+D)
  virtualisation.oci-containers.containers.open-webui = {
    # To manually upgrade:
    #   sudo podman pull ghcr.io/open-webui/open-webui:main
    #   sudo systemctl restart podman-open-webui.service
    image = "ghcr.io/open-webui/open-webui:main";
    ports = [ "${p.openWebui}:8080" ];
    volumes = [
      "/var/lib/open-webui:/app/backend/data"
      # Mount the combined CA bundle (system CAs + Caddy CA) so the container
      # can verify TLS to auth.local.doreto.com.br — built by caddy-cert-trust.service
      "/etc/ssl/certs/ca-bundle-with-local-ca.crt:/etc/ssl/certs/ca-bundle-with-local-ca.crt:ro"
    ];
    environment = {
      # llama-swap on chungus exposes an OpenAI-compatible API on port 8080
      OPENAI_API_BASE_URL = "http://192.168.0.3:8080/v1";
      OPENAI_API_KEY = "sk-dummy";
      # Trust Caddy's self-signed CA inside the container
      SSL_CERT_FILE = "/etc/ssl/certs/ca-bundle-with-local-ca.crt";
      # Authentik SSO
      OPENID_PROVIDER_URL = "https://auth.local.doreto.com.br/application/o/open-webui/.well-known/openid-configuration";
      OAUTH_PROVIDER_NAME = "Authentik";
      OAUTH_SCOPES = "openid email profile";
      ENABLE_OAUTH_SIGNUP = "true";
      OAUTH_MERGE_ACCOUNTS_BY_EMAIL = "true";
    };
    # OAUTH_CLIENT_ID and OAUTH_CLIENT_SECRET come from the secrets file
    environmentFiles = [ "/etc/secrets/open-webui/oauth" ];
    extraOptions = [
      # dnsmasq on mini resolves *.local.doreto.com.br; containers don't use
      # the host's 127.0.0.1 resolver so we point directly to the LAN IP
      "--dns=192.168.0.2"
    ];
  };

  systemd.tmpfiles.rules = [
    "d /var/lib/open-webui 0755 root root -"
    "d /etc/secrets/open-webui 0700 root root -"
  ];
}
