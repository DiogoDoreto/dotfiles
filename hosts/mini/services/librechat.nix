{ pkgs, ... }:

let
  vars = import ../_variables.nix;
  p = builtins.mapAttrs (_: toString) vars.ports;

  configFile = (pkgs.formats.yaml { }).generate "librechat.yaml" {
    version = "1.3.6";
    cache = true;
    endpoints = {
      custom = [
        {
          name = "llama-swap";
          apiKey = "sk-dummy";
          baseURL = "http://chungus.home:8080/v1";
          models = {
            default = [ "qwen3.5-35b-a3b-coding" ];
            fetch = true;
          };
          titleConvo = true;
          titleModel = "qwen3.5-35b-a3b-coding";
          modelDisplayLabel = "llama-swap";
        }
      ];
    };
  };
in

{
  # LibreChat setup (manual, one-time):
  #   1. In Authentik, create an OAuth2/OIDC provider for librechat
  #      with redirect URI: https://chat.local.doreto.com.br/oauth/openid/callback
  #      Subject mode: Based on the User's Email
  #   2. Create an application bound to that provider
  #   3. Append credentials to the secrets file:
  #        sudo tee -a /etc/secrets/librechat/env <<EOF
  #        OPENID_CLIENT_ID=<client id>
  #        OPENID_CLIENT_SECRET=<client secret>
  #        EOF
  #      (OPENID_SESSION_SECRET and crypto keys are already present from initial setup)
  #
  # To manually upgrade:
  #   sudo podman pull ghcr.io/danny-avila/librechat:latest
  #   sudo systemctl restart podman-librechat.service

  # Create the podman network so librechat containers can reach each other.
  # DNS is disabled to avoid conflicting with dnsmasq (which binds 0.0.0.0:53);
  # container-to-container resolution uses --add-host / static IPs instead.
  systemd.services.podman-create-librechat-network = {
    description = "Create podman network for LibreChat";
    wantedBy = [ "multi-user.target" ];
    before = [
      "podman-librechat.service"
      "podman-librechat-mongodb.service"
    ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = pkgs.writeShellScript "create-librechat-network" ''
        if ! ${pkgs.podman}/bin/podman network exists librechat; then
          ${pkgs.podman}/bin/podman network create --disable-dns librechat
        fi
      '';
    };
  };

  systemd.services.podman-librechat = {
    requires = [ "podman-create-librechat-network.service" ];
    after = [ "podman-create-librechat-network.service" ];
  };

  systemd.services.podman-librechat-mongodb = {
    requires = [ "podman-create-librechat-network.service" ];
    after = [ "podman-create-librechat-network.service" ];
  };

  virtualisation.oci-containers.containers.librechat-mongodb = {
    image = "mongo:7";
    volumes = [
      "/var/lib/librechat/mongodb:/data/db"
    ];
    extraOptions = [
      "--network=librechat"
      "--ip=10.89.0.10"
    ];
  };

  virtualisation.oci-containers.containers.librechat = {
    image = "ghcr.io/danny-avila/librechat:latest";
    ports = [ "${p.libreChat}:3080" ];
    volumes = [
      "/var/lib/librechat/data:/app/data/files"
      "${configFile}:/app/librechat.yaml:ro"
      "/etc/ssl/certs/ca-bundle-with-local-ca.crt:/etc/ssl/certs/ca-bundle-with-local-ca.crt:ro"
    ];
    environment = {
      HOST = "0.0.0.0";
      PORT = "3080";
      NODE_ENV = "production";
      MONGO_URI = "mongodb://librechat-mongodb:27017/LibreChat";
      # Trust Caddy's self-signed CA inside the container
      NODE_EXTRA_CA_CERTS = "/etc/ssl/certs/ca-bundle-with-local-ca.crt";
      # Public URL — needed for OIDC callback redirect construction
      DOMAIN_CLIENT = "https://chat.local.doreto.com.br";
      DOMAIN_SERVER = "https://chat.local.doreto.com.br";
      # Authentik SSO
      ALLOW_SOCIAL_LOGIN = "true";
      OPENID_ISSUER = "https://auth.local.doreto.com.br/application/o/librechat/.well-known/openid-configuration";
      OPENID_CALLBACK_URL = "/oauth/openid/callback";
      OPENID_SCOPE = "openid profile email";
      OPENID_BUTTON_LABEL = "Login with Authentik";
      OPENID_IMAGE_URL = "https://cdn.jsdelivr.net/gh/selfhst/icons/png/authentik.png";
      OPENID_USE_END_SESSION_ENDPOINT = "true";
    };
    # CREDS_KEY, CREDS_IV, JWT_SECRET, JWT_REFRESH_SECRET (and API keys) come from here
    environmentFiles = [ "/etc/secrets/librechat/env" ];
    extraOptions = [
      "--network=librechat"
      # Resolve mongodb by IP since DNS is disabled on the librechat network
      # (aardvark-dns conflicts with the host dnsmasq on 0.0.0.0:53)
      "--add-host=librechat-mongodb:10.89.0.10"
      # dnsmasq on mini resolves *.local.doreto.com.br; containers don't use
      # the host's 127.0.0.1 resolver so we point directly to the LAN IP
      "--dns=192.168.0.2"
    ];
  };

  systemd.tmpfiles.rules = [
    "d /var/lib/librechat 0755 root root -"
    "d /var/lib/librechat/mongodb 0755 root root -"
    "d /var/lib/librechat/data 0755 root root -"
    "d /etc/secrets/librechat 0700 root root -"
  ];
}
