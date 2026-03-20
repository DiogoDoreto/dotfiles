{ config, pkgs, ... }:

let
  vars = import ../_variables.nix;
in

{
  services.forgejo = {
    enable = true;
    package = pkgs.forgejo;
    settings = {
      server = {
        HTTP_ADDR = "127.0.0.1"; # localhost only — Caddy proxies
        HTTP_PORT = vars.ports.forgejo;
        DOMAIN = "git.local.doreto.com.br";
        ROOT_URL = "https://git.local.doreto.com.br/";
        SSH_DOMAIN = "git.local.doreto.com.br";
        SSH_PORT = vars.ports.forgejoSsh;
        START_SSH_SERVER = true;
      };
      service = {
        ALLOW_ONLY_EXTERNAL_REGISTRATION = true;
        DISABLE_REGISTRATION = true;
        ENABLE_INTERNAL_SIGNIN = false;
        SHOW_REGISTRATION_BUTTON = false;
      };
      oauth2_client = {
        ENABLE_AUTO_REGISTRATION = true; # auto-create accounts for OAuth2 users
      };
      openid = {
        ENABLE_OPENID_SIGNUP = true;
      };
      session = {
        COOKIE_SECURE = true;
      };
      actions = {
        ENABLED = true;
      };
    };
  };

  # Runner token must be provisioned manually:
  # 1. Go to https://git.local.doreto.com.br/admin/actions/runners
  # 2. Copy the registration token
  # 3. sudo install -m 600 -o root /dev/stdin /etc/secrets/forgejo/runner-token
  #    TOKEN=(paste token, then Ctrl+D)
  services.gitea-actions-runner.instances.mini = {
    enable = true;
    url = "https://git.local.doreto.com.br";
    name = "mini";
    tokenFile = "/etc/secrets/forgejo/runner-token";
    labels = [ "nix:host" ];
  };

  systemd.services.gitea-runner-mini = {
    serviceConfig.LoadCredential = [
      "caddy-ca:/var/lib/caddy/.local/share/caddy/pki/authorities/local/root.crt"
    ];
    environment.SSL_CERT_FILE = "%d/caddy-ca";
  };

  systemd.services.forgejo = {
    serviceConfig.LoadCredential = [
      "caddy-ca:/var/lib/caddy/.local/share/caddy/pki/authorities/local/root.crt"
    ];
    environment.SSL_CERT_FILE = "%d/caddy-ca";
  };

  systemd.services.forgejo-oauth-setup = {
    description = "Configure Authentik OAuth2 source in Forgejo";
    after = [ "forgejo.service" ];
    requires = [ "forgejo.service" ];
    wantedBy = [ "multi-user.target" ];
    path = [
      config.services.forgejo.package
      pkgs.gnugrep
    ];
    environment = {
      USER = "forgejo";
      HOME = "/var/lib/forgejo";
      FORGEJO_WORK_DIR = "/var/lib/forgejo";
      FORGEJO_CUSTOM = "/var/lib/forgejo/custom";
    };
    script = ''
      # LoadCredential makes the Caddy CA cert available via $CREDENTIALS_DIRECTORY
      # (systemd reads it as root, so forgejo user doesn't need direct access)
      export SSL_CERT_FILE="$CREDENTIALS_DIRECTORY/caddy-ca"

      if forgejo admin auth list | grep -qw "authentik"; then
        echo "Auth source 'authentik' already exists, skipping"
        exit 0
      fi

      forgejo admin auth add-oauth \
        --name authentik \
        --provider openidConnect \
        --auto-discover-url https://auth.local.doreto.com.br/application/o/forgejo/.well-known/openid-configuration \
        --key "$CLIENT_ID" \
        --secret "$CLIENT_SECRET" \
        --scopes "openid email profile"
    '';
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      User = "forgejo";
      Group = "forgejo";
      WorkingDirectory = "/var/lib/forgejo";
      EnvironmentFile = "/etc/secrets/forgejo/oauth";
      LoadCredential = "caddy-ca:/var/lib/caddy/.local/share/caddy/pki/authorities/local/root.crt";
    };
  };
}
