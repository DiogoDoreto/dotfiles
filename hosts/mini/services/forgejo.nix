{
  config,
  lib,
  pkgs,
  ...
}:

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
        DEFAULT_ACTIONS_URL = "https://git.local.doreto.com.br";
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

  users.groups.gitea-runner = { };
  users.users.gitea-runner = {
    isSystemUser = true;
    group = "gitea-runner";
  };

  systemd.services.gitea-runner-mini = {
    after = [
      "caddy-cert-trust.service"
      "forgejo.service"
    ];
    wants = [
      "caddy-cert-trust.service"
      "forgejo.service"
    ];
    path = [ pkgs.rsync ];
    serviceConfig = {
      DynamicUser = lib.mkForce false;
      User = "gitea-runner";
      Group = "gitea-runner";
    };
    environment.SSL_CERT_FILE = "/etc/ssl/certs/ca-bundle-with-local-ca.crt";
  };

  systemd.services.forgejo = {
    after = [ "caddy-cert-trust.service" ];
    wants = [ "caddy-cert-trust.service" ];
    environment.SSL_CERT_FILE = "/etc/ssl/certs/ca-bundle-with-local-ca.crt";
  };

  systemd.services.forgejo-oauth-setup = {
    description = "Configure Authentik OAuth2 source in Forgejo";
    after = [
      "forgejo.service"
      "caddy-cert-trust.service"
    ];
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
      SSL_CERT_FILE = "/etc/ssl/certs/ca-bundle-with-local-ca.crt";
    };
    script = ''
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
    };
  };

  systemd.services.forgejo-action-mirror-setup = {
    description = "Create local git-pages Forgejo Action mirror";
    after = [
      "forgejo.service"
      "caddy.service"
      "caddy-cert-trust.service"
    ];
    wants = [
      "caddy.service"
      "caddy-cert-trust.service"
    ];
    requires = [ "forgejo.service" ];
    wantedBy = [ "multi-user.target" ];
    path = [
      pkgs.coreutils
      pkgs.curl
    ];
    environment = {
      SSL_CERT_FILE = "/etc/ssl/certs/ca-bundle-with-local-ca.crt";
    };
    script = ''
      set -euo pipefail

      API="https://git.local.doreto.com.br/api/v1"
      umask 077
      tmpdir="$(mktemp -d)"
      trap 'rm -rf "$tmpdir"' EXIT

      auth_header="$tmpdir/authorization-header"
      {
        printf 'Authorization: token '
        tr -d '\n' < "$CREDENTIALS_DIRECTORY/admin-api-token"
        printf '\n'
      } > "$auth_header"
      chmod 600 "$auth_header"

      org_status="$(curl --silent --show-error --output "$tmpdir/org.json" --write-out "%{http_code}" \
        --cacert "$SSL_CERT_FILE" \
        --header "@$auth_header" \
        --header "Accept: application/json" \
        "$API/orgs/actions")"

      case "$org_status" in
        200)
          echo "Forgejo organization actions already exists"
          ;;
        404)
          create_org_status="$(curl --silent --show-error --output "$tmpdir/create-org.json" --write-out "%{http_code}" \
            --request POST \
            --cacert "$SSL_CERT_FILE" \
            --header "@$auth_header" \
            --header "Accept: application/json" \
            --header "Content-Type: application/json" \
            --data '{"username":"actions","full_name":"Actions","description":"Local mirrors for Forgejo Actions","visibility":"public"}' \
            "$API/orgs")"
          if [ "$create_org_status" != "201" ]; then
            echo "Failed to create actions organization: HTTP $create_org_status" >&2
            cat "$tmpdir/create-org.json" >&2
            exit 1
          fi
          echo "Created Forgejo organization actions"
          ;;
        *)
          echo "Failed to check actions organization: HTTP $org_status" >&2
          cat "$tmpdir/org.json" >&2
          exit 1
          ;;
      esac

      repo_status="$(curl --silent --show-error --output "$tmpdir/repo.json" --write-out "%{http_code}" \
        --cacert "$SSL_CERT_FILE" \
        --header "@$auth_header" \
        --header "Accept: application/json" \
        "$API/repos/actions/git-pages")"

      case "$repo_status" in
        200)
          echo "Forgejo action mirror actions/git-pages already exists"
          ;;
        404)
          migrate_status="$(curl --silent --show-error --output "$tmpdir/migrate.json" --write-out "%{http_code}" \
            --request POST \
            --cacert "$SSL_CERT_FILE" \
            --header "@$auth_header" \
            --header "Accept: application/json" \
            --header "Content-Type: application/json" \
            --data '{"clone_addr":"https://codeberg.org/git-pages/action.git","mirror":true,"private":false,"repo_name":"git-pages","repo_owner":"actions","service":"git","wiki":false,"issues":false,"pull_requests":false,"releases":false}' \
            "$API/repos/migrate")"
          case "$migrate_status" in
            200|201)
              echo "Created Forgejo action mirror actions/git-pages"
              ;;
            409)
              echo "Forgejo action mirror actions/git-pages already exists"
              ;;
            *)
              echo "Failed to create actions/git-pages mirror: HTTP $migrate_status" >&2
              cat "$tmpdir/migrate.json" >&2
              exit 1
              ;;
          esac
          ;;
        *)
          echo "Failed to check actions/git-pages repository: HTTP $repo_status" >&2
          cat "$tmpdir/repo.json" >&2
          exit 1
          ;;
      esac
    '';
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      User = "forgejo";
      Group = "forgejo";
      LoadCredential = [ "admin-api-token:/etc/secrets/forgejo/admin-api-token" ];
    };
  };
}
