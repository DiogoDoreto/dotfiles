{
  config,
  lib,
  pkgs,
  ...
}:

let
  vars = import ../_variables.nix;
  actionMirrorRepos = [
    "cache"
    "checkout"
    "download-artifact"
    "forgejo-release"
    "git-pages"
    "setup-go"
    "setup-node"
    "setup-python"
    "upload-artifact"
  ];
  actionRunnerHostPackages = with pkgs; [
    bash
    coreutils
    curl
    diffutils
    file
    findutils
    gawk
    gitMinimal
    gnugrep
    gnupatch
    gnused
    gnutar
    gzip
    jq
    nix
    nodejs
    openssh
    rsync
    unzip
    wget
    which
    xz
    zip
    zstd
  ];
  localCaBundle = "/etc/ssl/certs/ca-bundle-with-local-ca.crt";
  # v15.0.4 is incompatible with the patch below, so let's stay at v15.0.3 until
  # v16 arrives and then the patch is not needed anymore
  # https://github.com/NixOS/nixpkgs/pull/542602
  forgejoPinned = pkgs.callPackage (import (pkgs.path + "/pkgs/by-name/fo/forgejo/generic.nix") {
    version = "15.0.3";
    hash = "sha256-tGZ83TEG6iyZd5mfSuSvVkmUJINWLN661YpOk1+dgbM=";
    npmDepsHash = "sha256-BZSYjEsjUqMYWu3EUP+K35hqSOniv8Y6ek5bEC2vTPg=";
    vendorHash = "sha256-z3YTjt+SM9yPCsJdfSQbTpy3vRiXaFV2QMz1y6J6k/Q=";
    lts = true;
  }) { };
  forgejoWithGitPagesPreviewAuth = forgejoPinned.overrideAttrs (oldAttrs: {
    pname = "${oldAttrs.pname}-pr-12727";
    patches = (oldAttrs.patches or [ ]) ++ [
      (pkgs.fetchpatch {
        name = "forgejo-pr-12727-actions-run-api.patch";
        url = "https://codeberg.org/forgejo/forgejo/pulls/12727.patch";
        excludes = [ "tests/integration/actions_token_metadata_test.go" ];
        hash = "sha256-04JxQDnfpXUErpdAfTedjn6QmEIxztMmz9ScimwL+TA=";
      })
    ];
    passthru = (oldAttrs.passthru or { }) // {
      gitPagesPreviewAuthorizationPatch = "forgejo-pr-12727";
    };
  });
in

{
  services.forgejo = {
    enable = true;
    package = forgejoWithGitPagesPreviewAuth;
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

  # Runner tokens must be provisioned manually:
  # 1. Go to https://git.local.doreto.com.br/admin/actions/runners
  # 2. Copy the registration token
  # 3. sudo install -m 600 -o root /dev/stdin /etc/secrets/forgejo/runner-token
  #    TOKEN=(paste token, then Ctrl+D)
  services.gitea-actions-runner.instances = {
    mini = {
      enable = true;
      url = "https://git.local.doreto.com.br";
      name = "mini";
      tokenFile = "/etc/secrets/forgejo/runner-token";
      labels = [ "nix:host" ];
      hostPackages = actionRunnerHostPackages;
    };

    # Ubuntu runner: executes action jobs inside containers using
    # the catthehacker/ubuntu images (which include git, node, and other
    # common CI tooling). The runner user gets access to the podman socket
    # via the `podman` supplementary group (handled by the module — podman
    # provides a Docker-compatible API socket), enabling Docker-in-Docker
    # workflows (building/pushing images from CI).
    ubuntu = {
      enable = true;
      url = "https://git.local.doreto.com.br";
      name = "ubuntu";
      tokenFile = "/etc/secrets/forgejo/runner-token";
      labels = [
        "ubuntu-latest:docker://catthehacker/ubuntu:act-latest"
        "ubuntu-24.04:docker://catthehacker/ubuntu:act-24.04"
        "ubuntu-22.04:docker://catthehacker/ubuntu:act-22.04"
      ];
      # See `gitea-runner generate-config` for the schema.
      settings = {
        container.options = "--mount type=bind,source=/etc/ssl/certs,target=/etc/ssl/certs,readonly";
        container.valid_volumes = [ "/etc/ssl/certs" ];
        # Node.js does not read the system CA bundle by default, so the
        # bind-mounted local CA is ignored by JS-based actions. Point
        # NODE_EXTRA_CA_CERTS (and SSL_CERT_FILE for everything else) at
        # the bundle that includes the local Caddy CA.
        runner.envs = {
          NODE_EXTRA_CA_CERTS = "/etc/ssl/certs/ca-bundle-with-local-ca.crt";
          SSL_CERT_FILE = "/etc/ssl/certs/ca-bundle-with-local-ca.crt";
        };
      };
    };
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
    requires = [ "caddy-cert-trust.service" ];
    wants = [
      "caddy-cert-trust.service"
      "forgejo.service"
    ];
    serviceConfig = {
      DynamicUser = lib.mkForce false;
      User = "gitea-runner";
      Group = "gitea-runner";
    };
    environment.SSL_CERT_FILE = localCaBundle;
  };

  systemd.services.gitea-runner-ubuntu = {
    after = [
      "caddy-cert-trust.service"
    ];
    requires = [ "caddy-cert-trust.service" ];
    wants = [
      "caddy-cert-trust.service"
    ];
    serviceConfig = {
      DynamicUser = lib.mkForce false;
      User = "gitea-runner";
      Group = "gitea-runner";
    };
    environment.SSL_CERT_FILE = localCaBundle;
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
    description = "Create local Forgejo Action mirrors";
    after = [
      "network-online.target"
      "forgejo.service"
      "caddy.service"
      "caddy-cert-trust.service"
    ];
    wants = [
      "network-online.target"
      "caddy.service"
      "caddy-cert-trust.service"
    ];
    requires = [ "forgejo.service" ];
    wantedBy = [ "multi-user.target" ];
    path = [
      pkgs.coreutils
      pkgs.curl
      pkgs.jq
    ];
    environment = {
      SSL_CERT_FILE = "/etc/ssl/certs/ca-bundle-with-local-ca.crt";
    };
    script = ''
      set -euo pipefail

      HOST="git.local.doreto.com.br"
      API="https://$HOST/api/v1"
      umask 077
      tmpdir="$(mktemp -d)"
      trap 'rm -rf "$tmpdir"' EXIT

      ready_status=""
      for attempt in $(seq 1 60); do
        ready_status="$(curl --silent --show-error --output "$tmpdir/version.json" --write-out "%{http_code}" \
          --cacert "$SSL_CERT_FILE" \
          "$API/version" 2>"$tmpdir/version.err" || true)"
        if [ "$ready_status" = "200" ]; then
          break
        fi
        sleep 2
      done

      if [ "$ready_status" != "200" ]; then
        echo "Forgejo API did not become ready at $API: HTTP $ready_status" >&2
        cat "$tmpdir/version.err" >&2
        cat "$tmpdir/version.json" >&2
        exit 1
      fi

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

      create_mirror() {
        repo="$1"
        clone_addr="https://code.forgejo.org/actions/$repo.git"
        migrate_json="$tmpdir/migrate-$repo.json"

        migrate_status="$(curl --silent --show-error --output "$migrate_json" --write-out "%{http_code}" \
          --request POST \
          --cacert "$SSL_CERT_FILE" \
          --header "@$auth_header" \
          --header "Accept: application/json" \
          --header "Content-Type: application/json" \
          --data "$(jq -nc \
            --arg clone_addr "$clone_addr" \
            --arg repo "$repo" \
            '{"clone_addr":$clone_addr,"mirror":true,"private":false,"repo_name":$repo,"repo_owner":"actions","service":"git","wiki":false,"issues":false,"pull_requests":false,"releases":false}')" \
          "$API/repos/migrate")"
        case "$migrate_status" in
          200|201)
            echo "Created Forgejo action mirror actions/$repo from $clone_addr"
            ;;
          409)
            echo "Forgejo action mirror actions/$repo already exists"
            ;;
          *)
            echo "Failed to create actions/$repo mirror from $clone_addr: HTTP $migrate_status" >&2
            cat "$migrate_json" >&2
            exit 1
            ;;
        esac
      }

      ensure_mirror() {
        repo="$1"
        clone_addr="https://code.forgejo.org/actions/$repo.git"
        repo_json="$tmpdir/repo-$repo.json"

        repo_status="$(curl --silent --show-error --output "$repo_json" --write-out "%{http_code}" \
          --cacert "$SSL_CERT_FILE" \
          --header "@$auth_header" \
          --header "Accept: application/json" \
          "$API/repos/actions/$repo")"

        case "$repo_status" in
          200)
            mirror="$(jq -r '.mirror // false' "$repo_json")"
            original_url="$(jq -r '.original_url // ""' "$repo_json")"
            if [ "$mirror" != "true" ]; then
              echo "actions/$repo exists but is not a mirror; refusing to replace it" >&2
              exit 1
            fi
            if [ "$original_url" != "$clone_addr" ]; then
              if [ -z "$original_url" ]; then
                echo "actions/$repo mirror exists, but Forgejo did not report original_url; refusing to replace it blindly" >&2
                exit 1
              fi

              echo "Replacing actions/$repo mirror source: $original_url -> $clone_addr"
              delete_status="$(curl --silent --show-error --output "$tmpdir/delete-$repo.json" --write-out "%{http_code}" \
                --request DELETE \
                --cacert "$SSL_CERT_FILE" \
                --header "@$auth_header" \
                "$API/repos/actions/$repo")"
              if [ "$delete_status" != "204" ]; then
                echo "Failed to delete actions/$repo before recreating mirror: HTTP $delete_status" >&2
                cat "$tmpdir/delete-$repo.json" >&2
                exit 1
              fi
              create_mirror "$repo"
            else
              echo "Forgejo action mirror actions/$repo already exists from $clone_addr"
            fi
            ;;
          404)
            create_mirror "$repo"
            ;;
          *)
            echo "Failed to check actions/$repo repository: HTTP $repo_status" >&2
            cat "$repo_json" >&2
            exit 1
            ;;
        esac
      }

      for repo in ${lib.escapeShellArgs actionMirrorRepos}; do
        ensure_mirror "$repo"
      done
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
