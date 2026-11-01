{ pkgs, ... }:

let
  vars = import ../_variables.nix;
in
{
  # Persistent runtime secret; never evaluate secret contents into the store.
  systemd.services.grafana-secret-setup = {
    description = "Initialize Grafana signing key (preserve existing key)";
    before = [ "grafana.service" ];
    requiredBy = [ "grafana.service" ];
    serviceConfig = {
      Type = "oneshot";
      UMask = "0077";
    };
    path = [
      pkgs.coreutils
      pkgs.openssl
    ];
    script = ''
      set -eu
      install -d -m 0700 -o root -g root /etc/secrets/grafana
      key=/etc/secrets/grafana/secret-key
      if [ ! -e "$key" ]; then
        # Atomic rename, so interruption cannot leave a partial persistent key.
        trap 'rm -f "$key.new"' EXIT
        openssl rand -hex 32 > "$key.new"
        chmod 0600 "$key.new"
        mv "$key.new" "$key"
      fi
      test -s "$key"
      chown root:root "$key"
      chmod 0600 "$key"
    '';
  };
  systemd.services.grafana.serviceConfig.LoadCredential = [
    "secret-key:/etc/secrets/grafana/secret-key"
  ];

  services.grafana = {
    enable = true;
    openFirewall = false;
    # Grafana 13 distributes Prometheus separately; provisioning a datasource
    # does not install its plugin. Keep it pinned instead of downloading at boot.
    declarativePlugins = [ pkgs.grafanaPlugins.prometheus ];
    settings = {
      server = {
        http_addr = "127.0.0.1";
        http_port = vars.ports.grafana;
        domain = "grafana.local.doreto.com.br";
        root_url = "https://grafana.local.doreto.com.br/";
      };
      security = {
        secret_key = "$__file{/run/credentials/grafana.service/secret-key}";
        disable_initial_admin_creation = true;
        cookie_secure = true;
        cookie_samesite = "lax";
      };
      auth.disable_login_form = true;
      "auth.basic".enabled = false;
      "auth.anonymous".enabled = false;
      "auth.proxy" = {
        enabled = true;
        header_name = "X-WEBAUTH-USER";
        header_property = "username";
        auto_sign_up = true;
        whitelist = "127.0.0.1";
        enable_login_token = false;
      };
      users = {
        allow_sign_up = false;
        allow_org_create = false;
        auto_assign_org = true;
        auto_assign_org_role = "Viewer";
      };
      analytics = {
        reporting_enabled = false;
        check_for_updates = false;
        check_for_plugin_updates = false;
      };
      plugins.preinstall_disabled = true;
    };
    provision = {
      enable = true;
      datasources.settings = {
        apiVersion = 1;
        datasources = [
          {
            name = "VictoriaMetrics";
            uid = "victoriametrics";
            type = "prometheus";
            access = "proxy";
            url = "http://127.0.0.1:${toString vars.ports.victoriametrics}";
            isDefault = true;
            editable = false;
            jsonData.timeInterval = "30s";
          }
        ];
      };
      dashboards.settings = {
        apiVersion = 1;
        providers = [
          {
            name = "host-health";
            folder = "Host health";
            allowUiUpdates = false;
            disableDeletion = true;
            options.path = ./grafana/dashboards;
          }
        ];
      };
    };
  };
}
