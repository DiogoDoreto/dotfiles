{ config, pkgs, ... }:

let
  vars = import ../_variables.nix;
  nextcloudPkg = pkgs.nextcloud33;
in

{
  services.nextcloud = {
    enable = true;
    package = nextcloudPkg;
    hostName = "localhost";
    phpOptions = {
      # Use custom CA bundle that includes local Caddy certificate
      "openssl.cafile" = "/etc/ssl/certs/ca-bundle-with-local-ca.crt";
      "opcache.interned_strings_buffer" = "32";
    };
    settings = {
      trusted_domains = [
        "nextcloud.local.doreto.com.br"
      ];
      trusted_proxies = [
        "127.0.0.1"
      ];
      # Authentik OIDC setup — manual steps required:
      #
      # 1. In Authentik (https://auth.local.doreto.com.br):
      #    a. Applications → Providers → Create → OAuth2/OpenID Connect Provider
      #       - Redirect URI: https://nextcloud.local.doreto.com.br/apps/oidc_login/oidc
      #       - Note the generated Client ID and Client Secret
      #    b. Applications → Create → link to the provider above
      #       - The slug you choose must match the end of oidc_login_provider_url below
      #
      # 2. Update oidc_login_provider_url slug and oidc_login_client_id below with values from step 1
      #
      # 3. Create the secret file on the host (see secretFile below)
      oidc_login_provider_url = "https://auth.local.doreto.com.br/application/o/nextcloud/";
      oidc_login_client_id = "EITGNTIgfNxq3FsGuPuYtI2XcGzEDIIGZUgpPTaL";
      # Automatically redirect to Authentik instead of showing Nextcloud login page
      oidc_login_auto_redirect = true;
      oidc_login_button_text = "Login with Authentik";
      # Auto-create users from Authentik OIDC claims
      oidc_login_auto_create = true;
      # Allow registration from OIDC (must be false to enable auto-create for oidc_login app)
      oidc_login_disable_registration = false;
      oidc_login_logout_url = "https://auth.local.doreto.com.br/application/o/nextcloud/end-session/";
      # Hide the standard password login form - force OIDC only
      oidc_login_hide_password_form = true;
      # Map Authentik OIDC claims to NextCloud user attributes
      oidc_login_scope = "openid email profile";
      oidc_login_attributes = {
        id = "preferred_username"; # Use readable username instead of UUID hash
        name = "name"; # User's display name from Authentik
        mail = "email"; # User's email from Authentik
      };
      # Auto-promote specified users to admin on first login
      # Note: This may not work reliably with OIDC. If needed, use the occ CLI:
      #   nextcloud-occ group:adduser admin diogo
      oidc_login_admin_users = [ "diogo" ];

      # Maintenance window starts at 6AM UTC
      maintenance_window_start = 6;
    };
    # Manual step: create this file on the host with the Client Secret from Authentik step 1a:
    #   mkdir -p /etc/secrets/nextcloud
    #   echo '{"oidc_login_client_secret": "your-client-secret-from-authentik"}' \
    #     > /etc/secrets/nextcloud/oidc.json
    #   chmod 600 /etc/secrets/nextcloud/oidc.json
    secretFile = "/etc/secrets/nextcloud/oidc.json";
    config = {
      # Admin password file location. Contains the initial admin password in plain text.
      # Created on host with: echo "your-admin-password" > /etc/secrets/nextcloud/admin-pass.txt
      # Permissions: 600 (readable only by nextcloud service)
      adminpassFile = "/etc/secrets/nextcloud/admin-pass.txt";
      dbtype = "sqlite";
    };
    extraApps = {
      inherit (pkgs) orgnotes;
      inherit (nextcloudPkg.packages.apps) oidc_login;
    };
    extraAppsEnable = true;
  };
  services.nginx.virtualHosts."${config.services.nextcloud.hostName}".listen = [
    {
      addr = "127.0.0.1";
      port = vars.ports.nextcloud;
    }
  ];
}
