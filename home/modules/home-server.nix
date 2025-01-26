{ config, lib, pkgs, ... }:

/*
# Manual setup (for now...)

- Ensure home-server has the static IP defined in `hostip`
- Setup router to use home-server IP as DNS primary server and `1.1.1.1` as secondary
- Open ports 53, 80 in firewall (DNS, HTTP)
- On 1st run, you should run `fix-bind-permission.sh` so that systemd service won't fail
*/

with lib;

let
  cfg = config.dog.home-server;
  homedir = config.home.homeDirectory;
  hostname = "dogdot.home";
  hostip = "192.168.0.2";
in {
  options.dog.home-server = {
    enable = mkEnableOption "Home Server setup";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      caddy
      dnsmasq
      jellyfin
    ];

    home.file = {
      "bin/fix-bind-permission.sh" = {
        executable = true;
        text = ''
          #!/usr/bin/env bash
          sudo ${pkgs.libcap}/bin/setcap CAP_NET_BIND_SERVICE=+eip ${getExe pkgs.caddy}
          sudo ${pkgs.libcap}/bin/setcap CAP_NET_BIND_SERVICE=+eip ${getExe pkgs.dnsmasq}
          systemctl --user restart caddy
          systemctl --user restart dnsmasq
        '';
      };

      ".config/caddy/caddy.json" = {
        onChange = "${getExe pkgs.caddy} reload --config=${homedir}/.config/caddy/caddy.json || true";
        text = strings.toJSON {
          apps.http.servers.dogdot = {
            listen = [ ":80" ];
            routes = [
              {
                match = [{
                  host = [ "jellyfin.${hostname}" ];
                }];
                handle = [{
                  handler = "reverse_proxy";
                  upstreams = [{
                    dial = "localhost:8096";
                  }];
                }];
              }
              {
                match = [{
                  host = [ "ha.${hostname}" ];
                }];
                handle = [{
                  handler = "reverse_proxy";
                  upstreams = [{
                    dial = "localhost:8123";
                  }];
                }];
              }
              {
                match = [{
                  host = [ "3000.${hostname}" ];
                }];
                handle = [{
                  handler = "reverse_proxy";
                  upstreams = [{
                    dial = "localhost:3000";
                  }];
                }];
              }
            ];
          };
        };
      };

      ".config/dnsmasq/dnsmasq.conf".text = ''
        address=/${hostname}/${hostip}
        # Do not read system files
        no-hosts
        no-resolv
        # Do not send private addresses to upstream servers
        bogus-priv
        # Do not send addresses without dot to upstream servers
        domain-needed
        # Upstream DNS servers
        server=1.1.1.1
        server=1.0.0.1
      '';
    };

    systemd.user.services = {
      caddy = {
        Unit.Description = "Run Caddy";
        Install.WantedBy = [ "default.target" ];
        Service.ExecStart = "${getExe pkgs.caddy} run --config=${homedir}/.config/caddy/caddy.json";
      };
      dnsmasq = {
        Unit.Description = "Run dnsmasq";
        Install.WantedBy = [ "default.target" ];
        Service.ExecStart = "${getExe pkgs.dnsmasq} --keep-in-foreground --conf-file=${homedir}/.config/dnsmasq/dnsmasq.conf";
      };
      jellyfin = {
        Unit.Description = "Run Jellyfin";
        Install.WantedBy = [ "default.target" ];
        Service.ExecStart = "${getExe pkgs.jellyfin}";
      };
    };

    services.podman = {
      enable = true;
      containers = {
        home-assistant = {
          image = "ghcr.io/home-assistant/home-assistant:stable";
          autoStart = true;
          environment = { TZ = "Europe/Madrid"; };
          network = "host";
          addCapabilities = [ "CAP_NET_RAW" "CAP_NET_BIND_SERVICE" ];
          volumes = [
            "/home/dog/projects/my-home-assistant/config:/config"
            "/run/dbus:/run/dbus:ro"
          ];
          extraPodmanArgs = [ "--privileged" ];
        };
      };
    };
  };
}
