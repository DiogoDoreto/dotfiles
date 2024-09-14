{ config, lib, pkgs, ... }:

/*
# Manual setup (for now...)

- Ensure home-server has this static IP: 192.168.1.200
- Setup router to use home-server IP as DNS primary server and `1.1.1.1` as secondary
- Open DNS port 53 in firewall
- On 1st run, you should run `./.config/dnsmasq/fix-bind-permission.sh` so that systemd service won't fail
*/

with lib;

let
  cfg = config.dog.home-server;
  homedir = config.home.homeDirectory;
in {
  options.dog.home-server = {
    enable = mkEnableOption "Home Server setup";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      dnsmasq
      jellyfin
      libcap
    ];

    home.file = {
      ".config/dnsmasq/fix-bind-permission.sh" = {
        executable = true;
        text = ''
          #!/usr/bin/env bash
          sudo ${pkgs.libcap}/bin/setcap CAP_NET_BIND_SERVICE=+eip ${getExe pkgs.dnsmasq}
        '';
      };

      ".config/dnsmasq/hosts".text = ''
        192.168.1.200	dogdot.local
      '';
      ".config/dnsmasq/dnsmasq.conf".text = ''
        addn-hosts=${homedir}/.config/dnsmasq/hosts
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
  };
}
