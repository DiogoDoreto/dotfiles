{ pkgs, ... }:

let
  vars = import ../_variables.nix;
in

{
  services.searx = {
    enable = true;
    settings = {
      server.bind_address = "127.0.0.1";
      server.port = vars.ports.searx;
      server.secret_key = "searx";
      search.formats = [
        "html"
        "json"
      ];
      ui.query_in_title = true;
      ui.infinite_scroll = true;
    };
  };
  systemd.services.searx-restarter = {
    description = "Restarts searx every night";
    serviceConfig = {
      Type = "oneshot";
      User = "root";
    };
    script = "${pkgs.systemd}/bin/systemctl try-restart searx.service";
    startAt = "*-*-* 04:00:00";
  };
}
