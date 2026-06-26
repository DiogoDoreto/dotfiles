{ ... }:

let
  vars = import ../_variables.nix;
  p = builtins.mapAttrs (_: toString) vars.ports;
in

{
  services.victorialogs = {
    enable = true;
    listenAddress = "127.0.0.1:${p.victorialogs}";
    extraOptions = [
      "-retentionPeriod=3" # months
    ];
  };

  services.journald.upload = {
    enable = true;
    settings.Upload = {
      URL = "http://127.0.0.1:${p.victorialogs}/insert/journald";
    };
  };
}
