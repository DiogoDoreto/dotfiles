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

  dog.services.journald-to-victorialogs = {
    enable = true;
    host = "127.0.0.1";
    port = vars.ports.victorialogs;
    tls = false;
  };
}
