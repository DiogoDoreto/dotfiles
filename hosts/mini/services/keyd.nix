{
  lib,
  pkgs,
  ...
}:

let
  # Add a repeating variant of command(), analogous to macro2().
  keyd = pkgs.keyd.overrideAttrs (oldAttrs: {
    patches = (oldAttrs.patches or [ ]) ++ [ ./keyd-command2.patch ];
  });
in

{
  services.keyd = {
    enable = true;
    package = keyd;
    keyboards.default = {
      ids = [ "*" ];
      settings = {
        main = {
          # Run immediately, then repeat every 200 ms after a 400 ms delay.
          volumeup = "command2(400, 200, denon up)";
          volumedown = "command2(400, 200, denon down)";
          mute = "command(denon mute)";
          capslock = "overload(control, capslock)";
          esc = "overload(alt, esc)";
        };
      };
    };
  };

  systemd.services.keyd = {
    # denon is a system package and is not in the service's default PATH,
    # so add it explicitly.
    path = [ pkgs.my.denon ];

    # The keyd module sandboxes the service with no network access
    # (PrivateNetwork, IPAddressDeny, RestrictAddressFamilies), which would
    # block denon's curl to ha.local.doreto.com.br. Allow network access.
    serviceConfig = {
      PrivateNetwork = lib.mkForce false;
      RestrictAddressFamilies = lib.mkForce [
        "AF_UNIX"
        "AF_INET"
        "AF_INET6"
      ];
      IPAddressDeny = lib.mkForce [ ];
    };
  };
}
