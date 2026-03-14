{ pkgs, ... }:

let
  vars = import ../_variables.nix;
in

{
  systemd.services.vscodium-web = {
    description = "VSCodium web UI";
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" ];
    script = ''
      export PATH=/etc/profiles/per-user/dog/bin:/run/current-system/sw/bin:$PATH
      ${pkgs.vscodium-fhs}/bin/codium serve-web --without-connection-token --port ${toString vars.ports.vscodium}
    '';
    serviceConfig = {
      Type = "simple";
      User = "dog";
      WorkingDirectory = "/home/dog/projects";
      Restart = "on-failure";
    };
  };
}
