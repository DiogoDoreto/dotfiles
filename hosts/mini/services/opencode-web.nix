{ ... }:

{
  systemd.services.opencode-web = {
    description = "OpenCode web UI";
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" ];
    script = ''
      export PATH=/etc/profiles/per-user/dog/bin:/run/current-system/sw/bin:$PATH
      opencode web --port 32859
    '';
    serviceConfig = {
      Type = "simple";
      User = "dog";
      WorkingDirectory = "/home/dog/projects";
      Restart = "on-failure";
    };
  };
}
