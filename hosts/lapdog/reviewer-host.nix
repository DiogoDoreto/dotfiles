{
  config,
  inputs,
  ...
}:
let
  cfg = config.dog.services.agent-vm;
  package = inputs.host-command-review.lib.mkHost {
    # lapdog's existing dog account: verify `id -u dog` == 1000 before activation.
    # Do not change account UID/ownership as a side effect of installing reviewer.
    expectedUID = 1000;
    inherit (cfg) guestAddress guestUser;
    identity = "${cfg.stateDir}/ssh/host/host_to_vm";
  };
  runner = "${package}/bin/host-command-root-runner";
in
{
  assertions = [
    {
      assertion = config.users.users.dog.uid == null || config.users.users.dog.uid == 1000;
      message = "Host reviewer requires dog UID 1000; runtime also verifies an auto-assigned UID.";
    }
  ];
  environment.systemPackages = [ package ];
  security.polkit = {
    enable = true;
    enablePkexecWrapper = true;
  };
  # Deliberately precedes NixOS's 10-nixos.rules; no cached grants.
  environment.etc."polkit-1/rules.d/00-host-command-review.rules".text = ''
    polkit.addRule(function(action, subject) {
      if (action.id !== "br.com.doreto.host-command-review.execute") return;
      if (subject.user !== "dog" || !subject.local || !subject.active ||
          action.lookup("user") !== "root" || action.lookup("program") !== "${runner}")
        return polkit.Result.NO;
      return polkit.Result.AUTH_ADMIN;
    });
    polkit.addAdminRule(function(action, subject) {
      if (action.id === "br.com.doreto.host-command-review.execute")
        return ["unix-user:dog"];
    });
  '';
  systemd.tmpfiles.rules = [ "d /run/host-command-review-root 0700 root root -" ];
  # Cleanup-only maintenance, NOT a reviewer session: no SSH, browser, route,
  # listener, or execution. It skips a live reviewer's singleton lock. During a
  # session the foreground reviewer also sweeps each minute.
  systemd.services.host-command-review-cleanup = {
    description = "Remove private leftovers from ended host-command reviews";
    serviceConfig = {
      Type = "oneshot";
      User = "dog";
      ExecStart = "${package}/bin/agent-host-review --cleanup";
      UMask = "0077";
      NoNewPrivileges = true;
      TimeoutStartSec = "2min";
    };
  };
  systemd.timers.host-command-review-cleanup = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnCalendar = "*:0/15";
      Persistent = true;
    };
  };
  # No reviewer autostart, listener, Caddy route or activation launcher.
}
