{
  description = "Manual lapdog host-command reviewer and Agent VM stdio exchange";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  outputs =
    { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      mkHost =
        {
          expectedUID ? 1000,
          guestAddress ? "10.0.101.2",
          guestUser ? "agent",
          identity ? "/var/lib/agent-vm/ssh/host/host_to_vm",
        }:
        pkgs.buildGoModule {
          pname = "host-command-review-host";
          version = "0.1.0-unreleased";
          src = self;
          vendorHash = "sha256-NbuvbsmNbz3aKdjyyfpkbi52yRhXl473j3LvAUDn3L8=";
          subPackages = [
            "cmd/agent-host-review"
            "cmd/host-command-root-runner"
            "cmd/host-command-unit-helper"
          ];
          ldflags = [
            "-s"
            "-w"
          ];
          preBuild = ''
            ldflags+=(
              "-X=main.shell=${pkgs.bash}/bin/bash"
              "-X=main.systemdRun=${pkgs.systemd}/bin/systemd-run"
              "-X=main.systemctl=${pkgs.systemd}/bin/systemctl"
              "-X=main.helper=$out/bin/host-command-unit-helper"
              "-X=main.runner=$out/bin/host-command-root-runner"
              "-X=main.rootRuntime=/run/host-command-review-root"
              "-X=main.userRuntime=/run/user/${toString expectedUID}/host-command-review"
              "-X=main.expectedUID=${toString expectedUID}"
              "-X=main.ssh=${pkgs.openssh}/bin/ssh"
              "-X=main.loginctl=${pkgs.systemd}/bin/loginctl"
              "-X=main.dbusMonitor=${pkgs.dbus}/bin/dbus-monitor"
              "-X=main.opener=${pkgs.xdg-utils}/bin/xdg-open"
              "-X=main.guestAddress=${guestAddress}"
              "-X=main.guestUser=${guestUser}"
              "-X=main.identity=${identity}"
            )
          '';
          postInstall = ''
            mkdir -p $out/share/polkit-1/actions
            cat > $out/share/polkit-1/actions/br.com.doreto.host-command-review.policy <<EOF
            <?xml version="1.0" encoding="UTF-8"?>
            <!DOCTYPE policyconfig PUBLIC "-//freedesktop//DTD PolicyKit Policy Configuration 1.0//EN" "http://www.freedesktop.org/standards/PolicyKit/1/policyconfig.dtd">
            <policyconfig><action id="br.com.doreto.host-command-review.execute">
            <description>Run one reviewed host command as root</description>
            <message>Authenticate to run one reviewed host command as root</message>
            <defaults><allow_any>no</allow_any><allow_inactive>no</allow_inactive><allow_active>auth_admin</allow_active></defaults>
            <annotate key="org.freedesktop.policykit.exec.path">$out/bin/host-command-root-runner</annotate>
            <annotate key="org.freedesktop.policykit.exec.argv1">--approved-sha256</annotate>
            </action></policyconfig>
            EOF
          '';
          meta.platforms = [ system ];
        };
      guest = pkgs.buildGoModule {
        pname = "host-command-review-guest";
        version = "0.1.0-unreleased";
        src = self;
        vendorHash = "sha256-NbuvbsmNbz3aKdjyyfpkbi52yRhXl473j3LvAUDn3L8=";
        subPackages = [
          "cmd/host-review-mcp"
          "cmd/host-review-exchange"
        ];
        ldflags = [
          "-s"
          "-w"
        ];
        meta.platforms = [ system ];
      };
    in
    {
      lib.mkHost = mkHost;
      packages.${system} = {
        host = mkHost { };
        inherit guest;
        default = self.packages.${system}.host;
      };
    };
}
