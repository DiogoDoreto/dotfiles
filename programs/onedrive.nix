{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.dog.programs.onedrive;
  homedir = config.home.homeDirectory;
in {
  options.dog.programs.onedrive = {
    enable = mkEnableOption "OneDrive";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      curl
      onedrive
    ];

    home.file = {
      ".config/onedrive/sync_list".text = ''
        # https://github.com/abraunegg/onedrive/blob/master/docs/USAGE.md#performing-a-selective-sync-via-sync_list-file
        /Documentos/
        /Share/
      '';
    };

    systemd.user.services = {
      onedrive = {
        Unit = {
          Description = "OneDrive";
          After = "network-online.target";
          Wants = "network-online.target";
        };
        Service = {
          # Environment = "PATH=${homedir}/.nix-profile/bin";
          # ExecStart = "${lib.getExe pkgs.onedrive} --monitor --confdir=${homedir}/.config/onedrive --verbose --verbose";
          ExecStart = pkgs.writeShellScript "run-onedrive" ''
            HOME=${homedir}
            PATH=$PATH:${homedir}/.nix-profile/bin
            LD_LIBRARY_PATH=${lib.makeLibraryPath [pkgs.curl pkgs.openssl]}
            ${lib.getExe pkgs.onedrive} --debug-https --monitor --confdir=${homedir}/.config/onedrive --verbose --verbose
          '';
          # Restart = "on-failure";
          # RestartSec = 3;
          # RestartPreventExitStatus = 3;

          # Commented out hardenings are disabled because they may not work out of the box on your distribution
          # If you know what you are doing please try to enable them.
          # ProtectSystem = "full";
          #PrivateUsers = true;
          #PrivateDevices = true;
          # ProtectHostname = true;
          #ProtectClock = true;
          # ProtectKernelTunables = true;
          #ProtectKernelModules = true;
          #ProtectKernelLogs = true;
          # ProtectControlGroups = true;
          # RestrictRealtime = true;
        };
        Install.WantedBy = [ "default.target" ];
      };
    };
  };
}
