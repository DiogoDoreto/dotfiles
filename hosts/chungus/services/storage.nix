{
  pkgs,
  lib,
  config,
  ...
}:

# NOTES: btrfs submodules must be created through the CLI before mounting them

let
  dataDiskUuid = "cb74725c-0296-4ae5-86cd-743c4ce7c492";
  parityDiskUuid = "0106c07d-4ed7-4a7c-9698-098cdcd35880";
in
{
  nixpkgs.overlays = [
    (final: prev: {
      snapraid-btrfs = prev.callPackage ../pkgs/snapraid-btrfs.nix { };
      snapraid-btrfs-runner = prev.callPackage ../pkgs/snapraid-btrfs-runner.nix { };
    })
  ];

  fileSystems."/data" = {
    device = "/dev/disk/by-uuid/${dataDiskUuid}";
    fsType = "btrfs";
    options = [
      "noatime"
      "compress=zstd"
    ];
  };

  fileSystems."/data/.snapshots" = {
    device = "/dev/disk/by-uuid/${dataDiskUuid}";
    fsType = "btrfs";
    options = [ "subvol=.snapshots" ];
  };

  fileSystems."/mnt/data-snapraid-content" = {
    device = "/dev/disk/by-uuid/${dataDiskUuid}";
    fsType = "btrfs";
    options = [
      "noatime"
      "subvol=@content"
    ];
  };

  fileSystems."/mnt/snapraid-parity1" = {
    device = "/dev/disk/by-uuid/${parityDiskUuid}";
    fsType = "ext4";
  };

  users.groups.data = { };
  users.users.dog.extraGroups = [ "data" ];

  # setgid (2) ensures new files/dirs inherit the data group
  systemd.tmpfiles.rules = [
    "d /data 2775 root data -"
  ];

  environment.systemPackages = with pkgs; [
    btrfs-progs
    smartmontools
    snapraid-btrfs
    snapraid-btrfs-runner
  ];

  services.snapraid = {
    enable = true;
    parityFiles = [
      "/mnt/snapraid-parity1/snapraid.parity"
    ];
    contentFiles = [
      "/var/.snapraid-content"
      "/mnt/snapraid-parity1/.snapraid-content"
      "/mnt/data-snapraid-content/.snapraid-content"
    ];
    dataDisks = {
      d1 = "/data";
    };
    sync.interval = "";
    scrub.interval = "";
    exclude = [
      "*.unrecoverable"
      "/tmp/"
      "/lost+found/"
      "*.!sync"
      "/.snapshots/"
    ];
  };

  services.snapper = {
    configs = {
      data = {
        SUBVOLUME = "/data";
        ALLOW_GROUPS = [ "data" ];
        SYNC_ACL = true;
      };
    };
  };

  # Based on https://discourse.nixos.org/t/setting-up-snapraid-btrfs-on-nixos/46476/8
  systemd.services.snapraid-btrfs-sync = {
    description = "Run the snapraid-btrfs sync with the runner";
    startAt = [
      "03:00"
    ];
    serviceConfig = {
      Type = "oneshot";
      User = "root";
      Group = "root";
      ExecStart = "+${pkgs.snapraid-btrfs-runner}/bin/snapraid-btrfs-runner";
      Nice = 19;
      IOSchedulingPriority = 7;
      CPUSchedulingPolicy = "batch";

      LockPersonality = true;
      MemoryDenyWriteExecute = true;
      NoNewPrivileges = true;
      PrivateTmp = true;
      ProtectClock = true;
      ProtectControlGroups = true;
      ProtectHostname = true;
      ProtectKernelLogs = true;
      ProtectKernelModules = true;
      ProtectKernelTunables = true;
      RestrictAddressFamilies = "AF_UNIX";
      RestrictNamespaces = true;
      RestrictRealtime = true;
      RestrictSUIDSGID = true;
      SystemCallArchitectures = "native";
      SystemCallFilter = "@system-service";
      SystemCallErrorNumber = "EPERM";
      CapabilityBoundingSet = "";
      ProtectSystem = "strict";
      ProtectHome = "read-only";
      ReadOnlyPaths = [
        "/etc/snapraid.conf"
        "/etc/snapper"
      ];
      ReadWritePaths =
        # sync requires access to directories containing content files
        # to remove them if they are stale
        let
          inherit (config.services.snapraid) contentFiles parityFiles dataDisks;
          contentDirs = map dirOf contentFiles;
          # Multiple "split" parity files can be specified in a single
          # "parityFile", separated by a comma.
          # https://www.snapraid.it/manual#7.1
          splitParityFiles = map (s: lib.splitString "," s) parityFiles;
        in
        lib.unique (lib.attrValues dataDisks ++ splitParityFiles ++ contentDirs);
    };
  };

  services.nfs.server = {
    enable = true;
    exports = ''
      /data 192.168.0.0/24(rw,sync,no_subtree_check,no_root_squash)
    '';
  };

  networking.firewall.allowedTCPPorts = [ 2049 ];
  networking.firewall.allowedUDPPorts = [ 2049 ];
}
