{
  pkgs,
  lib,
  config,
  ...
}:

# NOTES: btrfs submodules must be created through the CLI before mounting them

let
  inherit (import ../_variables.nix) ports;
  dataDiskUuid = "cb74725c-0296-4ae5-86cd-743c4ce7c492";
  parityDiskUuid = "facdb380-5373-477f-84f0-86f31f57b57c";
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

  # sudo mkfs.xfs -f -L "snapraid-pa1" /dev/sdX
  fileSystems."/mnt/snapraid-parity1" = {
    device = "/dev/disk/by-uuid/${parityDiskUuid}";
    fsType = "xfs";
    options = [
      "defaults"
      "noatime"
      "nofail"
    ];
  };

  users.groups.data = { };
  users.users.dog.extraGroups = [ "data" ];

  # setgid (2) ensures new files/dirs inherit the data group
  systemd.tmpfiles.rules = [
    "d /data 2775 root data -"
  ];

  environment.systemPackages = with pkgs; [
    btrfs-progs
    xfsprogs

    smartmontools
    snapraid-btrfs
    snapraid-btrfs-runner
  ];

  services.snapraid = {
    enable = true;
    # Working set verified against snapshot 50 after the BIOS/memory fix.
    # Keep the independently verified snapshot-49-f18 set untouched as fallback.
    parityFiles = [
      "/mnt/snapraid-parity1/snapraid-baseline-50-f18.parity"
    ];
    contentFiles = [
      "/var/.snapraid-baseline-50-f18-content"
      "/mnt/snapraid-parity1/.snapraid-baseline-50-f18-content"
      "/mnt/data-snapraid-content/.snapraid-baseline-50-f18-content"
    ];
    dataDisks = {
      d1 = "/data";
    };
    exclude = [
      "*.unrecoverable"
      "/tmp/"
      "/lost+found/"
      "*.!sync"
      "/.snapshots/"
    ];
    extraConfig = ''
      autosave 50
    '';
  };

  # The upstream SnapRAID module schedules its own sync and scrub services.
  # Keep them available for manual use, but do not generate their timers:
  # the btrfs-aware runner below handles the scheduled sync instead.
  systemd.services.snapraid-sync.startAt = lib.mkForce [ ];
  systemd.services.snapraid-scrub.startAt = lib.mkForce [ ];

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
      # Keep the service available for manual use, but do not schedule it until
      # a new baseline is verified and interrupted-sync cleanup is safe.
      # "03:00"
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

  networking.firewall.allowedTCPPorts = [ ports.nfs ];
  networking.firewall.allowedUDPPorts = [ ports.nfs ];
}
