{ pkgs, ... }:

let
  dataDiskUuid = "cb74725c-0296-4ae5-86cd-743c4ce7c492";
in
{
  fileSystems."/data" = {
    device = "/dev/disk/by-uuid/${dataDiskUuid}";
    fsType = "btrfs";
    options = [
      "noatime"
      "compress=zstd"
    ];
  };

  # fileSystems."/data/.snapraid-content" = {
  #   device = "/dev/disk/by-uuid/${dataDiskUuid}";
  #   fsType = "btrfs";
  #   options = [
  #     "noatime"
  #     "subvol=@content"
  #   ];
  # };

  users.groups.data = { };
  users.users.dog.extraGroups = [ "data" ];

  # setgid (2) ensures new files/dirs inherit the data group
  systemd.tmpfiles.rules = [
    "d /data 2775 root data -"
    # "d /data/.snapraid-content 2775 root data -"
  ];

  environment.systemPackages = [ pkgs.btrfs-progs ];

  services.nfs.server = {
    enable = true;
    exports = ''
      /data 192.168.0.0/24(rw,sync,no_subtree_check,no_root_squash)
    '';
  };

  networking.firewall.allowedTCPPorts = [ 2049 ];
  networking.firewall.allowedUDPPorts = [ 2049 ];
}
