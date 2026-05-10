{ ... }:

let
  vars = import ../_variables.nix;
in
{
  services.immich = {
    enable = true;
    port = vars.ports.immich;
    host = "0.0.0.0";
    mediaLocation = "/data/immich";
    openFirewall = true;
    # null = allow all devices (needed for NVIDIA CUDA in ML and hardware transcoding)
    accelerationDevices = null;
  };

  # Enable hardware accelerated video transcoding
  users.users.immich.extraGroups = [
    "video"
    "render"
  ];

  # /data is a btrfs mount; create the immich subdir owned by the immich user.
  # The module's tmpfiles entry only sets attributes on existing dirs.
  systemd.tmpfiles.rules = [
    "d /data/immich 0700 immich immich -"
  ];

  services.redis.servers.immich.logLevel = "warning";
}
