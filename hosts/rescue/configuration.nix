{
  lib,
  modulesPath,
  pkgs,
  ...
}:
{
  imports = [ (modulesPath + "/installer/cd-dvd/installation-cd-graphical-base.nix") ];

  networking.hostName = "rescue";
  networking.networkmanager.enable = true;

  # This is a recovery desktop, not the Calamares installation image.
  isoImage.edition = "rescue-plasma";
  # Favor reasonable build time over maximum ISO compression.
  isoImage.squashfsCompression = "zstd -Xcompression-level 6";
  system.installer.channel.enable = false;
  programs.kde-pim.enable = false;
  services.desktopManager.plasma6 = {
    enable = true;
    enableQt5Integration = false;
  };
  services.displayManager = {
    plasma-login-manager.enable = true;
    autoLogin = {
      enable = true;
      user = "dog";
    };
  };
  services.getty.autologinUser = lib.mkForce "dog";
  services.getty.helpLine = lib.mkForce ''
    Rescue image: run sudo for administrator access; see /etc/rescue-guide.
    Use the boot menu's "Disable display-manager" option if KDE cannot start.
  '';

  users.users.dog = {
    isNormalUser = true;
    extraGroups = [
      "wheel"
      "networkmanager"
      "video"
    ];
    initialHashedPassword = "";
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFzvUuNy14x6avfx0mYrG3txTKgQZbTADajlZ7Sjk1bz dog@lapdog"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDLqT5b5ZCZvpifcoIKZsY2g163FYTCfGScFXVBaW/XcQpNOO/FxOLlaNswlsOeIkuP3vZW7iDfllMq1DfnAvOSa3HSXIp5lu6fPiN+eTvTpVLO5t9/RbZvLOJVZQfS0D4+rX9SkF5+g0us+DjBO1JBr+bVCpZFhEMpvWlg7T2qxhO5aQyMW8G//HuUoCQV6yOcEVyvgg0xtTGRMEDVHn4lwSKn4zWtpvD3lZ0RGuotREbTfwz6eZ5WIeePKrfeNLGGjuP/HUKoE4IEXRbGvnTb/napwFiyuPZaGbFsLdwVjWpWymbfAr1LlB+CIhIG3NsBbgY8RGEl/sVd2ENcDHXZ16Cvml8nfzqdXEFmBBbsM1Sv9KClf0Q7IZyOnxCvQmpxuC6V+oKHxPKOPlIW2o+XT71SzKZhUBqIJw/bjS6oK3AS+VCt7ZKyD/GzZ4hshCH9fVtlhLrfNEsZbDkFSwrv7ZHoJVwh7VXukyA3iDtLY7oU2CTebUhx3Z1GKDbNjWYj23uMPWGwgnN7c9ipftOuMdkyJTRkvYCNGKWaYQzb9svmmZJjbmicY7I2r7/I9xUCa1W/IlLDc0AsbIEju0GgR4AbiL34xKsdmOkvx40h6CvECnohdMhuOK/B0985uOJip5t9+k6uVpHXWsrBQIPEuIb6oqZ50ljl5UAC/ihE+Q== ShellFish@iPhone-07032026"
    ];
  };
  security.sudo.wheelNeedsPassword = lib.mkForce false;
  security.polkit.enablePkexecWrapper = true;
  programs.partition-manager.enable = true;

  services.openssh = {
    enable = true;
    openFirewall = true;
    settings = {
      PasswordAuthentication = false;
      KbdInteractiveAuthentication = false;
      PermitRootLogin = "no";
    };
  };

  # Dolphin mounts via UDisks. Default to inspection-only; USB drives are
  # writable destinations. Explicit repair operations can still request rw.
  environment.etc."udisks2/mount_options.conf".text = ''
    [defaults]
    defaults=ro
  '';
  services.udev.extraRules = ''
    SUBSYSTEM=="block", SUBSYSTEMS=="usb", ENV{UDISKS_MOUNT_OPTIONS_DEFAULTS}="rw"
  '';

  environment.systemPackages = with pkgs; [
    smartmontools
    gsmartcontrol
    nvme-cli
    ddrescue
    testdisk
    cryptsetup
    lvm2
    mdadm
    btrfs-progs
    xfsprogs
    e2fsprogs
    dosfstools
    exfatprogs
    ntfs3g
    pciutils
    usbutils
    dmidecode
    ethtool
    tcpdump
    bind
    rsync
  ];

  environment.etc."rescue-guide".text = ''
    Rescue image (temporary session)

    KDE starts as dog. If graphics fail, choose Options > Disable display-manager
    at the ISO boot menu; the console logs in as dog. The same tools are available.

    Network: use KDE's Wi-Fi menu or nmtui; wired DHCP is automatic.
    Find the address with: ip -br addr
    Find this boot's SSH fingerprint with: ssh-keygen -lf /etc/ssh/ssh_host_ed25519_key.pub
    SSH as dog using your lapdog or iPhone key. SSH host keys change on reboot.

    Dolphin mounts internal disks read-only; USB drives are writable so you can
    save recovered data. A drive attached via USB is considered writable even
    if it is a recovery source: mount such sources read-only explicitly.
    Check actual mount flags with: findmnt -o SOURCE,TARGET,OPTIONS

    Before repairing a failing disk, consider imaging it with ddrescue.
    For deliberate writes to a mounted filesystem, unmount it first and then
    use a disk tool or an explicit mount -o rw command. Avoid running repairs
    on a mounted filesystem. Nothing in the live session persists after reboot.
  '';
}
