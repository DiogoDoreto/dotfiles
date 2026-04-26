# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware.nix
    ./services/llama-swap.nix
    ./services/invokeai.nix
    ./services/unsloth-studio.nix
    ./services/auto-suspend.nix
    ./services/storage.nix
  ];

  nix.settings = {
    substituters = [
      "https://nix-community.cachix.org"
      "https://cuda-maintainers.cachix.org"
    ];
    trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "cuda-maintainers.cachix.org-1:0dq3bujKpuEPMCX6U4WylrUDZ9JyUG0VpVZa7CNfq5E="
    ];
  };

  hardware.nvidia-container-toolkit = {
    enable = true;
  };

  # Basic Graphics Setup
  hardware.graphics = {
    enable = true;
    enable32Bit = true; # Useful for some viewer dependencies
  };

  # enable CUDA per-program that needs it
  # nixpkgs.config.cudaSupport = true;

  # Early KMS: load i915 in initrd so Intel iGPU output is stable before SDDM starts.
  # Without this, there's a race between driver init and the display manager that
  # causes the HDMI signal to randomly drop on cold boot.
  boot.initrd.kernelModules = [ "i915" ];

  # Intel (display) + NVIDIA (compute) via PRIME offload
  # Intel UHD 770 handles display; RTX 4090 is free for CUDA/compute workloads
  services.xserver.videoDrivers = [
    "modesetting"
    "nvidia"
  ];

  hardware.nvidia = {
    modesetting.enable = true;
    powerManagement.enable = true;
    open = true;
    nvidiaSettings = true;
    package = config.boot.kernelPackages.nvidiaPackages.stable;

    prime = {
      offload = {
        enable = true;
        enableOffloadCmd = true; # provides `nvidia-offload` wrapper
      };
      intelBusId = "PCI:0:2:0"; # Intel UHD Graphics 770
      nvidiaBusId = "PCI:1:0:0"; # NVIDIA RTX 4090
    };
  };

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.supportedFilesystems = [ "ntfs" ];

  # Needed for podman. See https://github.com/containers/podman/discussions/23193#discussioncomment-9958326
  boot.tmp.useTmpfs = true;

  networking.hostName = "chungus";
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;
  networking.networkmanager.unmanaged = [ "interface-name:enp4s0" ];

  networking.interfaces.enp4s0.ipv4.addresses = [
    {
      address = "192.168.0.3";
      prefixLength = 24;
    }
  ];
  networking.defaultGateway = {
    address = "192.168.0.1";
    interface = "enp4s0";
  };
  networking.nameservers = [ "192.168.0.2" ];

  security.pki.certificateFiles = [ ../mini/home-caddy.crt ];

  # Set your time zone.
  time.timeZone = "Europe/Madrid";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "es_ES.UTF-8";
    LC_IDENTIFICATION = "es_ES.UTF-8";
    LC_MEASUREMENT = "es_ES.UTF-8";
    LC_MONETARY = "es_ES.UTF-8";
    LC_NAME = "es_ES.UTF-8";
    LC_NUMERIC = "es_ES.UTF-8";
    LC_PAPER = "es_ES.UTF-8";
    LC_TELEPHONE = "es_ES.UTF-8";
    LC_TIME = "es_ES.UTF-8";
  };

  # Enable the X11 windowing system.
  # You can disable this if you're only using the Wayland session.
  services.xserver.enable = true;

  # Enable the KDE Plasma Desktop Environment.
  services.desktopManager.plasma6.enable = true;
  services.displayManager.sddm.enable = true;

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  services.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # OpenTabletDriver is an open source, cross-platform, low latency, user-mode tablet driver.
  # hardware.opentabletdriver.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.dog = {
    isNormalUser = true;
    description = "Diogo Doreto";
    extraGroups = [
      "networkmanager"
      "wheel"
    ];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFzvUuNy14x6avfx0mYrG3txTKgQZbTADajlZ7Sjk1bz dog@lapdog"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDLqT5b5ZCZvpifcoIKZsY2g163FYTCfGScFXVBaW/XcQpNOO/FxOLlaNswlsOeIkuP3vZW7iDfllMq1DfnAvOSa3HSXIp5lu6fPiN+eTvTpVLO5t9/RbZvLOJVZQfS0D4+rX9SkF5+g0us+DjBO1JBr+bVCpZFhEMpvWlg7T2qxhO5aQyMW8G//HuUoCQV6yOcEVyvgg0xtTGRMEDVHn4lwSKn4zWtpvD3lZ0RGuotREbTfwz6eZ5WIeePKrfeNLGGjuP/HUKoE4IEXRbGvnTb/napwFiyuPZaGbFsLdwVjWpWymbfAr1LlB+CIhIG3NsBbgY8RGEl/sVd2ENcDHXZ16Cvml8nfzqdXEFmBBbsM1Sv9KClf0Q7IZyOnxCvQmpxuC6V+oKHxPKOPlIW2o+XT71SzKZhUBqIJw/bjS6oK3AS+VCt7ZKyD/GzZ4hshCH9fVtlhLrfNEsZbDkFSwrv7ZHoJVwh7VXukyA3iDtLY7oU2CTebUhx3Z1GKDbNjWYj23uMPWGwgnN7c9ipftOuMdkyJTRkvYCNGKWaYQzb9svmmZJjbmicY7I2r7/I9xUCa1W/IlLDc0AsbIEju0GgR4AbiL34xKsdmOkvx40h6CvECnohdMhuOK/B0985uOJip5t9+k6uVpHXWsrBQIPEuIb6oqZ50ljl5UAC/ihE+Q== ShellFish@iPhone-07032026"
    ];
  };

  users.users.root = {
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFzvUuNy14x6avfx0mYrG3txTKgQZbTADajlZ7Sjk1bz dog@lapdog"
    ];
  };

  home-manager = {
    # install packages in /etc/profiles/per-user/dog
    useUserPackages = true;
    # reuse system pkgs
    useGlobalPkgs = true;
    users.dog = ./home.nix;
  };

  # Install firefox.
  programs.firefox.enable = true;

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    home-manager
    vim
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    # require public key authentication for better security
    settings = {
      PasswordAuthentication = false;
      KbdInteractiveAuthentication = false;
    };
  };

  virtualisation.podman.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.11"; # Did you read the comment?

}
