# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware.nix
    ];

  # TODO update kernel to use this:
  # hardware.intelgpu.driver = "xe";

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Needed for podman. See https://github.com/containers/podman/discussions/23193#discussioncomment-9958326
  boot.tmp.useTmpfs = true;

  networking = {
    hostName = "dogdot";
    networkmanager.enable = true;
    firewall = {
      enable = true;
      allowedTCPPorts = [ 53 80 21064 21065 ];
      allowedUDPPorts = [ 53 5353 ];
    };
  };

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
  services.displayManager.sddm.enable = true;
  services.desktopManager.plasma6.enable = true;

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  # Enable sound with pipewire.
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # OpenTabletDriver is an open source, cross-platform, low latency, user-mode tablet driver.
  hardware.opentabletdriver.enable = true;

  # Bluetooth
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
  };
  services.blueman.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.dog = {
    isNormalUser = true;
    description = "Diogo Doreto";
    extraGroups = [ "networkmanager" "wheel" ];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDDhA9Eum8X+nly3QFAhrazy+5JLdVx+r8natZ6tCex0 diogo@doreto.com.br"
    ];
  };

  home-manager = {
    # TODO check if this is the optimal way to have nixos and hm sharing the same nixpkgs reference
    useUserPackages = true;
    useGlobalPkgs = true;
    users.dog = import ./home.nix;
  };

  # Enable automatic login for the user.
  services.displayManager.autoLogin.enable = true;
  services.displayManager.autoLogin.user = "dog";

  # Install firefox.
  programs.firefox.enable = true;

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
  #  vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
  #  wget
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

  services.jellyfin.enable = true;

  services.dnsmasq = {
    enable = true;
    alwaysKeepRunning = true;
    settings = let
      static-ip = "192.168.0.2"; # needs to be set manually in networkmanager
    in {
      # interface = "wlo1";
      # bind-interfaces = true;
      listen-address="::1,127.0.0.1,${static-ip}";
      address = [
        "/${config.networking.hostName}.home/${static-ip}"
        "/chungus.home/192.168.0.3"
      ];

      # Accept DNS queries only from hosts whose address is on a local subnet
      # local-service = true;
      # Do not read system files
      no-hosts = true;
      no-resolv = true;
      # Do not send private addresses to upstream servers
      bogus-priv = true;
      # Do not send addresses without dot to upstream servers
      domain-needed = true;
      # Upstream DNS servers
      server = [ "1.1.1.1" "1.0.0.1" ];
      # Log DNS queries
      log-queries = true;

      # Enable DNSSEC
      # dnssec = true;
      # DNSSEC trust anchor. Source: https://data.iana.org/root-anchors/root-anchors.xml
      # trust-anchor = ".,20326,8,2,E06D44B80B8F1D39A95C0B0D7C65D08458E880409BBC683457104237C7F8EC8D";
    };
  };

  services.caddy = {
    enable = true;
    settings = let
      hostname = "${config.networking.hostName}.home";
    in {
      apps.http.servers.dogdot = {
        listen = [ ":80" ];
        routes = [
          {
            match = [{
              host = [ "jellyfin.${hostname}" ];
            }];
            handle = [{
              handler = "reverse_proxy";
              upstreams = [{
                dial = "localhost:8096";
              }];
            }];
          }
          {
            match = [{
              host = [ "ha.${hostname}" ];
            }];
            handle = [{
              handler = "reverse_proxy";
              upstreams = [{
                dial = "192.168.0.2:8123";
              }];
            }];
          }
          {
            match = [{
              host = [ "vite.${hostname}" ];
            }];
            handle = [{
              handler = "reverse_proxy";
              upstreams = [{
                dial = "192.168.0.2:5173";
              }];
            }];
          }
        ];
      };
    };
  };

  virtualisation.podman.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.11"; # Did you read the comment?

}
