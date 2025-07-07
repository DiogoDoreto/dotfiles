# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [
      ./hardware.nix # Include the results of the hardware scan.
      ./media.nix
    ];

  # TODO update kernel to use this:
  # hardware.intelgpu.driver = "xe";

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # https://discourse.nixos.org/t/realtek-audio-sound-card-not-recognized-by-pipewire/36637/2
  boot.kernelParams = [ "snd_hda_intel.dmic_detect=0" ];

  # Needed for podman. See https://github.com/containers/podman/discussions/23193#discussioncomment-9958326
  boot.tmp.useTmpfs = true;

  networking = {
    hostName = "dogdot";
    networkmanager.enable = true;
    firewall = {
      enable = true;
      allowedTCPPorts = [ 53 80 443 21064 21065 ];
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
  services.pulseaudio.enable = false;
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

  hardware.enableAllFirmware = true;

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
    # install packages in /etc/profiles/per-user/dog
    useUserPackages = true;
    # reuse system pkgs
    useGlobalPkgs = true;
    users.dog = import ./home.nix;
  };

  # Enable automatic login for the user.
  services.displayManager.autoLogin.enable = true;
  services.displayManager.autoLogin.user = "dog";

  # Install firefox.
  programs.firefox.enable = true;

  programs.kdeconnect.enable = true;

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    openvpn
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

  services.nextcloud = {
    enable = true;
    package = pkgs.nextcloud31;
    hostName = "localhost";
    settings = {
      trusted_domains = [ "nextcloud.dogdot.home" "nextcloud.local.doreto.com.br" ];
    };
    config = {
      adminpassFile = "/var/lib/nextcloud-pass.txt";
      dbtype = "sqlite";
    };
  };
  services.nginx.virtualHosts."${config.services.nextcloud.hostName}".listen = [
    { addr = "127.0.0.1"; port = 5387; }
  ];

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    # require public key authentication for better security
    settings = {
      PasswordAuthentication = false;
      KbdInteractiveAuthentication = false;
    };
  };

  services.openvpn.servers = {
    vpn-es = {
      config = "config /home/dog/.vpn/es-mad.prod.surfshark.comsurfshark_openvpn_udp.ovpn";
      autoStart = true;
    };
    vpn-br = {
      config = "config /home/dog/.vpn/br-sao.prod.surfshark.comsurfshark_openvpn_udp.ovpn";
      autoStart = false;
    };
  };

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
        "/.local.doreto.com.br/${static-ip}"
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
      rproxies = [
        { name = "nextcloud"; port = "5387"; }
        { name = "lidarr";    port = "8686"; }
        { name = "radarr";    port = "7878"; }
        { name = "sonarr";    port = "8989"; }
        { name = "prowlarr";  port = "9696"; }
        { name = "qbit";      port = "8079"; }
        { name = "jellyfin";  port = "8096"; }
        { name = "ha";        host = "192.168.0.2"; port = "8123"; }
        { name = "home";      port = "8082"; }
        { name = "vite";      port = "5173"; }
      ];
      make-rproxy = {name, host ? "localhost", port}: {
        match = [{
          host = [ "${name}.${hostname}" "${name}.local.doreto.com.br" ];
        }];
        handle = [{
          handler = "reverse_proxy";
          upstreams = [{
            dial = "${host}:${port}";
          }];
        }];
      };
    in {
      apps.http.servers.dogdot = {
        listen = [ ":80" ":443" ];
        routes = (map make-rproxy rproxies);
      };
      apps.tls.certificates.load_files = [{
        certificate = "/var/lib/caddy/certs/fullchain1.pem";
        key = "/var/lib/caddy/certs/privkey1.pem";
      }];
    };
  };

  services.homepage-dashboard = {
    enable = true;
    listenPort = 8082;
    allowedHosts = "home.dogdot.home";
    widgets = [
      {
        resources = {
          cpu = true;
          memory = true;
          disk = "/";
        };
      }
      {
        search = {
          provider = "duckduckgo";
          target = "_blank";
        };
      }
    ];
    services = [
      # search for icons in https://dashboardicons.com
      {
        Apps = [
          {
            "HomeAssistant" = rec {
              icon = "home-assistant.png";
              href = "http://ha.dogdot.home";
              ping = href;
            };
          }
          {
            "NextCloud" = rec {
              icon = "nextcloud.png";
              href = "http://nextcloud.dogdot.home";
              ping = href;
            };
          }
          {
            "Open WebUI" = rec {
              icon = "open-webui.png";
              href = "http://chungus.home:8080";
              ping = href;
            };
          }
        ];
      }
      {
        Media = [
          {
            "jellyfin" = rec {
              icon = "jellyfin.png";
              href = "http://jellyfin.dogdot.home";
              ping = href;
            };
          }
          {
            "qBittorrent" = rec {
              icon = "qbittorrent.png";
              href = "http://qbit.dogdot.home";
              ping = href;
              widget = {
                type = "qbittorrent";
                url = href;
              };
            };
          }
          {
            "Radarr" = rec {
              icon = "radarr.png";
              description = "Movies downloader";
              href = "http://radarr.dogdot.home";
              ping = href;
              widget = {
                type = "radarr";
                url = href;
                key = "29b5783c14964b3abae3886e4fe3b93b";
              };
            };
          }
          {
            "Sonarr" = rec {
              icon = "sonarr.png";
              description = "TV Shows downloader";
              href = "http://sonarr.dogdot.home";
              ping = href;
              widget = {
                type = "sonarr";
                url = href;
                key = "023e2f0779304b6e9bbb6cd077cf8dc8";
              };
            };
          }
          {
            "Lidarr" = rec {
              icon = "lidarr.png";
              description = "Music downloader";
              href = "http://lidarr.dogdot.home";
              ping = href;
              widget = {
                type = "lidarr";
                url = href;
                key = "d7bc3d65c5334400b6f3338de945915a";
              };
            };
          }
          {
            "Prowlarr " = rec {
              icon = "prowlarr.png";
              description = "Search torrents";
              href = "http://prowlarr.dogdot.home";
              ping = href;
            };
          }
        ];
      }
    ];
  };
  systemd.services.homepage-dashboard.path = [ pkgs.unixtools.ping ];

  virtualisation.podman.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.11"; # Did you read the comment?

}
