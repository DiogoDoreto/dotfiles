# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ pkgs, ... }:

{
  imports = [
    ./hardware.nix # Include the results of the hardware scan.
  ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Use latest kernel.
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # Needed for podman. See https://github.com/containers/podman/discussions/23193#discussioncomment-9958326
  boot.tmp.useTmpfs = true;

  networking = {
    hostName = "lapdog";
    networkmanager.enable = true;
    firewall = {
      enable = true;
      allowedTCPPorts = [ ];
      allowedUDPPorts = [ ];
    };
  };

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
  services.displayManager.sddm.enable = true;
  services.desktopManager.plasma6.enable = true;

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  services.flatpak.enable = true;

  # Enable sound with pipewire.
  services.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
    extraConfig.pipewire."92-low-latency" = {
      "context.properties" = {
        "default.clock.rate" = 48000;
        "default.clock.quantum" = 32;
        "default.clock.min-quantum" = 32;
        "default.clock.max-quantum" = 32;
      };
    };
  };

  musnix = {
    enable = true;
    soundcardPciId = "00:1f.3";
  };

  hardware.ipu7 = {
    enable = true;
    platform = "ipu7x";
  };

  hardware.enableAllFirmware = true;

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
    extraGroups = [
      "networkmanager"
      "wheel"
      "audio"
      "video"
      "kvm" # needed to run QEMU/microvm without root
    ];
  };

  home-manager = {
    # install packages in /etc/profiles/per-user/dog
    useUserPackages = true;
    # reuse system pkgs
    useGlobalPkgs = true;
    users.dog = ./home.nix;
  };

  # Niri WM
  programs.niri.enable = true;
  programs.niri.package = pkgs.niri-unstable;
  environment.variables.NIXOS_OZONE_WL = "1";

  # Install firefox.
  programs.firefox.enable = true;

  programs.kdeconnect.enable = true;

  programs.steam.enable = true;

  programs.ydotool.enable = true;

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    home-manager
    openvpn
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  services.fwupd.enable = true;

  services.tailscale.enable = true;

  # Keyboard remaping
  services.evremap = {
    enable = true;
    settings = {
      device_name = "AT Translated Set 2 keyboard";
      dual_role = [
        {
          input = "KEY_CAPSLOCK";
          hold = [ "KEY_RIGHTCTRL" ];
          tap = [ "KEY_ESC" ];
        }
        {
          input = "KEY_LEFTCTRL";
          hold = [ "KEY_LEFTCTRL" ];
          tap = [ "KEY_CAPSLOCK" ];
        }
      ];
      remap = [
        {
          input = [
            "KEY_LEFTCTRL"
            "KEY_H"
          ];
          output = [ "KEY_LEFT" ];
        }
        {
          input = [
            "KEY_LEFTCTRL"
            "KEY_J"
          ];
          output = [ "KEY_DOWN" ];
        }
        {
          input = [
            "KEY_LEFTCTRL"
            "KEY_K"
          ];
          output = [ "KEY_UP" ];
        }
        {
          input = [
            "KEY_LEFTCTRL"
            "KEY_L"
          ];
          output = [ "KEY_RIGHT" ];
        }
      ];
    };
  };

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
      autoStart = false;
    };
    vpn-br = {
      config = "config /home/dog/.vpn/br-sao.prod.surfshark.comsurfshark_openvpn_udp.ovpn";
      autoStart = false;
    };
  };

  virtualisation.podman.enable = true;

  # ── MicroVM: share ~/.claude.json with the agent VM ───────────────────────
  # virtiofsd can only share *directories*, not individual files.  We create
  # a small staging dir and bind-mount ~/.claude.json into it at runtime;
  # the VM then sees it via the "claude-share" virtiofs tag.
  systemd.tmpfiles.settings."lapdog-agent-claude" = {
    "/var/lib/lapdog-agent" = {
      d = {
        mode = "0755";
        user = "root";
        group = "root";
      };
    };
    "/var/lib/lapdog-agent/claude-share" = {
      d = {
        mode = "0700";
        user = "dog";
        group = "users";
      };
    };
    # Placeholder file that the bind mount will overlay at runtime.
    "/var/lib/lapdog-agent/claude-share/.claude.json" = {
      f = {
        mode = "0600";
        user = "dog";
        group = "users";
      };
    };
  };

  systemd.services."lapdog-agent-bind-claude-json" = {
    description = "Bind-mount ~/.claude.json into lapdog-agent virtiofs share dir";
    wantedBy = [ "multi-user.target" ];
    after = [
      "systemd-tmpfiles-setup.service"
      "local-fs.target"
    ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = pkgs.writeShellScript "lapdog-agent-bind-start" ''
        src=/home/dog/.claude.json
        dst=/var/lib/lapdog-agent/claude-share/.claude.json
        # Create the file on first run so claude-code inside the VM can
        # write its auth token back to the host even before the first
        # host-side login.
        if [ ! -f "$src" ]; then
          install -m 600 -o dog -g users /dev/null "$src"
        fi
        if ! ${pkgs.util-linux}/bin/mountpoint -q "$dst"; then
          ${pkgs.util-linux}/bin/mount --bind "$src" "$dst"
        fi
      '';
      ExecStop = pkgs.writeShellScript "lapdog-agent-bind-stop" ''
        dst=/var/lib/lapdog-agent/claude-share/.claude.json
        if ${pkgs.util-linux}/bin/mountpoint -q "$dst"; then
          ${pkgs.util-linux}/bin/umount "$dst"
        fi
      '';
    };
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "25.05"; # Did you read the comment?
}
