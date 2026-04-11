# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ pkgs, self, ... }:

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
    networkmanager = {
      enable = true;
      # Keep NetworkManager away from the microvm bridge and its TAP devices.
      unmanaged = [
        "interface-name:vm0"
        "interface-name:vm-*"
      ];
    };
    firewall = {
      enable = true;
      allowedTCPPorts = [ ];
      allowedUDPPorts = [ ];
      # Allow forwarding between the vm0 bridge and the outside world.
      # The nftables table below handles logging; these rules open the door.
      extraCommands = ''
        iptables -A FORWARD -i vm0 -j ACCEPT
        iptables -A FORWARD -o vm0 -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT
        iptables -t nat -A POSTROUTING -s 10.0.100.0/24 ! -o vm0 -j MASQUERADE
      '';
      extraStopCommands = ''
        iptables -D FORWARD -i vm0 -j ACCEPT 2>/dev/null || true
        iptables -D FORWARD -o vm0 -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT 2>/dev/null || true
        iptables -t nat -D POSTROUTING -s 10.0.100.0/24 ! -o vm0 -j MASQUERADE 2>/dev/null || true
      '';
    };

    # Bridge for the coding-agent MicroVM.
    # VM gets static 10.0.100.2/24; this host is the gateway at 10.0.100.1.
    # Services bound only to 127.0.0.1 are not reachable from the VM —
    # the loopback address is not routable via this bridge.
    bridges.vm0.interfaces = [ ];
    interfaces.vm0.ipv4.addresses = [
      {
        address = "10.0.100.1";
        prefixLength = 24;
      }
    ];
  };

  # IP forwarding is required to route packets between vm0 and the LAN/internet.
  boot.kernel.sysctl."net.ipv4.ip_forward" = 1;

  # ── MicroVM audit logging via nftables ────────────────────────────────────
  # Log every new TCP/UDP connection the VM initiates.  Connection records go
  # to journald (kernel facility) and are readable with:
  #   journalctl -k -g '\[vm-agent\]'
  # DNS hostnames are captured separately by dnsmasq below, which is more
  # readable for auditing than raw IP:port pairs.
  networking.nftables.enable = true;
  networking.nftables.tables."vm-agent-audit" = {
    family = "inet";
    content = ''
      chain vm-forward-log {
        type filter hook forward priority filter - 1;
        # Log new outbound connections (TCP + UDP; skip ICMP to reduce noise).
        iifname "vm0" ip protocol tcp  ct state new log prefix "[vm-agent] " level info
        iifname "vm0" ip protocol udp  ct state new log prefix "[vm-agent] " level info
      }
    '';
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

  # ── MicroVM: agent VM declaration ─────────────────────────────────────────
  # The host module (microvm.nixosModules.host, loaded in flake.nix) reads
  # this and creates microvm@lapdog-agent.service which runs as root and
  # handles TAP interface creation automatically.
  microvm.vms."lapdog-agent" = {
    # Don't start on boot — launch manually when you need an agent session.
    autoStart = false;
    # Pull the guest NixOS config from this flake's nixosConfigurations.
    flake = self;
  };

  # ── MicroVM: DNS logging via dnsmasq ──────────────────────────────────────
  # dnsmasq runs only on the vm0 bridge so it doesn't interfere with the
  # host's own DNS (systemd-resolved on 127.0.0.53).  All queries from the
  # VM are logged to journald with hostnames, making audits readable:
  #   journalctl -u dnsmasq -g 'query'
  #
  # To block a host after auditing, add to dnsmasq.settings in this file:
  #   address = [ "/unwanted.example.com/#" ];
  services.dnsmasq = {
    enable = true;
    resolveLocalQueries = false; # don't touch host DNS resolution
    settings = {
      interface = "vm0";
      bind-interfaces = true; # only listen on vm0, not all interfaces
      log-queries = true;
      # Forward to Cloudflare; change to "no-resolv = false" to use the
      # host's resolv.conf instead.
      no-resolv = true;
      server = [
        "1.1.1.1"
        "1.0.0.1"
      ];
    };
  };

  # Allow dog to start/stop the agent VM without a password prompt.
  security.sudo.extraRules = [
    {
      users = [ "dog" ];
      commands = [
        {
          command = "${pkgs.systemd}/bin/systemctl start microvm@lapdog-agent.service";
          options = [ "NOPASSWD" ];
        }
        {
          command = "${pkgs.systemd}/bin/systemctl stop microvm@lapdog-agent.service";
          options = [ "NOPASSWD" ];
        }
      ];
    }
  ];

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
