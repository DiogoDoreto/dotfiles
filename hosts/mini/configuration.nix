# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running 'nixos-help').

{ pkgs, ... }:

let
  vars = import ./_variables.nix;
in

{
  imports = [
    ./hardware.nix # Include the results of the hardware scan.
    ./networking.nix
    ./caddy.nix
    ./backup.nix
    ./services/authentik.nix
    ./services/forgejo.nix
    ./services/freshrss.nix
    ./services/homepage.nix
    ./services/mail-me.nix
    ./services/media.nix
    ./services/nextcloud.nix
    ./services/opencode-web.nix
    ./services/librechat.nix
    # ./services/openwebui.nix
    ./services/kokoro.nix
    ./services/searx.nix
    ./services/vscodium-web.nix
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
  # services.pulseaudio.daemon.config = {
  #   default-sample-channels = 6;
  #   default-channel-map = "front-left,front-right,lfe,front-center,rear-left,rear-right";
  #   enable-lfe-remixing = "yes";
  # };
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
  };

  hardware.enableAllFirmware = true;

  # Bluetooth
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = false;
  };
  services.blueman.enable = true;

  users.users = {
    dog = {
      isNormalUser = true;
      description = "Diogo Doreto";
      extraGroups = [
        "networkmanager"
        "wheel"
        "caddy_www"
      ];
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFzvUuNy14x6avfx0mYrG3txTKgQZbTADajlZ7Sjk1bz dog@lapdog"
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDLqT5b5ZCZvpifcoIKZsY2g163FYTCfGScFXVBaW/XcQpNOO/FxOLlaNswlsOeIkuP3vZW7iDfllMq1DfnAvOSa3HSXIp5lu6fPiN+eTvTpVLO5t9/RbZvLOJVZQfS0D4+rX9SkF5+g0us+DjBO1JBr+bVCpZFhEMpvWlg7T2qxhO5aQyMW8G//HuUoCQV6yOcEVyvgg0xtTGRMEDVHn4lwSKn4zWtpvD3lZ0RGuotREbTfwz6eZ5WIeePKrfeNLGGjuP/HUKoE4IEXRbGvnTb/napwFiyuPZaGbFsLdwVjWpWymbfAr1LlB+CIhIG3NsBbgY8RGEl/sVd2ENcDHXZ16Cvml8nfzqdXEFmBBbsM1Sv9KClf0Q7IZyOnxCvQmpxuC6V+oKHxPKOPlIW2o+XT71SzKZhUBqIJw/bjS6oK3AS+VCt7ZKyD/GzZ4hshCH9fVtlhLrfNEsZbDkFSwrv7ZHoJVwh7VXukyA3iDtLY7oU2CTebUhx3Z1GKDbNjWYj23uMPWGwgnN7c9ipftOuMdkyJTRkvYCNGKWaYQzb9svmmZJjbmicY7I2r7/I9xUCa1W/IlLDc0AsbIEju0GgR4AbiL34xKsdmOkvx40h6CvECnohdMhuOK/B0985uOJip5t9+k6uVpHXWsrBQIPEuIb6oqZ50ljl5UAC/ihE+Q== ShellFish@iPhone-07032026"
      ];
    };

    root.openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFzvUuNy14x6avfx0mYrG3txTKgQZbTADajlZ7Sjk1bz dog@lapdog"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDLqT5b5ZCZvpifcoIKZsY2g163FYTCfGScFXVBaW/XcQpNOO/FxOLlaNswlsOeIkuP3vZW7iDfllMq1DfnAvOSa3HSXIp5lu6fPiN+eTvTpVLO5t9/RbZvLOJVZQfS0D4+rX9SkF5+g0us+DjBO1JBr+bVCpZFhEMpvWlg7T2qxhO5aQyMW8G//HuUoCQV6yOcEVyvgg0xtTGRMEDVHn4lwSKn4zWtpvD3lZ0RGuotREbTfwz6eZ5WIeePKrfeNLGGjuP/HUKoE4IEXRbGvnTb/napwFiyuPZaGbFsLdwVjWpWymbfAr1LlB+CIhIG3NsBbgY8RGEl/sVd2ENcDHXZ16Cvml8nfzqdXEFmBBbsM1Sv9KClf0Q7IZyOnxCvQmpxuC6V+oKHxPKOPlIW2o+XT71SzKZhUBqIJw/bjS6oK3AS+VCt7ZKyD/GzZ4hshCH9fVtlhLrfNEsZbDkFSwrv7ZHoJVwh7VXukyA3iDtLY7oU2CTebUhx3Z1GKDbNjWYj23uMPWGwgnN7c9ipftOuMdkyJTRkvYCNGKWaYQzb9svmmZJjbmicY7I2r7/I9xUCa1W/IlLDc0AsbIEju0GgR4AbiL34xKsdmOkvx40h6CvECnohdMhuOK/B0985uOJip5t9+k6uVpHXWsrBQIPEuIb6oqZ50ljl5UAC/ihE+Q== ShellFish@iPhone-07032026"
    ];
  };

  home-manager = {
    # install packages in /etc/profiles/per-user/dog
    useUserPackages = true;
    # reuse system pkgs
    useGlobalPkgs = true;
    users.dog = ./home.nix;
  };

  # Enable automatic login for the user.
  services.displayManager.autoLogin.enable = true;
  services.displayManager.autoLogin.user = "dog";

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  programs.firefox.enable = true;
  programs.fish.enable = true;
  programs.kdeconnect.enable = true;

  # List packages installed in system profile.
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

  # Trust the local Caddy root certificate so services can verify
  # certificates signed by Caddy's internal CA (for local domains).
  # A systemd service copies the cert from Caddy to the system trust store at boot.
  systemd.services.caddy-cert-trust =
    let
      setupCaCertScript = pkgs.writeShellScript "setup-caddy-ca-cert" ''
        set -e
        CADDY_CERT="/var/lib/caddy/.local/share/caddy/pki/authorities/local/root.crt"
        CUSTOM_CA_BUNDLE="/etc/ssl/certs/ca-bundle-with-local-ca.crt"

        if [ ! -f "$CADDY_CERT" ]; then
          echo "Error: Caddy certificate not found at $CADDY_CERT" >&2
          exit 1
        fi

        mkdir -p /etc/ssl/certs
        cp /etc/static/ssl/certs/ca-bundle.crt "$CUSTOM_CA_BUNDLE"

        # Append Caddy cert if not already present
        if ! grep -q "$(head -1 "$CADDY_CERT")" "$CUSTOM_CA_BUNDLE" 2>/dev/null; then
          echo "" >> "$CUSTOM_CA_BUNDLE"
          cat "$CADDY_CERT" >> "$CUSTOM_CA_BUNDLE"
        fi

        chmod 644 "$CUSTOM_CA_BUNDLE"
      '';
    in
    {
      description = "Setup Caddy root certificate for system CA bundle";
      wantedBy = [ "multi-user.target" ];
      before = [ "caddy.service" ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = "yes";
        ExecStart = setupCaCertScript;
      };
    };

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    ports = [ vars.ports.openssh ];
    # require public key authentication for better security
    settings = {
      PasswordAuthentication = false;
      KbdInteractiveAuthentication = false;
    };
  };

  services.postgresql = {
    enable = true;
    ensureDatabases = [ "dog" ];
    ensureUsers = [
      {
        name = "dog";
        ensureDBOwnership = true;
        ensureClauses.createdb = true;
      }
    ];
  };

  virtualisation.podman.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It's perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.11"; # Did you read the comment?

}
