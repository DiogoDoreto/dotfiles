{ pkgs, pkgs-unstable, ... }:

{
  imports = [
    ../../modules/home-manager
    ./plasma.nix
  ];

  home = {
    packages =
      (with pkgs; [
        calibre
        keepassxc
        nodejs_24
        pods # podman GUI
        ungoogled-chromium
      ])
      ++ (with pkgs-unstable; [
        onedrivegui
      ]);

    # I remember this fixed something, but I don't recall what. So I'm leaving
    # it commented until it breaks again :D
    # sessionVariablesExtra = ''
    #   export NIX_PROFILES="$NIX_PROFILES /etc/profiles/per-user/$USER"
    # '';
  };

  programs = {
    bash = {
      initExtra = ''
        if [[ $TERM == "dumb" ]]; then
          export PS1="$ "
        fi
      '';
    };

    mpv = {
      enable = true;
      scripts = with pkgs.mpvScripts; [
        uosc # Feature-rich minimalist proximity-based UI for MPV player
        mpris # allows control of the player using standard media keys
        # YouTube improvements
        sponsorblock
        quality-menu
        youtube-upnext
      ];
    };

    neovim.enable = true;
  };

  dog.dotfilesPath = /home/dog/projects/dotfiles;

  dog.presets.linux.enable = true;

  dog.programs = {
    cli-tools.enable = true;
    emacs.enable = true;
    firefox.enable = true;
    ghostty.enable = true;
    git.enable = true;
  };

  services.podman = {
    enable = true;
    autoUpdate.enable = true;

    containers = {
      home-assistant = {
        image = "ghcr.io/home-assistant/home-assistant:stable";
        autoStart = true;
        autoUpdate = "registry";
        environment = {
          TZ = "Europe/Madrid";
        };
        network = "host";
        addCapabilities = [
          "CAP_NET_RAW"
          "CAP_NET_BIND_SERVICE"
        ];
        volumes = [
          "/home/dog/projects/home-assistant-config/config:/config"
          "/run/dbus:/run/dbus:ro"
        ];
        extraPodmanArgs = [ "--privileged" ];
      };
    };
  };

  services.kdeconnect = {
    enable = true;
    indicator = true;
  };
}
