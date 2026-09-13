{ pkgs, config, ... }:

{
  home = {
    packages = with pkgs; [
      calibre
      keepassxc
      onedrivegui
      pods # podman GUI
      ungoogled-chromium
      yt-dlp

      # To function the browser extension must be installed and open-in-mpv must be set as the default scheme-handler for mpv:// eg.:
      #   xdg-mime default open-in-mpv.desktop x-scheme-handler/mpv
      # https://addons.mozilla.org/en-US/firefox/addon/iina-open-in-mpv/
      open-in-mpv
    ];

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
      package = pkgs.mpv.override {
        youtubeSupport = true;
        scripts = with pkgs.mpvScripts; [
          uosc # Feature-rich minimalist proximity-based UI for MPV player
          mpris # allows control of the player using standard media keys
          # YouTube improvements
          sponsorblock
          quality-menu
          youtube-upnext
        ];
      };
      config = {
        "ytdl-raw-options" = "cookies-from-browser=chromium";
        # "ytdl-raw-options" = "extractor-args=\"youtube:player_client=default,web_embedded\"";
        "ytdl-format" = "bestvideo+bestaudio[channels>2]/bestvideo+bestaudio/best";
      };
    };

    nh = {
      enable = true;
      flake = config.dog.dotfilesPath + "/hosts/mini";
    };

    neovim = {
      enable = true;
      withPython3 = false;
      withRuby = false;
    };
  };

  dog.dotfilesPath = /home/dog/projects/dotfiles;

  dog.presets.linux.enable = true;

  dog.programs = {
    cli-tools.enable = true;
    emacs.enable = true;
    firefox = {
      enable = true;
      plasma-integration = true;
    };
    ghostty.enable = true;
    git.enable = true;
    plasma-fix-taskbar-icons.enable = true;

    opencode = {
      enable = true;
      extraWritablePaths = [
        "~/projects/"
      ];
      extraReadOnlyPaths = [
        "/var/run/postgresql"
      ];
    };
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
