{ pkgs, pkgs-unstable, inputs, ... }:

{
  imports = [
    inputs.plasma-manager.homeManagerModules.plasma-manager
    ../../home/_core.nix
    ../../home/_linux.nix
    ./plasma.nix
  ];

  home = {
    packages = (with pkgs; [
      calibre
      inputs.home-manager.packages.x86_64-linux.home-manager
      keepassxc
      moonlight-qt
      nodejs_24
      pods # podman GUI
      ungoogled-chromium
    ]) ++ (with pkgs-unstable; [
      ardour
      onedrivegui
    ]);
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

  dog.programs = {
    cli-tools.enable = true;
    git.enable = true;
    firefox.enable = true;
    ghostty.enable = true;

    emacs = {
      enable = true;
      extraConfig = builtins.readFile ./emacs-extra.el;
    };

    aider = {
      enable = true;
      ollamaApiBase = "http://chungus.home:11434";
    };
  };

  services.podman = {
    enable = true;
    containers = {
      home-assistant = {
        image = "ghcr.io/home-assistant/home-assistant:stable";
        autoStart = true;
        environment = { TZ = "Europe/Madrid"; };
        network = "host";
        addCapabilities = [ "CAP_NET_RAW" "CAP_NET_BIND_SERVICE" ];
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
