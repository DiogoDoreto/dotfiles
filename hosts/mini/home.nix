{ pkgs, pkgs-unstable, inputs, ... }:

{
  imports = [
    ../../home/_core.nix
    ../../home/_linux.nix
    ./plasma.nix
  ];

  home = {
    packages = (with pkgs; [
      calibre
      inputs.home-manager.defaultPackage.x86_64-linux
      keepassxc
      moonlight-qt
      nodejs_23
      pods # podman GUI
      qbittorrent
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
}
