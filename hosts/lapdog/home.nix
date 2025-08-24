{ pkgs, ... }:

{
  imports = [
    ../../modules/home-manager
  ];

  home = {
    packages = with pkgs; [
      calibre
      hydrogen
      keepassxc
      krita
      nodejs_24
      pods # podman GUI
      ungoogled-chromium
      onedrivegui
    ];
  };

  programs = {
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

    ssh = {
      enable = true;
      addKeysToAgent = "yes";
    };
  };

  dog.dotfilesPath = /home/dog/projects/dotfiles;

  dog.presets.linux.enable = true;

  dog.programs = {
    cli-tools.enable = true;
    git.enable = true;
    firefox.enable = true;
    wezterm.enable = true;

    emacs = {
      enable = true;
    };

    plasma-toggle-tablet-mode = {
      enable = true;
      devices = [
        "AT Translated Set 2 keyboard"
        "ELAN06D5:00 04F3:32B7 Touchpad"
        "TPPS/2 Elan TrackPoint"
        "ThinkPad Extra Buttons"
      ];
    };
  };

  services.kdeconnect = {
    enable = true;
    indicator = true;
  };
}
