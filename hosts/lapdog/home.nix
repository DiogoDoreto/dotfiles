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
      nodejs_24
      pods # podman GUI
      ungoogled-chromium
      onedrivegui
    ];
  };

  xdg.desktopEntries.plasma-toggle-tablet-mode = {
    name = "Plasma Toggle Tablet Mode";
    exec = "${pkgs.plasma-toggle-tablet-mode}/bin/plasma-toggle-tablet-mode";
    icon = "preferences-system-tablet";
    terminal = false;
    categories = [ "Utility" ];
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
  };

  services.kdeconnect = {
    enable = true;
    indicator = true;
  };
}
