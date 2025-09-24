{ pkgs, ... }:

{
  imports = [
    ../../modules/home-manager
  ];

  home = {
    packages = with pkgs; [
      aider-chat
      blender
      calibre
      hydrogen
      kdePackages.kdenlive
      keepassxc
      krita
      nodejs_24
      obs-studio
      pods # podman GUI
      ungoogled-chromium
      onedrivegui
    ];

    file = {
      ".aider.conf.yml".text = builtins.toJSON {
        model = "github_copilot/gpt-4.1";
        show-model-warnings = false;
        max-chat-history-tokens = 8000;
        dark-mode = true;
        auto-commits = false;
      };
      ".aider.model.settings.yml".text = builtins.toJSON [
        {
          name = "github_copilot/gpt-4.1";
          extra_params = {
            max_tokens = 80000;
            extra_headers = {
              User-Agent = "GithubCopilot/1.155.0";
              Editor-Plugin-Version = "copilot/1.155.0";
              Editor-Version = "vscode/1.85.1";
              Copilot-Integration-Id = "vscode-chat";
            };
          };
        }
      ];
    };
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
      bindings = {
        "Alt+RIGHT" = "cycle_values video-rotate 90 180 270 0";
        "Alt+LEFT" = "cycle_values video-rotate 0 270 180 90";
      };
    };

    neovim.enable = true;

    ssh = {
      enable = true;
      enableDefaultConfig = false;
      matchBlocks."*".addKeysToAgent = "yes";
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
        "evremap Virtual input for /dev/input/event0"
      ];
    };
  };

  services.kdeconnect = {
    enable = true;
    indicator = true;
  };
}
