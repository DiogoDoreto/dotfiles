{ config, pkgs, lib, inputs, ... }:
{
  imports = [
    inputs.nur.hmModules.nur
    # todo: remove when https://github.com/nix-community/home-manager/pull/5355 gets merged:
    (builtins.fetchurl {
      url = "https://raw.githubusercontent.com/Smona/home-manager/nixgl-compat/modules/misc/nixgl.nix";
      sha256 = "0g5yk54766vrmxz26l3j9qnkjifjis3z2izgpsfnczhw243dmxz9";
    })

    ./modules/home-server.nix
    ../programs
  ];

  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "dog";
  home.homeDirectory = "/home/dog";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "24.05"; # Please read the comment before changing.

  home.keyboard = {
    layout = "us";
    options = [ "compose:ralt" ];
  };

  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "reaper"
    "spotify"
  ];

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = with pkgs; [
      (nerdfonts.override { fonts = [ "VictorMono" ]; })
      nodejs_20
      tldr
      cheat
      turbovnc
      htop
      ffmpeg_7-full

      # build tools
      cmake
      gcc
      gnumake
      libtool

      nil # nix lsp server

      nixgl.nixGLIntel

      rofimoji
      rofi-power-menu
      keepassxc
      reaper
      spotify
      openvpn
      qbittorrent
    ];

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';

    ".config/cheat/cheatsheets/community".source = pkgs.fetchFromGitHub {
      owner = "cheat";
      repo = "cheatsheets";
      rev = "36bdb99dcfadde210503d8c2dcf94b34ee950e1d";
      hash = "sha256-Afv0rPlYTCsyWvYx8UObKs6Me8IOH5Cv5u4fO38J8ns=";
    };
    ".config/cheat/cheatsheets/personal".source = ../.config/cheat/cheatsheets/personal;
    ".config/cheat/conf.yml".text = builtins.toJSON {
      editor = lib.getExe pkgs.emacs;
      colorize = true;
      style = "onedark";
      formatter = "terminal16m";
      pager = "less";
      cheatpaths = [
        {
          name = "community";
          path = "~/.config/cheat/cheatsheets/community";
          tags = [ "community" ];
          readonly = true;
        }
        {
          name = "personal";
          path = "~/.config/cheat/cheatsheets/personal";
          tags = [ "personal" ];
          readonly = false;
        }
      ];
    };

    ".Xresources".text = ''
      Xft.dpi: 140
    '';

    ".vnc/xstartup" = {
      executable = true;
      text = ''
        #!/bin/sh
        export I3SOCK="~/.i3/i3-ipc.sock"
        i3
      '';
    };
  };

  # Home Manager can also manage your environment variables through
  # 'home.sessionVariables'. These will be explicitly sourced when using a
  # shell provided by Home Manager. If you don't want to manage your shell
  # through Home Manager then you have to manually source 'hm-session-vars.sh'
  # located at either
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  ~/.local/state/nix/profiles/profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/dog/etc/profile.d/hm-session-vars.sh
  #
  home.sessionVariables = {
  };

  fonts.fontconfig = {
    enable = true;
  };

  # settings that make Home Manager work better on GNU/Linux distributions other than NixOS
  targets.genericLinux.enable = true;
  nixGL.prefix = "${pkgs.nixgl.nixGLIntel}/bin/nixGLIntel";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  dog = {
    # My Home Server setup
    home-server.enable = true;

    programs = {
      emacs.enable = true;
      firefox.enable = true;
      onedrive.enable = true;
      podman.enable = true;
    };
  };

  # shell stuff
  programs.atuin = {
    enable = true;
    enableFishIntegration = true;
  };
  programs.bash.enable = true;
  programs.fish = {
    enable = true;
    shellAbbrs = {
      e = "emacsclient -nc";
    };
  };
  programs.starship = {
    enable = true;
    enableFishIntegration = true;
  };
  programs.zoxide = {
    enable = true;
    enableFishIntegration = true;
  };

  # CLI tools
  programs.fd.enable = true;
  programs.fzf.enable = true;
  programs.ripgrep.enable = true;

  programs.git = {
    enable = true;
    userName = "Diogo Doreto";
    includes = [
      { path = "~/.config/git/config.private"; } # set user.email here
    ];
    aliases = {
      co = "checkout";
      st = "status -sb";
      lg = "log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit";
      fm = "fetch origin master:master";
      pf = "push --force-with-lease";
      undo = "reset --soft HEAD~1";
      ri = "rebase -i";
      rc = "rebase --continue";
      ra = "rebase --abort";
    };
    ignores = [
      # npm/yarn
      "npm-debug.log*"
      "yarn-debug.log*"
      "yarn-error.log*"
    ];
    extraConfig = {
      pull = {
        ff = "only";
      };
      rebase = {
        autoSquash = true;
      };
    };
  };

  programs.wezterm = {
    enable = true;
    package = (config.lib.nixGL.wrap pkgs.wezterm);
    extraConfig = ''
      return {
        font = wezterm.font {
          family = "VictorMono Nerd Font Propo",
          weight = "Medium",
          harfbuzz_features = { 'calt=0', 'clig=0', 'liga=0' },
        },
        color_scheme = "duskfox",
        font_size = 10,
        line_height = 1.1,
        default_prog = { "${lib.getExe pkgs.fish}", "-l" },
        window_decorations = "RESIZE",
        window_padding = {
          left = "1cell",
          right = "1cell",
          top = "0.5cell",
          bottom = "0.5cell",
        },
        use_fancy_tab_bar = false,
        hide_tab_bar_if_only_one_tab = true,
      }
    '';
  };

  programs.rofi = {
    enable = true;
    font = "VictorMono Nerd Font 12";
    theme = "${inputs.rofi-material-ocean}/material-ocean.rasi";
    terminal = "wezterm";
    extraConfig = {
      show-icons = true;
      combi-hide-mode-prefix = true;
    };
  };

  xsession = {
    enable = true;

    windowManager.i3 = {
      enable = true;
      config = {
        menu = "${lib.getExe pkgs.rofi} -show combi -modes combi -combi-modes \"window#drun#Power:rofi-power-menu --choices=shutdown/reboot --confirm=logout\"";
        modifier = "Mod4"; # Windows key
        terminal = "wezterm";
        gaps.inner = 10;
        keybindings = let
          modifier = config.xsession.windowManager.i3.config.modifier;
        in lib.mkOptionDefault {
          # Mod1 == Alt key
          "Ctrl+Shift+Mod1+e" = "exec emacsclient -nc";
          "Ctrl+Shift+Mod1+f" = "exec firefox";
          "Ctrl+Shift+Mod1+k" = "exec keepassxc";
          "Ctrl+Shift+Mod1+m" = "exec spotify";
          "Ctrl+Shift+Mod1+w" = "exec wezterm";
          "${modifier}+period" = "exec rofimoji";
        };
      };
    };
  };

  systemd.user = {
    enable = true;
    startServices = "sd-switch";
    systemctlPath = "/usr/bin/systemctl";
  };

  services.ssh-agent.enable = true;
}
