{ pkgs, lib, config, ... }:
{
  imports = [
    ./_core.nix
    ./_linux.nix
    ./modules/home-server.nix
  ];

  home = {
    username = "dog";
    homeDirectory = "/home/dog";
  };

  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "reaper"
    "spotify"
  ];

  home.packages = with pkgs; [
      nodejs_20
      cheat
      turbovnc
      mpv

      keepassxc
      onedrivegui
      openvpn
      qbittorrent
      reaper
      spotify
      (config.lib.nixGL.wrap remmina)
  ];

  home.file = {
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

    ".vnc/xstartup" = {
      executable = true;
      text = ''
        #!/bin/sh
        export I3SOCK="~/.i3/i3-ipc.sock"
        i3
      '';
    };
  };

  home.sessionVariables = {
  };

  dog = {
    home-server.enable = true;

    programs = {
      blueman.enable = true;
      cli-tools.enable = true;
      emacs.enable = true;
      firefox.enable = true;
      flameshot.enable = true;
      ghostty.enable = true;
      git.enable = true;
      gromit-mpx.enable = true;
      i3.enable = true;
      podman.enable = true;
    };
  };

  xsession.windowManager.i3.config = {
    startup = [
      { command = "exec onedrivegui"; always = false; notification = false; }
    ];
  };
}
