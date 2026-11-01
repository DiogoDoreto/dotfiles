{
  pkgs,
  lib,
  config,
  ...
}:

{
  imports = [
    # ./ai.nix
  ];

  home = {
    packages = with pkgs; [
      podman-compose
      pods
      ungoogled-chromium
      nvtopPackages.nvidia
      (llm-agents.t3code.override { providerPackages = [ ]; })
      (llm-agents.t3code.override { providerPackages = [ ]; }).desktop
    ];
  };

  # The desktop connects to the persistent service, never a second local backend.
  # Keep the settings mutable so UI-managed preferences survive activation.
  home.activation.t3codeRemoteOnly = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    settings_dir="$HOME/.t3/userdata"
    settings_file="$settings_dir/desktop-settings.json"
    if [ ! -e "$settings_file" ] || ! ${pkgs.jq}/bin/jq -e '.localEnvironmentEnabled == false' "$settings_file" >/dev/null; then
      run mkdir -p "$settings_dir"
      if [[ ! -v DRY_RUN ]]; then
        settings_tmp=$(mktemp "$settings_dir/.desktop-settings.XXXXXX")
        if ${pkgs.jq}/bin/jq '. + {localEnvironmentEnabled: false}' \
          <(if [ -e "$settings_file" ]; then ${pkgs.coreutils}/bin/cat "$settings_file"; else echo '{}'; fi) > "$settings_tmp"; then
          mv "$settings_tmp" "$settings_file"
        else
          rm -f "$settings_tmp"
          exit 1
        fi
      fi
    fi
  '';

  programs = {
    bash = {
      initExtra = ''
        if [[ $TERM == "dumb" ]]; then
          export PS1="$ "
        fi
      '';
    };

    starship = {
      enableBashIntegration = lib.mkForce false;
    };

    neovim = {
      enable = true;
      withPython3 = false;
      withRuby = false;
    };

    nh = {
      enable = true;
      flake = config.dog.dotfilesPath + "/hosts/chungus";
    };

    mpv = {
      enable = true;
      scripts = with pkgs.mpvScripts; [
        uosc # Feature-rich minimalist proximity-based UI for MPV player
        mpris # allows control of the player using standard media keys
      ];
    };
  };

  dog.presets.linux.enable = true;

  dog.dotfilesPath = /home/dog/p/dotfiles;

  dog.programs = {
    cli-tools.enable = true;
    pi = {
      enable = true;
      extraWritablePaths = [ "~/p/" ];
    };
    git.enable = true;
    emacs.enable = true;
    firefox = {
      enable = true;
      plasma-integration = true;
    };
    ghostty.enable = true;
    plasma-fix-taskbar-icons.enable = true;
    opencode = {
      enable = true;
      package = pkgs.llm-agents.opencode2;
      extraWritablePaths = [
        "~/p/"
      ];
    };
  };

  services.kdeconnect = {
    enable = true;
    indicator = true;
  };

  services.podman = {
    enable = true;
    containers = {
    };
  };
}
