{ pkgs, lib, ... }:

{
  home.packages = with pkgs; [
    (reaper.overrideAttrs (prev: {
      # systemd-inhibit ensures the laptop won't go to sleep during the app lifetime
      postInstall = (prev.postInstall or "") + ''
        substituteInPlace $out/share/applications/cockos-reaper.desktop \
          --replace-fail "Exec=reaper" "Exec=systemd-inhibit reaper"
      '';
    }))

    # wineWow
    wineWowPackages.yabridge
    yabridge
    yabridgectl
  ];

  # needed for yabridgectl
  home.sessionVariablesExtra = lib.mkAfter ''
    export NIX_PROFILES="$NIX_PROFILES /etc/profiles/per-user/$USER"
  '';
}
