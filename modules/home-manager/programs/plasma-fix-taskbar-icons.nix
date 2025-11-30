{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.dog.programs.plasma-fix-taskbar-icons;

  # based on:
  # - https://discuss.kde.org/t/plasma-6-1-3-pinned-kde-application-icons-go-blank-after-gc-nixos/19444/3
  # - https://github.com/NixOS/nixpkgs/issues/308252#issuecomment-2543048917
  plasma-fix-taskbar-icons = pkgs.writeShellScriptBin "plasma-fix-taskbar-icons" ''
    sed -i 's|file:///nix/store/[^/]*/share/applications/|applications:|g' ~/.config/plasma-org.kde.plasma.desktop-appletsrc
    systemctl restart --user plasma-plasmashell
  '';
in
{
  options.dog.programs.plasma-fix-taskbar-icons = {
    enable = lib.mkEnableOption "plasma-fix-taskbar-icons";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ plasma-fix-taskbar-icons ];
  };
}
