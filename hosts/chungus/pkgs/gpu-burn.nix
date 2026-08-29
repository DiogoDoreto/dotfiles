{
  pkgs ? import <nixpkgs> {
    config = {
      allowUnfree = true;
      allowBroken = true;
    };
  },
}:

(pkgs.gpu-burn.overrideAttrs (old: {
  meta = old.meta // {
    broken = false;
    platforms = [ "x86_64-linux" ];
    badPlatforms = [ ];
  };
}))
