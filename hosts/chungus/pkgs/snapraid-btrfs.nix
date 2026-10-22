# Based on https://discourse.nixos.org/t/setting-up-snapraid-btrfs-on-nixos/46476/8
{
  symlinkJoin,
  fetchFromGitHub,
  writeScriptBin,
  makeWrapper,
  coreutils,
  gnugrep,
  gawk,
  gnused,
  snapraid,
  snapper,
  bash,
}:

let
  name = "snapraid-btrfs";
  deps = [
    coreutils
    gnugrep
    gawk
    gnused
    snapraid
    snapper
  ];
  # NOTE: Forked version from D34DC3N73R to fix snapper 0.11.1 compatibility.
  src = fetchFromGitHub {
    owner = "D34DC3N73R";
    repo = "snapraid-btrfs";
    rev = "ea9a1cfbfbe1cefcae9c038e1a4962d4bc2de843";
    sha256 = "sha256-+UCBGlGFqRKgFjCt1GdOSxaayTONfwisxdnZEwxOnSY=";
  };
  script = writeScriptBin name (
    builtins.replaceStrings [ "#!/bin/bash -" ] [ "#!${bash}/bin/bash -" ] (
      builtins.readFile (src + "/snapraid-btrfs")
    )
  );
in
symlinkJoin {
  inherit name;
  paths = [ script ] ++ deps;
  buildInputs = [ makeWrapper ];
  # Isolate the verified post-recovery working set from old snapshot labels.
  postBuild = ''
    wrapProgram $out/bin/${name} \
      --set PATH $out/bin \
      --set SNAPRAID_USERDATA_KEY snapraid-btrfs-f18
  '';
}
