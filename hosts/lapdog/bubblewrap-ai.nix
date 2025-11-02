{
  package ? null,
  extraReadOnlyPaths ? [ ],
  extraWritablePaths ? [ ],
  extraCommandFlags ? [ ],

  lib,
  stdenv,
  bubblewrap,
  writeShellScript,
}:

assert lib.asserts.assertMsg (package != null) "The 'package' parameter must not be null";

stdenv.mkDerivation (finalAttrs: {
  pname = package.pname + "-wrapped";
  version = package.version;
  meta = package.meta;

  phases = [ "installPhase" ];

  installPhase =
    let
      mainProgram = package.meta.mainProgram;
      bwrap = lib.getExe bubblewrap;
      # based on https://github.com/numtide/nix-ai-tools/blob/main/packages/claudebox/claudebox.sh
      wrapperScript = writeShellScript "${mainProgram}-wrapper.sh" ''
        # Create isolated home directory (protects real home from YOLO mode)
        fake_home=$(mktemp -d)
        at_exit() {
          rm -rf "$fake_home"
        }
        trap at_exit EXIT

        bwrap_args=(
          --dev /dev
          --proc /proc
          --ro-bind /usr /usr
          --ro-bind /bin /bin
          --ro-bind /lib64 /lib64
          --ro-bind /etc /etc
          --ro-bind /nix /nix
          ${lib.concatMapStrings (path: "--ro-bind ${path} ${path}\n") extraReadOnlyPaths}
          --bind /nix/var/nix/daemon-socket /nix/var/nix/daemon-socket # For package installs
          --tmpfs /tmp
          --bind "$fake_home" "$HOME"           # Isolated home (YOLO safety)
          ${lib.concatMapStrings (path: "--bind ${path} ${path}\n") extraWritablePaths}
          --unshare-all
          --share-net
          --ro-bind /run /run
          --setenv HOME "$HOME"
          --setenv USER "$USER"
          --setenv PATH "$PATH"
          --setenv TMPDIR "/tmp"
          --setenv TEMPDIR "/tmp"
          --setenv TEMP "/tmp"
          --setenv TMP "/tmp"
          ${package}/bin/${mainProgram}
          ${lib.concatStringsSep "\n" extraCommandFlags}
        )

        ${bwrap} "''\${bwrap_args[@]}" "$@"
      '';
    in
    ''
      mkdir -p $out/bin
      cp ${wrapperScript} $out/bin/${mainProgram}
    '';
})
