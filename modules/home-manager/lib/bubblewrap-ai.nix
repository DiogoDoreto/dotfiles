{
  # Custom params:
  # The package that will be bubblewrapped
  package,
  # We default to wrapping the `mainProgram`, but use `wrappedBinName` to overwrite this
  wrappedBinName ? "",
  # List of paths that the wrapped command will have read-only access
  extraReadOnlyPaths ? [ ],
  # List of paths that the wrapped command will have write access
  extraWritablePaths ? [ ],
  # Any extra flags that should always be provided to the wrapped command
  extraCommandFlags ? [ ],

  # args provided by callPackage
  lib,
  stdenv,
  bubblewrap,
  writeShellScript,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = package.pname + "-bubblewrapped";
  version = package.version;
  meta = package.meta;

  phases = [ "installPhase" ];

  installPhase =
    let
      mainProgram = package.meta.mainProgram;
      finalBinName = if wrappedBinName != "" then wrappedBinName else mainProgram;
      bwrap = lib.getExe bubblewrap;
      defaultReadOnlyPaths = [
        "/usr"
        "/bin"
        "/lib64"
        "/etc"
        "/nix"
        "/run"
        "~/.config/git"
        "~/.local/share/direnv"
      ];
      defaultWritablePaths = [
        # "/nix/var/nix/daemon-socket" # For package installs
      ];
      readOnlyPaths = defaultReadOnlyPaths ++ extraReadOnlyPaths;
      writablePaths = defaultWritablePaths ++ extraWritablePaths;
      # based on https://github.com/numtide/nix-ai-tools/blob/main/packages/claudebox/claudebox.sh
      wrapperScript = writeShellScript "${mainProgram}-bubblewrapped.sh" ''
        # Create isolated home directory (protects real home from YOLO mode)
        fake_home=$(mktemp -d)
        at_exit() {
          rm -rf "$fake_home"
        }
        trap at_exit EXIT

        bwrap_args=(
          --dev /dev
          --proc /proc
          --tmpfs /tmp
          --bind "$fake_home" "$HOME"
          ${lib.concatMapStrings (path: "--ro-bind ${path} ${path}\n") readOnlyPaths}
          ${lib.concatMapStrings (path: "--bind ${path} ${path}\n") writablePaths}
          --unshare-all
          --share-net
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
      cp ${wrapperScript} $out/bin/${finalBinName}
    '';
})
