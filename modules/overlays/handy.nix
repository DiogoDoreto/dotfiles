# Apply https://github.com/cjpais/Handy/pull/2360 without updating Handy.
final: prev:
let
  handy = prev.handy;
  patchedSrc = final.applyPatches {
    name = "handy-webkit-renderer-source";
    src = handy.src;
    patches = [
      (final.fetchurl {
        url = "https://github.com/cjpais/Handy/commit/ac256bb8fe00742f044ee824a07f6be6ec42dd5c.patch";
        hash = "sha256-lAWdAg02sayFOMTEDhol4jVDWO4D9fa0bYj7dzta18E=";
      })
    ];
  };
in
{
  handy = handy.overrideAttrs (old: {
    src = patchedSrc;
    # The patch adds a direct webkit2gtk-sys dependency to the lockfile.
    cargoDeps = final.rustPlatform.importCargoLock {
      lockFile = "${patchedSrc}/src-tauri/Cargo.lock";
      allowBuiltinFetchGit = true;
    };
    # Patching flake.nix in src does not change the already-evaluated derivation.
    # Apply its wrapper change here too, so runtime renderer selection can run.
    preFixup =
      final.lib.replaceStrings [ "--set-default WEBKIT_DISABLE_DMABUF_RENDERER 1" ] [ "" ]
        old.preFixup;
  });
}
