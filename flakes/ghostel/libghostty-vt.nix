# Builds libghostty-vt.a (and its bundled C++ deps libsimdutf.a, libhighway.a)
# from the ghostty commit vendored by ghostel.
#
# ghostty source: ghostty-org/ghostty @ ba398dfff3e30ff83da07140981ca138410cf608
# (version 1.3.2-dev, minimum_zig_version = "0.15.2")
#
# highway and simdutf are local path deps inside ghostty's own source tree
# (./pkg/highway, ./pkg/simdutf), compiled by zig from source — no pre-fetching.
# All other external Zig deps are supplied offline via ghostty-deps.nix (linkFarm),
# passed to zig with --system so the sandbox has no network access.
#
# Output layout:
#   $out/lib/libghostty-vt.a
#   $out/lib/libhighway.a
#   $out/lib/libsimdutf.a
#   $out/include/ghostty.h          <- umbrella C header (from include/ghostty.h)
#   $out/include/ghostty/vt.h       <- VT API header (installed by zig build)
#   $out/include/ghostty/vt/        <- VT sub-headers
{
  lib,
  stdenv,
  zig_0_15,
  callPackage,
  fetchFromGitHub,
}:
let
  ghosttySrc = fetchFromGitHub {
    owner = "ghostty-org";
    repo = "ghostty";
    rev = "ba398dfff3e30ff83da07140981ca138410cf608";
    hash = "sha256-Tdf9W6ZMvxfMEnMZ3p9LvmizMFPWpP2Evb34zrRzP3c=";
  };

  zigDeps = callPackage ./ghostty-deps.nix { inherit zig_0_15; };
in
stdenv.mkDerivation {
  pname = "libghostty-vt";
  # Tracks the ghostty submodule commit in ghostel; bump together with ghostty rev above.
  version = "1.3.2-dev-ba398df";

  src = ghosttySrc;

  nativeBuildInputs = [ zig_0_15 ];

  # Set cache dirs at derivation level so all phases see consistent paths.
  # ZIG_GLOBAL_CACHE_DIR is a zig-recognised env var; local cache is redirected
  # via --cache-dir in buildPhase so highway/simdutf .a files land in a known
  # location rather than the source tree's .zig-cache.
  ZIG_GLOBAL_CACHE_DIR = "$TMPDIR/zig-global-cache";

  dontConfigure = true;

  buildPhase = ''
    export HOME=$TMPDIR

    # Build libghostty-vt (static) and its bundled C++ deps.
    # --system points zig to the pre-fetched offline package store.
    # --cache-dir redirects the per-project cache to a known path so highway
    # and simdutf .a files are findable from installPhase.
    zig build \
      -Demit-lib-vt=true \
      -Doptimize=ReleaseFast \
      --system ${zigDeps} \
      --cache-dir "$TMPDIR/zig-local-cache"
  '';

  installPhase = ''
    mkdir -p $out/lib $out/include/ghostty

    cp zig-out/lib/libghostty-vt.a $out/lib/

    # highway and simdutf are compiled from ./pkg/* as Zig packages and their
    # .a files land somewhere under $ZIG_GLOBAL_CACHE_DIR/o/<hash>/.
    # ghostel's build.zig needs them as explicit link inputs.
    highway_a=$(find "$TMPDIR/zig-local-cache" "$TMPDIR/zig-global-cache" \
                     -name 'libhighway.a' -print -quit 2>/dev/null)
    simdutf_a=$(find "$TMPDIR/zig-local-cache" "$TMPDIR/zig-global-cache" \
                     -name 'libsimdutf.a' -print -quit 2>/dev/null)

    if [ -z "$highway_a" ]; then
      echo "ERROR: libhighway.a not found in $ZIG_GLOBAL_CACHE_DIR" >&2
      find "$ZIG_GLOBAL_CACHE_DIR" -name '*.a' | head -20 >&2
      exit 1
    fi
    if [ -z "$simdutf_a" ]; then
      echo "ERROR: libsimdutf.a not found in $ZIG_GLOBAL_CACHE_DIR" >&2
      find "$ZIG_GLOBAL_CACHE_DIR" -name '*.a' | head -20 >&2
      exit 1
    fi

    cp "$highway_a" $out/lib/libhighway.a
    cp "$simdutf_a" $out/lib/libsimdutf.a

    # Top-level umbrella C header (ghostty.h) — ghostel stages this at
    # vendor/ghostty/include/ghostty.h so its build.zig include path resolves.
    cp include/ghostty.h $out/include/

    # VT sub-headers installed by zig build into zig-out/include/ghostty/.
    # ghostel's build.zig adds vendor/ghostty/zig-out/include, so ghostty/vt.h
    # (the one actually #included by ghostel's src/ghostty.zig) resolves there.
    cp -r zig-out/include/ghostty/. $out/include/ghostty/
  '';

  meta = {
    description = "libghostty-vt static library for ghostel (Emacs terminal emulator)";
    homepage = "https://ghostty.org/";
    license = lib.licenses.mit;
    platforms = lib.platforms.linux;
  };
}
