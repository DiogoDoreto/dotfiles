# Builds ghostel-module.so — the Emacs dynamic module for ghostel.
#
# Takes the pre-built libghostty-vt (static libs + headers) and compiles
# ghostel's Zig source into a shared library Emacs can load via module-load.
#
# ghostel source: dakra/ghostel @ ff6dc1b027b34dd5d9371b0af094a56c86ae2468
#
# ghostel's build.zig expects this directory layout relative to the source root:
#   vendor/ghostty/zig-out/lib/libghostty-vt.a
#   vendor/ghostty/zig-out/lib/libsimdutf.a
#   vendor/ghostty/zig-out/lib/libhighway.a
#   vendor/ghostty/zig-out/include/ghostty/vt.h   <- ghostty.zig @cInclude("ghostty/vt.h")
#   vendor/ghostty/include/ghostty.h              <- umbrella header (include path)
#
# EMACS_INCLUDE_DIR must point to the dir containing emacs-module.h.
# emacs.zig does: @cInclude("emacs-module.h")
{
  lib,
  stdenv,
  zig_0_15,
  emacs,
  ghostel-src,
  libghostty-vt,
}:
stdenv.mkDerivation {
  pname = "ghostel-module";
  version = "0.2.50";

  src = ghostel-src;

  nativeBuildInputs = [ zig_0_15 ];

  dontConfigure = true;

  buildPhase = ''
    export HOME=$TMPDIR

    # Stage pre-built static libs and headers where ghostel's build.zig expects them.
    # (preBuild hooks don't fire for custom buildPhase; staging is inlined here.)
    mkdir -p vendor/ghostty/zig-out/lib \
             vendor/ghostty/zig-out/include \
             vendor/ghostty/include

    # Static libraries
    ln -s ${libghostty-vt}/lib/libghostty-vt.a  vendor/ghostty/zig-out/lib/
    ln -s ${libghostty-vt}/lib/libsimdutf.a      vendor/ghostty/zig-out/lib/
    ln -s ${libghostty-vt}/lib/libhighway.a      vendor/ghostty/zig-out/lib/

    # VT headers (ghostty/vt.h and ghostty/vt/) — searched via zig-out/include
    ln -s ${libghostty-vt}/include/ghostty        vendor/ghostty/zig-out/include/ghostty

    # Umbrella header ghostty.h — searched via vendor/ghostty/include
    ln -s ${libghostty-vt}/include/ghostty.h      vendor/ghostty/include/ghostty.h

    export EMACS_INCLUDE_DIR="${emacs}/include"

    # ghostel has no external Zig deps (build.zig.zon .dependencies = .{})
    # so no --system flag is needed here.
    zig build -Doptimize=ReleaseFast
  '';

  installPhase = ''
    mkdir -p $out/lib
    cp ghostel-module.so $out/lib/
  '';

  meta = {
    description = "Emacs dynamic module for ghostel terminal emulator";
    homepage = "https://github.com/dakra/ghostel";
    license = lib.licenses.gpl3Only;
    platforms = lib.platforms.linux;
  };
}
