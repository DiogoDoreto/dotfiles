{
  lib,
  stdenv,
  fetchFromGitHub,
  autoPatchelfHook,
  expat,
  zlib,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "ipu7-camera-bins";
  version = "unstable-2026-02-09";

  src = fetchFromGitHub {
    repo = "ipu7-camera-bins";
    owner = "intel";
    rev = "403c67db6b279dd02752f11db6a34552f31a3ac5";
    hash = "sha256-Sj1jBOOegTk8tdmDN06MYEa7KmutnfSb5AEhXhoQkSc=";
  };

  nativeBuildInputs = [
    autoPatchelfHook
    (lib.getLib stdenv.cc.cc)
    expat
    zlib
  ];

  installPhase = ''
    runHook preInstall

    mkdir -p $out
    cp --no-preserve=mode --recursive \
      lib \
      include \
      $out/

    runHook postInstall
  '';

  postFixup = ''
    for lib in $out/lib/lib*.so.*; do \
      lib=''${lib##*/}; \
      target=$out/lib/''${lib%.*}; \
      if [ ! -e "$target" ]; then \
        ln -s "$lib" "$target"; \
      fi \
    done

    for pcfile in $out/lib/pkgconfig/*.pc; do
      substituteInPlace $pcfile \
        --replace 'prefix=/usr' "prefix=$out"
    done
  '';

  meta = with lib; {
    description = "IPU firmware and proprietary image processing libraries";
    homepage = "https://github.com/intel/ipu7-camera-bins";
    license = licenses.issl;
    sourceProvenance = with sourceTypes; [
      binaryFirmware
    ];
    maintainers = [ ];
    platforms = [ "x86_64-linux" ];
  };
})
