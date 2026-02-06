{
  lib,
  stdenv,
  fetchFromGitHub,
  meson,
  ninja,
  pkg-config,
  qt6,
  kdePackages,
  libxkbcommon,
}:

stdenv.mkDerivation rec {
  pname = "kwtype";
  version = "0.1.0";

  src = fetchFromGitHub {
    owner = "Sporif";
    repo = "KWtype";
    rev = version;
    sha256 = "sha256-1MG5YGNdylNSPwNbe1buNrcPOtW1MgiDtb0x6J4bDIY=";
  };

  nativeBuildInputs = [
    meson
    ninja
    pkg-config
    qt6.wrapQtAppsHook
  ];

  buildInputs = [
    qt6.qtbase
    kdePackages.kwayland
    libxkbcommon
  ];

  meta = with lib; {
    description = "Virtual keyboard input tool for KDE Wayland";
    homepage = "https://github.com/Sporif/KWtype";
    license = licenses.mit;
    maintainers = [ ];
    platforms = platforms.linux;
    mainProgram = "kwtype";
  };
}
