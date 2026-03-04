# Full derivation from https://github.com/NixOS/nixpkgs/pull/493363
# TODO: remove once lager 0.1.2 lands on nixos-unstable
{
  lib,
  stdenv,
  fetchFromGitHub,
  fetchpatch,
  cmake,
  boost,
  cereal,
  immer,
  zug,
  catch2,
  qt5,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "lager";
  version = "0.1.2";

  src = fetchFromGitHub {
    owner = "arximboldi";
    repo = "lager";
    tag = "v${finalAttrs.version}";
    hash = "sha256-ssGBQu8ba798MSTtJeCBE3WQ7AFfvSGLhZ7WBYHEgfw=";
  };

  patches = lib.optionals finalAttrs.finalPackage.doCheck [
    # https://github.com/arximboldi/lager/pull/233
    (fetchpatch {
      name = "Stop-using-Boost-system.patch";
      url = "https://github.com/arximboldi/lager/commit/0eb1d3d3a6057723c5b57b3e0ee3e41924ff419a.patch";
      hash = "sha256-peGpuyuCznCDqYo+9zk1FytLV+a6Um8fvjLmrm7Y2CI=";
    })
  ];

  strictDeps = true;

  nativeBuildInputs = [ cmake ];

  buildInputs = [
    boost
    cereal
    immer
    zug
  ];

  checkInputs = [
    catch2
    qt5.qtdeclarative
  ];

  cmakeFlags = [
    (lib.cmakeBool "lager_BUILD_DEBUGGER_EXAMPLES" false)
    (lib.cmakeBool "lager_BUILD_DOCS" false)
    (lib.cmakeBool "lager_BUILD_EXAMPLES" false)
    (lib.cmakeBool "lager_BUILD_TESTS" finalAttrs.finalPackage.doCheck)
  ];

  # remove BUILD file to avoid conflicts with the build directory
  preConfigure = ''
    rm BUILD
  '';

  doCheck = true;

  dontWrapQtApps = true;

  meta = {
    changelog = "https://github.com/arximboldi/lager/releases/tag/${finalAttrs.src.tag}";
    description = "C++ library for value-oriented design using the unidirectional data-flow architecture — Redux for C++";
    homepage = "https://sinusoid.es/lager/";
    license = lib.licenses.mit;
    maintainers = [ ];
  };
})
