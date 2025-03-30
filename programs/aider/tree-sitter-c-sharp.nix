{
  lib,
  buildPythonPackage,
  fetchFromGitHub,
  setuptools,
  rustPlatform,
  cargo,
  rustc,
}:

buildPythonPackage rec {
  pname = "tree-sitter-c-sharp";
  version = "0.23.1";

  src = fetchFromGitHub {
    owner = "tree-sitter";
    repo = pname;
    rev = "v${version}";
    hash = "sha256-weH0nyLpvVK/OpgvOjTuJdH2Hm4a1wVshHmhUdFq3XA=";
  };

  cargoDeps = rustPlatform.fetchCargoVendor {
    inherit src;
    name = "${pname}-${version}";
    hash = "sha256-IogdMRj1eHRLtdNFdGNInpEQAAbRpM248GqkY+Mgu10=";
  };

  build-system = [
    cargo
    rustPlatform.cargoSetupHook
    rustc
    setuptools
  ];

  # do not run tests
  doCheck = false;

  # specific to buildPythonPackage, see its reference
  pyproject = true;
}
