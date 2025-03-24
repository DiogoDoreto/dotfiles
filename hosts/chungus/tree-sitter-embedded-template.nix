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
  pname = "tree-sitter-embedded-template";
  version = "0.23.2";

  src = fetchFromGitHub {
    owner = "tree-sitter";
    repo = pname;
    rev = "v${version}";
    hash = "sha256-C2Lo3tT2363O++ycXiR6x0y+jy2zlmhcKp7t1LhvCe8=";
  };

  cargoDeps = rustPlatform.fetchCargoVendor {
    inherit src;
    name = "${pname}-${version}";
    hash = "sha256-DscTKXKukh3RsqtKjplyzrxY977zUgpFpeXtFOLJEXA=";
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
