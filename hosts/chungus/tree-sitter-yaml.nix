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
  pname = "tree-sitter-yaml";
  version = "0.7.0";

  src = fetchFromGitHub {
    owner = "tree-sitter-grammars";
    repo = pname;
    rev = "v${version}";
    hash = "sha256-23/zcjnQUQt32N2EdQMzWM9srkXfQxlBvOo7FWH6rnw=";
  };

  cargoDeps = rustPlatform.fetchCargoVendor {
    inherit src;
    name = "${pname}-${version}";
    hash = "sha256-Rxjimtp5Lg0x8wgWvyyCepMJipPdc0TplxznrF9COtM=";
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
