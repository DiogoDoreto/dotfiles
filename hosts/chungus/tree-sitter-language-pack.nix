{
  lib,
  buildPythonPackage,
  fetchPypi,
  setuptools,
  wheel,
  cython,
  typing-extensions,
  tree-sitter,
  tree-sitter-c-sharp,
  tree-sitter-embedded-template,
  tree-sitter-yaml,
}:

buildPythonPackage rec {
  pname = "tree_sitter_language_pack";
  version = "0.6.1";

  src = fetchPypi {
    inherit pname version;
    hash = "sha256-pGNfW2ubZCVi2QHk6qJfyClJ1mDIi5R1Pm1GfZY0Ark=";
  };

  dependencies = [
    cython
    typing-extensions
    tree-sitter
    tree-sitter-c-sharp
    tree-sitter-embedded-template
    tree-sitter-yaml
  ];

  # do not run tests
  doCheck = false;

  # specific to buildPythonPackage, see its reference
  pyproject = true;
  build-system = [
    setuptools
    wheel
  ];
}
