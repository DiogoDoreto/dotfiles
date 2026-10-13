{
  lib,
  buildPythonPackage,
  fetchPypi,
  autoPatchelfHook,
  stdenv,
  cffi,
  requests,
  srt,
  tqdm,
  websockets,
}:

buildPythonPackage rec {
  pname = "vosk";
  version = "0.3.45";
  format = "wheel";

  src = fetchPypi {
    inherit pname version;
    format = "wheel";
    dist = "py3";
    python = "py3";
    abi = "none";
    platform = "manylinux_2_12_x86_64.manylinux2010_x86_64";
    hash = "sha256-JeAlCTxDmdcnj1Q1aO2MxUYKw6S/SMI2c6zh4l0mYZ8=";
  };

  nativeBuildInputs = [ autoPatchelfHook ];
  buildInputs = [ stdenv.cc.cc.lib ];
  dependencies = [
    cffi
    requests
    srt
    tqdm
    websockets
  ];
  pythonImportsCheck = [ "vosk" ];

  meta = {
    description = "Vosk wheel for the throwaway lapdog recognition comparison";
    homepage = "https://alphacephei.com/vosk/";
    license = lib.licenses.asl20;
    platforms = [ "x86_64-linux" ];
  };
}
