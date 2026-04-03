{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
}:

buildNpmPackage rec {
  pname = "pi-acp";
  version = "0.0.24";

  src = fetchFromGitHub {
    owner = "svkozak";
    repo = "pi-acp";
    rev = "v${version}";
    sha256 = "109qb7phz8rlmcwvwgr6n1fjz2p7lpb13zqix2d40blq4fbhsz7k";
  };

  npmDepsHash = "sha256-GNn4XTeFDrmWQeuLSjRlz4nwP5T76HCwBLnIDFPcJkg=";

  npmPackFlags = [ "--ignore-scripts" ];

  meta = with lib; {
    description = "ACP adapter bridging the pi coding agent with ACP-compatible clients";
    homepage = "https://github.com/svkozak/pi-acp";
    license = licenses.mit;
    mainProgram = "pi-acp";
    platforms = platforms.all;
  };
}
