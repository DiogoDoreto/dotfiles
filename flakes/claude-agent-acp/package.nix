{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
}:

buildNpmPackage rec {
  pname = "claude-agent-acp";
  version = "0.30.0";

  src = fetchFromGitHub {
    owner = "agentclientprotocol";
    repo = "claude-agent-acp";
    rev = "v${version}";
    hash = "sha256-Fb5P9LUPIeVYZ7LDVreHZCtuXUtHNdZjqC4gRVGVg50=";
  };

  npmDepsHash = "sha256-lF1me4oRLCol2Nx14BWjognjmzK6GzHZJajS6s4tJSQ=";

  npmPackFlags = [ "--ignore-scripts" ];

  meta = with lib; {
    description = "Claude Agent ACP server";
    homepage = "https://github.com/agentclientprotocol/claude-agent-acp";
    license = licenses.asl20;
    mainProgram = "claude-agent-acp";
    platforms = platforms.all;
  };
}
