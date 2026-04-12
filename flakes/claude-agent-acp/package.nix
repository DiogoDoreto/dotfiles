{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
}:

buildNpmPackage rec {
  pname = "claude-agent-acp";
  version = "0.26.0";

  src = fetchFromGitHub {
    owner = "agentclientprotocol";
    repo = "claude-agent-acp";
    rev = "v${version}";
    hash = "sha256-2G8gjMCnk3W1I2+4sNsumL15ts9bLXAOMguCmwnzWSA=";
  };

  npmDepsHash = "sha256-msm4L8Yi7ma2eHOYXbZx+Qtrx4TzK7FV3HpVzRhQ19o=";

  npmPackFlags = [ "--ignore-scripts" ];

  meta = with lib; {
    description = "Claude Agent ACP server";
    homepage = "https://github.com/agentclientprotocol/claude-agent-acp";
    license = licenses.asl20;
    mainProgram = "claude-agent-acp";
    platforms = platforms.all;
  };
}
