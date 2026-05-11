{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
}:

buildNpmPackage rec {
  pname = "claude-agent-acp";
  version = "0.33.1";

  src = fetchFromGitHub {
    owner = "agentclientprotocol";
    repo = "claude-agent-acp";
    rev = "v${version}";
    hash = "sha256-FwcIJf/tfH6prDFKtOo7X1mTocibf4Ne6JHOS9ITG8U=";
  };

  npmDepsHash = "sha256-y795LyNjSJjTpIqtA5bC/AgeFLghM0yU5xQRD3m+Ajs=";

  npmPackFlags = [ "--ignore-scripts" ];

  meta = with lib; {
    description = "Claude Agent ACP server";
    homepage = "https://github.com/agentclientprotocol/claude-agent-acp";
    license = licenses.asl20;
    mainProgram = "claude-agent-acp";
    platforms = platforms.all;
  };
}
