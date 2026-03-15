{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
}:

buildNpmPackage rec {
  pname = "claude-agent-acp";
  version = "0.21.0";

  src = fetchFromGitHub {
    owner = "zed-industries";
    repo = "claude-agent-acp";
    rev = "v${version}";
    sha256 = "1wavd5ak0x4p9ps84b5ls56864bkn76qiiwpwnbdip98xcg9pkp9";
  };

  npmDepsHash = "sha256-UtiIcjgNCYMFrRpO5AlUbOyutJ3ipwIbcpMi2BqawEk=";

  npmPackFlags = [ "--ignore-scripts" ];

  meta = with lib; {
    description = "Claude Agent ACP server";
    homepage = "https://github.com/zed-industries/claude-agent-acp";
    license = licenses.asl20;
    mainProgram = "claude-agent-acp";
    platforms = platforms.all;
  };
}
