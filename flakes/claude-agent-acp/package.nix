{ lib, buildNpmPackage, fetchFromGitHub }:

buildNpmPackage rec {
  pname = "claude-agent-acp";
  version = "0.20.2";

  src = fetchFromGitHub {
    owner = "zed-industries";
    repo = "claude-agent-acp";
    rev = "v${version}";
    sha256 = "1qc52h3vk0nrdnyjsqv6daxbw5z4dw8ysk63m80lv4g6zsb2zsnh";
  };

  npmDepsHash = "sha256-DjkQUcx/osL+ZBJF7hOQT3qWlaKkB91VelJxReKbOO4=";

  npmPackFlags = [ "--ignore-scripts" ];

  meta = with lib; {
    description = "Claude Agent ACP server";
    homepage = "https://github.com/zed-industries/claude-agent-acp";
    license = licenses.asl20;
    mainProgram = "claude-agent-acp";
    platforms = platforms.all;
  };
}
