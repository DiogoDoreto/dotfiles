{ lib, buildGoModule, fetchFromGitHub }:

buildGoModule rec {
  pname = "bitbucket-cli";
  version = "0-unstable-2026-03-02";

  src = fetchFromGitHub {
    owner = "avivsinai";
    repo = "bitbucket-cli";
    rev = "1f7faea43c6afdbfbb3dc52ab07898a32f916024";
    hash = "sha256-Dnhi1/7hWXEQv9jcseAxEeWu926JeDUqiQmivJCWxr4=";
  };

  vendorHash = "sha256-5dhMUDFHE97hobgBcI654VR4hDFLyGu2VZgIlW7suEk=";

  subPackages = [ "cmd/bkt" ];

  ldflags = [
    "-s" "-w"
    "-X github.com/avivsinai/bitbucket-cli/internal/build.versionFromLdflags=${version}"
    "-X github.com/avivsinai/bitbucket-cli/internal/build.commitFromLdflags=${src.rev}"
  ];

  meta = with lib; {
    description = "A CLI tool for Bitbucket";
    homepage = "https://github.com/avivsinai/bitbucket-cli";
    license = licenses.mit;
    mainProgram = "bkt";
  };
}
