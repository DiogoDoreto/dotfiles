{ lib, buildGoModule, fetchFromGitHub }:

buildGoModule rec {
  pname = "jenkins-cli";
  version = "0-unstable-2026-03-02";

  src = fetchFromGitHub {
    owner = "avivsinai";
    repo = "jenkins-cli";
    rev = "8e50e6c30ae6c983d515383a64dce638146972cf";
    hash = "sha256-pyBl3g5bOD4hdTa5CyX4ogTGYj/Lv4VgNzz5armnYoY=";
  };

  vendorHash = "sha256-yL86bUYqKPa0LctHOQ02K3TadHB8Z6G00INj1wyDy0Y=";

  subPackages = [ "cmd/jk" ];

  ldflags = [
    "-s" "-w"
    "-X github.com/avivsinai/jenkins-cli/internal/build.versionFromLdflags=${version}"
    "-X github.com/avivsinai/jenkins-cli/internal/build.commitFromLdflags=${src.rev}"
  ];

  meta = with lib; {
    description = "A CLI tool for Jenkins";
    homepage = "https://github.com/avivsinai/jenkins-cli";
    license = licenses.mit;
    mainProgram = "jk";
  };
}
