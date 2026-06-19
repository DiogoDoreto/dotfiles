{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.dog.programs.playwright-cli;

  playwright-cli = pkgs.writeShellScriptBin "playwright-cli" ''
    export PLAYWRIGHT_MCP_EXECUTABLE_PATH=${getExe cfg.browser}
    exec ${getExe' pkgs.nodejs "npx"} @playwright/cli "$@"
  '';

  playwrightCliRepo = pkgs.fetchFromGitHub {
    owner = "microsoft";
    repo = "playwright-cli";
    rev = "3a1bafc8b4e973c72d0364eb5b427d1ce0aa8317";
    hash = "sha256-hHK/GR5Drlt+e0L9kyNmn+ht1PCrVH6WrVbxGB1Wsxg=";
  };
in
{
  options.dog.programs.playwright-cli = {
    enable = mkEnableOption "playwright-cli";

    browser = mkOption {
      type = types.package;
      default = pkgs.ungoogled-chromium;
      defaultText = literalExpression "pkgs.ungoogled-chromium";
      description = "Browser package whose executable is hardcoded as PLAYWRIGHT_MCP_EXECUTABLE_PATH.";
    };

    installClaudeSkill = mkEnableOption "symlinking the upstream playwright-cli skill into ~/.claude/skills/";
    installAgentsSkill = mkEnableOption "symlinking the upstream playwright-cli skill into ~/.agents/skills/";
  };

  config = mkIf cfg.enable {
    home.packages = [ playwright-cli ];

    home.file.".claude/skills/playwright-cli" = mkIf cfg.installClaudeSkill {
      source = "${playwrightCliRepo}/skills/playwright-cli";
    };

    home.file.".agents/skills/playwright-cli" = mkIf cfg.installAgentsSkill {
      source = "${playwrightCliRepo}/skills/playwright-cli";
    };
  };
}
