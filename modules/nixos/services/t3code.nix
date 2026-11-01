{
  config,
  lib,
  pkgs,
  utils,
  ...
}:

let
  cfg = config.dog.services.t3code;
  home = lib.attrByPath [ cfg.user "home" ] "/var/empty" config.users.users;
in
{
  options.dog.services.t3code = {
    enable = lib.mkEnableOption "the persistent T3 Code backend";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.llm-agents.t3code.override { providerPackages = [ ]; };
      defaultText = lib.literalExpression "pkgs.llm-agents.t3code.override { providerPackages = [ ]; }";
      description = "T3 Code package. Agents are installed separately and found through the user's login-shell PATH.";
    };

    user = lib.mkOption {
      type = lib.types.str;
      description = "Existing user that runs T3. The module does not create a user or install agents.";
    };

    host = lib.mkOption {
      type = lib.types.str;
      default = "127.0.0.1";
      description = "Listen address. Firewall rules and reverse proxies are managed by the deployment, not this module.";
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 32862;
      description = "HTTP/WebSocket listen port.";
    };

    workingDirectory = lib.mkOption {
      type = lib.types.str;
      default = home;
      defaultText = lib.literalExpression "config.users.users.<user>.home";
      description = "Working directory for the backend.";
    };

    stateDirectory = lib.mkOption {
      type = lib.types.str;
      default = "${home}/.t3";
      defaultText = lib.literalExpression ''"<user home>/.t3"'';
      description = "Persistent T3 state directory, writable by the service user. Credentials and pairing are managed by T3, not Nix.";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = builtins.hasAttr cfg.user config.users.users;
        message = "dog.services.t3code.user must name an existing users.users entry.";
      }
      {
        assertion = lib.hasPrefix "/" cfg.workingDirectory && lib.hasPrefix "/" cfg.stateDirectory;
        message = "T3 Code workingDirectory and stateDirectory must be absolute paths.";
      }
    ];

    systemd.services.t3code = {
      description = "T3 Code backend";
      wantedBy = [ "multi-user.target" ];
      wants = [ "network-online.target" ];
      after = [ "network-online.target" ];
      environment = {
        HOME = home;
        T3CODE_HOME = cfg.stateDirectory;
      };
      serviceConfig = {
        Type = "simple";
        User = cfg.user;
        WorkingDirectory = cfg.workingDirectory;
        # Include Home Manager and Nix profile tools, without bundling providers.
        ExecStart = utils.escapeSystemdExecArgs [
          "${pkgs.bash}/bin/bash"
          "-lc"
          "exec ${
            lib.escapeShellArgs [
              "${cfg.package}/bin/t3"
              "serve"
              "--host"
              cfg.host
              "--port"
              (toString cfg.port)
              "--no-browser"
            ]
          }"
        ];
        Restart = "on-failure";
        RestartSec = "5s";
        UMask = "0077";
      };
    };
  };
}
