{
  config,
  lib,
  ...
}:

let
  cfg = config.dog.services.journald-to-victorialogs;
  ingestUri = "/insert/jsonline?_stream_fields=_HOSTNAME,_TRANSPORT&_msg_field=MESSAGE&_time_field=date";
in

{
  options.dog.services.journald-to-victorialogs = {
    enable = lib.mkEnableOption "journald shipping to VictoriaLogs with Fluent Bit";

    host = lib.mkOption {
      type = lib.types.str;
      default = "logs.local.doreto.com.br";
      description = "VictoriaLogs HTTP host for Fluent Bit to send logs to.";
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 443;
      description = "VictoriaLogs HTTP port for Fluent Bit to send logs to.";
    };

    tls = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Whether Fluent Bit should use TLS for the VictoriaLogs HTTP output.";
    };
  };

  config = lib.mkIf cfg.enable {
    services.fluent-bit = {
      enable = true;
      settings = {
        service = {
          flush = 1;
          log_level = "info";
          "storage.path" = "/var/lib/fluent-bit/storage";
          "storage.type" = "filesystem";
          "storage.inherit" = "on";
          "storage.sync" = "normal";
          "storage.max_chunks_up" = 128;
        };

        pipeline = {
          inputs = [
            {
              name = "systemd";
              tag = "journal.*";
              db = "/var/lib/fluent-bit/journald.db";
              read_from_tail = true;
            }
          ];

          outputs = [
            (
              {
                name = "http";
                match = "journal.*";
                host = cfg.host;
                port = cfg.port;
                uri = ingestUri;
                format = "json_lines";
                json_date_key = "date";
                json_date_format = "iso8601";
                retry_limit = false;
                "storage.total_limit_size" = "100M";
              }
              // lib.optionalAttrs cfg.tls {
                tls = "on";
                "tls.verify" = "on";
              }
            )
          ];
        };
      };
    };

    systemd.services.fluent-bit = {
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      serviceConfig.StateDirectory = "fluent-bit";
    };
  };
}
