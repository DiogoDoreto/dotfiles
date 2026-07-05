{ ... }:

let
  vars = import ../_variables.nix;
  p = builtins.mapAttrs (_: toString) vars.ports;
in

{
  services.cloudflared = {
    enable = true;
    tunnels."ed93f0fa-f178-4219-8b08-070240906058" = {
      credentialsFile = "/etc/secrets/cloudflared/doreto-tunnel.json";
      ingress = {
        "*.doreto.com.br" = "http://127.0.0.1:${p.publicCaddy}";
      };
      default = "http_status:404";
    };
  };
}
