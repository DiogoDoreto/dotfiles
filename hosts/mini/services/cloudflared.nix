{ ... }:

let
  vars = import ../_variables.nix;
  p = builtins.mapAttrs (_: toString) vars.ports;
in

{
  services.cloudflared = {
    enable = true;
    # nix run nixpkgs#cloudflared -- tunnel create doreto.com.br
    # cp .cloudflared/a31719a0-f44c-4471-be04-10a689f6bcd5.json /etc/secrets/cloudflared/doreto-tunnel.json
    tunnels."a31719a0-f44c-4471-be04-10a689f6bcd5" = {
      credentialsFile = "/etc/secrets/cloudflared/doreto-tunnel.json";
      ingress = {
        "*.doreto.com.br" = "http://127.0.0.1:${p.publicCaddy}";
      };
      default = "http_status:404";
    };
  };
}
