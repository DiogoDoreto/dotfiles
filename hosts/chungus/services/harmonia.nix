{ ... }:

let
  vars = import ../_variables.nix;
in
{
  # Provision the key before deployment; systemd loads it as a private credential.
  # See docs/nix-cache.md. No signing key material belongs in the Nix store.
  services.harmonia.cache = {
    enable = true;
    signKeyPaths = [ "/etc/secrets/harmonia/cache.secret" ];
    settings = {
      bind = "0.0.0.0:${toString vars.ports.harmonia}";
      # Prefer the LAN cache to cache.nixos.org (priority 40).
      priority = 30;
    };
  };

  # Only Mini's Caddy may reach the HTTP backend. Do not open this port globally.
  networking.firewall.extraCommands = ''
    iptables -A nixos-fw -p tcp -s 192.168.0.2 --dport ${toString vars.ports.harmonia} -j nixos-fw-accept
  '';
}
