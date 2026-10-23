{
  imports = [
    ./nix-cache-client.nix
    ./services/journald-to-victorialogs.nix
    ./services/nixos-generation-cleanup.nix
  ];
}
