{
  description = "Local Vosk voice commands with system tray control";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/e158d9ed9b51c98974c5e66e1ba1c9e0255fecaa";

  outputs =
    { nixpkgs, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      voiceCommands = pkgs.callPackage ./package.nix { };
    in
    {
      packages.${system} = {
        default = voiceCommands;
        voice-commands = voiceCommands;
        listener = voiceCommands.listener;
        model = voiceCommands.model;
      };
    };
}
