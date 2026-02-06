# KWtype Flake

This flake packages [KWtype](https://github.com/Sporif/KWtype), a CLI tool that provides virtual keyboard input on KDE Plasma Wayland.

## About KWtype

KWtype is a command-line tool that uses KWin's privileged Fake Input protocol to simulate keyboard input on KDE Wayland sessions. It's useful for automation scripts, accessibility tools, and other scenarios where you need to programmatically type text.

**Note:** KWtype only works on KDE Plasma Wayland sessions as it relies on the `kde-fake-input` Wayland protocol implemented by KWin.

## Usage

### As a Flake Input

Add this flake as an input in your host's flake.nix:

```nix
{
  inputs = {
    kwtype = {
      url = "path:../../modules/flakes/kwtype";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
}
```

### Add the Overlay

Include the overlay in your host configuration:

```nix
overlays = [
  # ... other overlays
  inputs.kwtype.overlays.default
];
```


### Add the package

In your Home Manager configuration:

```nix
home.packages = [ pkgs.kwtype ];
```

Or in NixOS configuration:

```nix
environment.systemPackages = [ pkgs.kwtype ];
```

## Command-Line Usage

Once installed, use kwtype to type text:

```bash
# Type some text
kwtype "Hello, World!"

# Type with a delay between keys
kwtype --key-delay 100 "Slow typing"

# Hold each key longer
kwtype --key-hold 50 "Long press"

# Combine options
kwtype -d 50 -H 10 "Custom timing"
```

For more options, run:

```bash
kwtype --help
```

## Building Locally

To build kwtype locally:

```bash
nix build .#kwtype
```

To run without installing:

```bash
nix run .#kwtype -- "Type this text"
```

## License

KWtype is licensed under the MIT License. See the [upstream repository](https://github.com/Sporif/KWtype) for details.
