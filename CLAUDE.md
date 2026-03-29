## Repository Overview

This is a multi-machine NixOS/Home Manager dotfiles repository. It covers four hosts:
- **lapdog** (ThinkPad X1 2-in-1 Gen 10, primary dev machine)
- **chungus** (desktop AI/compute workstation with RTX 4090, 64 GB RAM)
- **mini/dogdot** (always on home server running many services)
- **inspiron7520** (secondary machine, unused).

## Key Commands

Manual flake check before switching:
```sh
nix flake check ./hosts/<hostname>/
```

## Architecture

### Host Structure

Each host lives in `hosts/<hostname>/` and is a self-contained flake:
- `flake.nix` — declares inputs (nixpkgs unstable, home-manager, nixos-hardware, etc.) and system outputs
- `configuration.nix` — NixOS system config
- `hardware.nix` — bootloader, GPU, filesystem mounts
- `home.nix` — Home Manager config for user `dog`
- `services/` — modular service definitions (especially on mini and chungus)

### Shared Modules

`modules/home-manager/` contains reusable Home Manager modules imported by hosts:
- `programs/` — per-app configs (emacs/doom, firefox, i3, polybar, rofi, wezterm, git, etc.)
- `presets/` — grouped presets (`linux.nix`, `x11.nix`)
- `lib/bubblewrap-ai.nix` — helper for running AI apps in bubblewrap sandboxes

`modules/flakes/` contains standalone Nix flakes used as inputs. Use it when asked to create a new derivation for a package.

### Application Configs (`.config/`)

`.config/` is tracked directly (bare repo pattern — `$HOME` is the work tree). Notable:
- `doom/` — Doom Emacs config

### Mini Services

`hosts/mini/services/` contains modular NixOS service definitions for the home server: Caddy (reverse proxy), Forgejo, Nextcloud, FreshRSS, Authentik (SSO/OIDC), Immich, Jellyfin, Homepage dashboard, mail server, and backup scripts.

## Flake Development Environments

`flakes/` contains standalone dev environment flakes. Use with direnv:
```sh
# In a project directory with a flake.nix
echo "use flake /path/to/flakes/<name>" > .envrc
direnv allow
```

## Deployment Notes

- all NixOS deployments are manually done by the user, you may build lapdog and mini hosts locally for verification when necessary
