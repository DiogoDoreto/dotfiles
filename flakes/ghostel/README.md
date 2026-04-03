# ghostel flake

Nix flake that builds [ghostel](https://github.com/dakra/ghostel) — an Emacs
terminal emulator powered by [libghostty-vt](https://ghostty.org/), the same VT
engine that drives the Ghostty terminal.

This flake is the **single source of truth** for the ghostel version used
in this dotfiles setup. Updating the `rev`/`hash` in `flake.nix` is the only
step needed to track a new ghostel release.

## Why a separate flake?

ghostel requires a native Zig dynamic module (`ghostel-module.so`) that must be
compiled against both the Emacs C headers and a static build of `libghostty-vt`.
The build chain is non-trivial and cannot be expressed inside
`modules/home-manager/programs/emacs.nix` without pulling its heavy dependencies
into every host's evaluation. Isolating it here keeps the shared modules clean
and lets hosts opt in explicitly.

## File structure

```
flakes/ghostel/
├── flake.nix            Main flake: fetches ghostel, wires derivations, exposes outputs
├── flake.lock           Locked nixpkgs + flake-utils pins
├── ghostty-deps.nix     Zig dependency linkFarm for the vendored ghostty commit
│                        (analogous to nixpkgs' pkgs/by-name/gh/ghostty/deps.nix)
├── libghostty-vt.nix    Builds libghostty-vt.a + libsimdutf.a + libhighway.a
├── ghostel-module.nix   Builds ghostel-module.so (the Emacs dynamic module)
├── Makefile             Helper targets for updating and building
└── README.md            This file
```

## Flake outputs

| Output | Description |
|---|---|
| `packages.${system}.ghostel-package` | Combined dir with elisp + `.so` — consumed by `emacs.nix` |
| `packages.${system}.ghostel-module` | Just the compiled `ghostel-module.so` |
| `packages.${system}.libghostty-vt` | Just the static libraries + headers |
| `packages.${system}.default` | Alias for `ghostel-package` |
| `overlays.${system}.default` | Injects `ghostel-package` into `pkgs` |

## How it integrates with Doom Emacs

The `dog.programs.emacs.ghostel.enable` option in
`modules/home-manager/programs/emacs.nix` (when `true`) symlinks
`pkgs.ghostel-package` into straight.el's repos directory:

```
~/.local/share/doomemacs/straight/repos/ghostel  →  /nix/store/…-ghostel-package/
```

`packages.el` then declares the package with `:type nil` so straight skips all
VCS operations and just uses the directory as-is.  `ghostel.el`'s own module
loading code finds `ghostel-module.so` in the same directory via
`(file-name-directory load-file-name)` at require time — no extra Elisp
configuration needed.

## Host integration

Hosts that want ghostel add this to their `flake.nix`:

```nix
# inputs
my-ghostel = {
  url = "../../flakes/ghostel";
  inputs.nixpkgs.follows = "nixpkgs";
};

# overlays list
inputs.my-ghostel.overlays.${system}.default

# home.nix
dog.programs.emacs.ghostel.enable = true;
```

## Updating ghostel

1. Find the new commit hash on [dakra/ghostel](https://github.com/dakra/ghostel):
   ```sh
   make show-ghostel-rev
   ```
2. Edit `flake.nix`: update `rev` and clear `hash` (set to `lib.fakeHash` or `""`).
3. Run `nix build .#ghostel-package` — Nix will report the correct hash in the error.
4. Update `hash` with the reported value.
5. Run `make lock` to refresh `flake.lock`.
6. If the vendored ghostty commit also changed, see **Updating the ghostty dependency** below.

## Updating the ghostty dependency

ghostel vendors a specific ghostty commit as a git submodule
(`vendor/ghostty`). When that commit changes (check `.gitmodules` or
`vendor/ghostty` in the ghostel repo), `libghostty-vt.nix` and
`ghostty-deps.nix` must be updated together.

1. Find the new ghostty submodule commit:
   ```sh
   make show-ghostty-rev GHOSTEL_REV=<new-ghostel-rev>
   ```
2. Fetch the new ghostty source hash:
   ```sh
   make ghostty-src-hash GHOSTTY_REV=<new-ghostty-rev>
   ```
3. Update `rev` and `hash` in `libghostty-vt.nix`.
4. Inspect the new `build.zig.zon` for changed external deps:
   ```sh
   make show-ghostty-zon GHOSTTY_REV=<new-ghostty-rev>
   ```
5. Manually update `ghostty-deps.nix` for any changed entries
   (the Makefile's `update-ghostty-deps` target assists with hash fetching):
   ```sh
   make update-ghostty-deps GHOSTTY_REV=<new-ghostty-rev>
   ```

> **Note:** `zon2nix` currently cannot parse the `.zon` format used by this
> version of ghostty (it uses `.fingerprint` and the new multihash format).
> The deps file is therefore maintained manually, cross-referenced against
> nixpkgs' `pkgs/by-name/gh/ghostty/deps.nix` which covers most entries.
