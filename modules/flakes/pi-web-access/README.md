# pi-web-access

Pinned Nix package for <https://github.com/nicobailon/pi-web-access>.
The overlay uses the consuming host's nixpkgs. The standalone flake supports
`x86_64-linux` (the Agent VM platform).

The published npm release contains a prebuilt extension bundle. We install it
with a locked runtime dependency tree, omitting development dependencies and
Pi-supplied host modules (`@earendil-works/pi-*` and `typebox`). Pi resolves those
modules from its own runtime. Transitive non-host peers, such as the MCP SDK's
`zod`, remain in the lockfile.

Home Manager links the resulting package into the Agent VM user's
`~/.pi/agent/extensions/pi-web-access`. The package's `pi.extensions` manifest
selects `dist/`; no package installation or build runs when Pi starts.

## Verification

```sh
nix build ./modules/flakes/pi-web-access
nix flake check ./modules/flakes/pi-web-access
```

To smoke-test automatic discovery without a model call, run:

```sh
python3 modules/flakes/pi-web-access/smoke-test.py "$(realpath result)" "$(command -v pi)"
```

The smoke test uses an isolated Pi configuration under `.private/tmp/` and checks
that the extension registers its commands through RPC.

## Updating

1. Select and review an upstream npm release and its runtime dependencies.
2. Update the version and tarball URL/hash in `package.nix`.
3. Refresh `package.json` from the release, keeping only `name`, `version`, `type`,
   `license`, `pi`, and `dependencies`. Do not install Pi's host peer modules.
4. Delete `package-lock.json`, then run `npm install --package-lock-only
   --ignore-scripts --no-audit --no-fund` in this directory.
5. Recalculate `npmDepsHash` (temporarily use `lib.fakeHash`, build, then use the
   reported actual hash), rebuild, and repeat the discovery smoke test with the
   Pi version installed in the Agent VM.

Provider credentials belong in the VM user's writable Pi configuration or
environment, never in this derivation or its source files. Optional external
helpers for video/browser features are not installed by this package.
