# Chungus binary cache

Chungus serves its Nix store through Harmonia. Mini's Caddy exposes it at
`https://nix-cache.local.doreto.com.br`, using the existing local CA. The local
DNS wildcard already resolves this name to Mini on both LAN and Tailscale.

The cache accepts LAN, Tailscale, and loopback clients only. Caddy checks the
direct peer address, not client-controlled forwarded headers. The public
Cloudflare ingress remains separate and does not route to this cache. Chungus's
HTTP backend on port 5000 accepts only Mini (`192.168.0.2`). The cache route goes
directly to Chungus (`192.168.0.3`), not through the Wake-on-LAN proxy.

## Provision the signing key (manually on Chungus)

Before deploying the Harmonia configuration, run:

```sh
sudo install -d -m 0700 /etc/secrets/harmonia
sudo sh -eu -c '
  umask 077
  test ! -e /etc/secrets/harmonia/cache.secret
  test ! -e /etc/secrets/harmonia/cache.pub
  nix-store --generate-binary-cache-key nix-cache.local.doreto.com.br-1 \
    /etc/secrets/harmonia/cache.secret /etc/secrets/harmonia/cache.pub
'
sudo cat /etc/secrets/harmonia/cache.pub
```

Do not regenerate an existing key. Keep the secret backed up securely, outside
Git and the Nix store. The NixOS Harmonia module passes it through systemd
`LoadCredential`; the service does not need access to the original directory.

The supplied public key is already configured as the `dog.nix-cache.publicKey`
default in `modules/nixos/nix-cache-client.nix`:

```text
nix-cache.local.doreto.com.br-1:/37nJvnSImJtzKHzMDI1e2iKk1SisZAEFujENOmB/co=
```

For a future key change, set the `dog.nix-cache.publicKey` default in
`modules/nixos/nix-cache-client.nix` to the printed public key. This distributes
the same key to Mini and Lapdog. Until then, their client configuration emits a
warning and does not add the new substituter. Never disable signature checks or
mark the cache trusted as a workaround.

## Deployment order (manual)

1. Provision the secret on Chungus and configure its public key in the repo.
2. Check the affected host flakes. Build/deploy Chungus manually.
3. Build/deploy Mini to enable its Caddy route and cache client.
4. Build/deploy Lapdog to enable its cache client.

Mini and Lapdog already trust the local Caddy CA. Existing upstream substituters
and trusted keys are preserved. Chungus uses its existing store directly and
does not configure itself as a client of this endpoint.

## Verify after deployment

On Lapdog, check DNS, HTTPS, and cache metadata:

```sh
getent hosts nix-cache.local.doreto.com.br
curl --fail https://nix-cache.local.doreto.com.br/nix-cache-info
nix config show substituters
nix config show trusted-public-keys
```

On Chungus, select a built output that is absent from Lapdog (preferably one
built locally rather than an upstream cache output):

```sh
nix path-info /nix/store/REPLACE-WITH-BUILT-OUTPUT
```

On Lapdog, confirm that this path is absent, then copy it using only the new
cache as the source. Do not delete installed store paths just to create a miss.

```sh
test ! -e /nix/store/REPLACE-WITH-BUILT-OUTPUT
curl --fail https://nix-cache.local.doreto.com.br/REPLACE-WITH-OUTPUT-HASH.narinfo
nix copy --from https://nix-cache.local.doreto.com.br \
  /nix/store/REPLACE-WITH-BUILT-OUTPUT
```

The narinfo should contain a `Sig: nix-cache.local.doreto.com.br-1:...` field.
Leave signature verification enabled for the copy. Verify an unrelated upstream
cache path is still obtainable as well. From Lapdog, direct access to
`http://192.168.0.3:5000/nix-cache-info` should fail, while the HTTPS URL works.

For failures inspect `journalctl -u harmonia -u harmonia.socket` on Chungus and
`journalctl -u caddy` on Mini. Missing signing credentials prevent Harmonia from
starting; an unconfigured public key prevents clients from trusting its outputs.

## Retention

Harmonia serves the existing store, not an archive. Persistent per-host main and
update-branch GC roots are part of stage 2, not yet implemented. Until then,
keep any test output rooted (for example through a retained `nix build
--out-link ...` result) if it must survive garbage collection.
