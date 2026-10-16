# Agent VM

The Agent VM runs coding agents and their control surfaces in an isolated
MicroVM. It optionally runs `opencode serve` and the Paseo daemon, and exposes
their interfaces through host-local proxies. Its storage consists of VM-owned
persistent state and explicitly configured host-directory shares.

## Organization

There are two layers:

- **Agent VM definition** — this directory contains the reusable configuration
  for this particular workload.
- **Agent VM deployment** — `hosts/<host>/vms/agent-vm/` contains a host's choices
  about shares, access, operation, and guest customization.

```text
modules/nixos/vms/agent-vm/
  options.nix       Configuration interface: dog.services.agent-vm
  host.nix          Host networking, state, credentials, proxies, and VM registration
  guest.nix         Guest OS, mounts, users, Home Manager, and agent services
  README.md         Architecture, operations, and migration

hosts/{lapdog,mini}/vms/agent-vm/
  settings.nix      Values that both sides must agree on
  host.nix          Complete host-side entry point
  guest.nix         Complete guest-side entry point
```

Start at a host's `flake.nix`: the physical host imports
`./vms/agent-vm/host.nix`, and `nixosConfigurations.agent-vm` imports
`./vms/agent-vm/guest.nix`. Each deployment entry point imports the corresponding
MicroVM module and reusable Agent VM module. Importing an entry point enables
that side; there are no separate host/guest enable flags and no global import
through `modules/nixos/default.nix`.

Both entry points import their local `settings.nix` attribute set. Use that file
for agreement across the boundary: network identities, persistent backing
paths, filesystem shares, and capabilities such as Paseo that need both a guest
daemon and a host proxy. Defaults in `options.nix` already provide the common
network identities and state paths. Host-only policy belongs in the deployment's
`host.nix`; guest-only policy belongs in its `guest.nix`.

VM deployment settings are not global `specialArgs`. The existing flake-level
common module lists still supply package overlays, Home Manager and its shared
modules, and Nix input configuration to both systems. The deployment entry points
expect those common ingredients, plus `inputs` and `self` in `specialArgs`.

## Current deployments

| Choice | lapdog | mini (NixOS host name: dogdot) |
| --- | --- | --- |
| Starts at host boot | No | Yes |
| Additional host shares | `/home/dog/projects` → `/home/agent/projects`, read-write | None |
| Agent working directory | `/home/agent/projects` | `/home/agent` |
| OpenCode | Disabled | Enabled |
| Paseo | Enabled | Enabled |
| Public frontends | None | Caddy with Authentik for OpenCode and Paseo |
| Guest Tailscale | Disabled | Enabled |
| Guest Nix GC | No automatic policy | Daily space check; collect below 15 GiB free |

Lapdog forwards DNS queries for `local.doreto.com.br` and `home` to
`192.168.0.2`. Its host dnsmasq is dedicated to VM DNS and binds the VM bridge.

Mini supplies LAN and Tailscale DNS through its own dnsmasq services in
`hosts/mini/networking.nix`. When the VM host entry point is imported, that file
includes the configured VM host address in the LAN listener. The deployment
disables bridge-only and strict-interface binding, adds no extra upstream DNS
servers, and preserves this existing setup.

Mini's guest GC timer runs at 03:00 Europe/Madrid without replaying missed runs.
Its `ExecCondition` measures available space on `/nix/.rw-store`, not the host
read-only store or guest tmpfs. Collection runs only below 15 GiB free and leaves
rooted system and Home Manager paths intact. This policy is deployment-specific
and lives in `hosts/mini/vms/agent-vm/guest.nix`.

## Operating it

After deploying the physical host configuration:

```sh
start-agent-vm
stop-agent-vm
agent-vm-ssh
agent-vm-public-key
agent-vm-logs
```

`start-agent-vm` initializes state, starts `microvm@agent-vm.service`, and prints
the URLs of enabled services (OpenCode: `http://127.0.0.1:32860`, Paseo:
`http://127.0.0.1:32861`). `autostart = true` starts the VM at host
boot instead, as configured on mini.

`agent-vm-public-key` prints the outbound SSH public key for manually adding to
GitHub, Forgejo, or other Git remotes. The private key stays in VM-owned state;
the host user's SSH directory and agent socket are not shared.

`agent-vm-ssh` connects as `agent` with a dedicated host-to-guest maintenance key.
It disables host-key checking for that connection. Other SSH clients can retain
the guest's host keys: those keys persist under `/var/lib/ssh` inside the guest.

Useful host logs:

```sh
agent-vm-logs
journalctl -u dnsmasq -g 'query'
journalctl -k -g '\[agent-vm\]'
```

On mini, query the separately managed `dnsmasq-lan` or `dnsmasq-tailscale` unit
instead of the disabled standard dnsmasq service.

## Persistence and credentials

The host's `/var/lib/agent-vm/` contains:

| Host path, relative to stateDir | Guest use |
| --- | --- |
| `home/` | Persistent `/home/agent` |
| `var/lib/` | Persistent service state, including SSH host keys and Tailscale identity |
| `var/log/` | Persistent guest logs and journal |
| `ssh/host/host_to_vm` | Host maintenance key; not shared into the guest |
| `ssh/guest/host_to_vm.pub` | Guest authorized key for maintenance access |
| `ssh/guest/vm_outbound` | Outbound Git SSH key, shared read-only |
| `ssh/host/vm_outbound.pub` | Host-readable copy of the outbound public key |
| `rw-store.img` | Persistent 64 GiB sparse ext4 image for the guest's writable Nix store |

`agent-vm-init.service` creates backing directories and missing keys before both
the VM and its virtiofs daemons start. The state root is `root:kvm`, mode `0775`,
so the `microvm:kvm` runner can create the store image. SSH directories have
narrower permissions. Moving existing state preserves keys rather than generating
new ones.

The host store is read-only at `/nix/.ro-store`. The guest's writable store overlay
uses `/nix/.rw-store`, backed by `rw-store.img`, instead of consuming the tmpfs
root. The image is sparse: `ls -lh` reports capacity, while `du -h` reports actual
host storage consumption.

MicroVM separately registers its generated runner under
`/var/lib/microvms/agent-vm/`. That directory is not the guest's persistent data
directory. `install-microvm-agent-vm.service` installs the runner from the host
flake's `nixosConfigurations.agent-vm` output.

## Networking and control surfaces

Defaults shared by both deployments:

```text
Bridge:        agent-vm0
TAP:           agent-tap0
Host address:  10.0.101.1/24
Guest address: 10.0.101.2/24
Guest MAC:     02:00:00:00:01:02
Network:       10.0.101.0/24
```

The host enables forwarding and NAT, serves guest DNS on the bridge address, and
logs DNS queries and new outbound TCP/UDP connections. This logging is audit
visibility, not an outbound allowlist; the guest can reach the LAN and internet.

OpenCode is optional (`opencode.enable`, default `false`) and enabled only on mini.
When enabled, its package, guest service, firewall port, and host proxy are included.
It listens on guest `10.0.101.2:32859`. A host-local socat proxy forwards
`127.0.0.1:32860` to it. On mini, Caddy exposes
`https://opencode.local.doreto.com.br` with Authentik forward authentication.

Both deployments also run `paseo-server --no-relay` with its bundled web interface
on guest port `6767`. The host proxy listens on `127.0.0.1:32861` (on lapdog, open
`http://127.0.0.1:32861`). On mini, Caddy exposes
`https://paseo.local.doreto.com.br` with the same authentication. Paseo accepts that
hostname through DNS-rebinding protection and trusts the host bridge address for
forwarded requests. Its state lives in the persistent guest home at
`/home/agent/.paseo`.

## Guest environment and isolation

The guest has 4 vCPUs and 16 GiB RAM by default. Its user is `agent`, UID/GID 1000,
matching host project ownership while keeping the VM identity visible in prompts
and logs. The user has passwordless sudo inside the guest. The isolation boundary
is the VM plus its network policy, credentials, and explicit shares.

Neither deployment implicitly mounts the host user's home, SSH agent, OpenCode
state, Claude state, or secrets. Lapdog's broad read-write projects share is an
intentional existing choice, including access to this dotfiles checkout. Mini
uses only VM-owned persistent storage.

The guest trusts `hosts/mini/home-caddy.crt` so Git, curl, and Forgejo CLI can use
local HTTPS services. Home Manager configures Git, CLI tools, agent skills,
Playwright CLI, and Forgejo CLI; the system includes SSH, GitHub CLI, and common
maintenance tools. OpenCode uses a login shell, while Paseo's explicit PATH
includes the guest's Nix and Home Manager profiles.

The dotfiles checkout is expected at `/home/agent/projects/dotfiles`. Agent skills
are linked from that checkout into `~/.agents/skills`. Edits to linked skills
are immediate; adding a skill requires tracking it in Git and redeploying the
guest so Home Manager adds its link.

## Using this structure for another VM

This is a reference implementation, not a general-purpose VM framework.

1. Create `hosts/<host>/vms/<name>/host.nix`, `guest.nix`, and, when there are
   shared values, `settings.nix`.
2. Let `host.nix` import `inputs.microvm.nixosModules.host` and define host-side
   registration, connectivity, and backing storage. Set `microvm.vms.<name>.flake
   = self` and choose its autostart policy.
3. Let `guest.nix` import `inputs.microvm.nixosModules.microvm` and define that
   workload's guest OS and MicroVM hardware. Keep physical-host hardware modules
   out of the guest.
4. Put host/guest agreement in `settings.nix`, and import it locally from both
   entry points. Use unique bridge/TAP names, addresses, MAC, host ports, and
   backing directories for VMs that coexist on one host.
5. Import the host entry point in the physical host's module list. Expose a guest
   `nixosConfigurations.<name>` using the same name as the MicroVM registration.
   Supply the guest's required package overlays and module dependencies explicitly.
6. Check the host flake and build both systems before deploying.

For another **Agent VM deployment**, reuse this directory's host and guest
modules, with deployment settings matching the new guest output name. For a
different workload, follow the organization and selectively adapt infrastructure.
Agent users, outbound Git keys, project sharing, OpenCode, Paseo, and the agent
Home Manager setup are workload choices, not requirements for every VM. The helper
command names currently assume one Agent VM per host.

## Migrating from opencode-agent-vm

Perform this once on **each physical host that has existing VM state**, from a
host session outside the Agent VM. Stopping the guest ends agent sessions. Build
and validate the new configuration first; do the following immediately before
deploying it. The commands assume the old deployment's default names and paths.

### 1. Stop the old VM and retire its bridge

```sh
sudo systemctl stop microvm@opencode-agent-vm.service
sudo systemctl stop microvm-virtiofsd@opencode-agent-vm.service \
  microvm-tap-interfaces@opencode-agent-vm.service \
  opencode-agent-vm-init.service
sudo systemctl mask --runtime microvm@opencode-agent-vm.service
sudo systemctl stop network-addresses-opencode-vm0.service opencode-vm0-netdev.service
```

The runtime mask prevents accidentally starting the old runner after moving its
data. Stopping the old bridge avoids having both bridge names claim
`10.0.101.1` during a live switch. This does not stop the physical LAN interface.

### 2. Move the persistent state intact

Run these commands in a shell with `set -e` so a failed precondition stops the
sequence:

```sh
set -e
sudo test -d /var/lib/opencode-agent-vm
sudo test ! -e /var/lib/agent-vm
sudo mv -T /var/lib/opencode-agent-vm /var/lib/agent-vm
```

This same-filesystem rename preserves ownership, permissions, SSH keys, guest
home, service state, journal, and the sparse store image. If the target already
exists, investigate it rather than merging the directories or overwriting state.

Do **not** move the old `/var/lib/microvms/opencode-agent-vm/` registration to the
new name: its generated runner refers to old paths and interfaces. Leave it
stopped; the new registration is generated from the new configuration.

### 3. Deploy the new host generation and start the new registration

Deploy manually using the normal host workflow. After the new generation is
active, run on that physical host:

```sh
sudo systemctl start install-microvm-agent-vm.service
start-agent-vm
agent-vm-public-key
agent-vm-ssh hostname
```

Mini may already have autostarted the new VM. Starting it again is harmless.
The hostname should be `agent-vm`; check that the public key matches the previous
one and that the guest projects, agent state, and web interfaces are present.
No Git remote key replacement is needed when the state was moved intact.
Tailscale's persisted device identity is retained; its displayed name can be
checked separately after the hostname change.

The new namespace is `dog.services.agent-vm`, and the guest flake output is
`nixosConfigurations.agent-vm`. Commands use `agent-vm` in place of
`opencode-agent-vm`. The guest SSH-material mount moves to `/run/agent-vm/ssh`;
the guest setup service updates the persistent user's SSH symlinks automatically.
OpenCode/Paseo ports and public URLs do not change.

For rollback, stop the new VM and its virtiofs daemons before moving
`/var/lib/agent-vm` back to the old path. Restore the old host generation, remove
the old VM's runtime mask, and start the old VM. A generation rollback alone does
not undo the state-directory rename.

## Validation

From the repository root:

```sh
make format-nix
nix flake check ./hosts/lapdog/
nix flake check ./hosts/mini/
nix build --no-link ./hosts/lapdog#nixosConfigurations.lapdog.config.system.build.toplevel
nix build --no-link ./hosts/lapdog#nixosConfigurations.agent-vm.config.system.build.toplevel
nix build --no-link ./hosts/mini#nixosConfigurations.dogdot.config.system.build.toplevel
nix build --no-link ./hosts/mini#nixosConfigurations.agent-vm.config.system.build.toplevel
```

All deployment and migration actions are manual. Agents validate configurations
without activating host generations or moving runtime state.
