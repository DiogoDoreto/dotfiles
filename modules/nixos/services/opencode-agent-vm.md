# OpenCode Agent VM

`opencode-agent-vm.nix` defines a reusable NixOS service for running OpenCode in an isolated MicroVM. The VM starts `opencode serve` on boot, exposes the web UI through a host-local proxy, and only sees VM-owned state plus any extra host paths explicitly configured as shares.

## Motivation

The goal is to give an LLM coding agent more freedom inside its own machine without giving it implicit access to the host user's home directory, SSH agent, OpenCode state, Claude state, or secrets. The isolation boundary is the MicroVM plus its explicit share list.

The first deployment target was `lapdog`, where the VM is used locally. `mini` also enables the same service with its own host flake guest configuration and a Caddy/Authentik frontend.

## Operating It

After applying a host generation that enables `dog.services.opencode-agent-vm`, these commands are installed in the system profile:

```sh
start-opencode-agent-vm
stop-opencode-agent-vm
opencode-agent-vm-ssh
opencode-agent-vm-public-key
opencode-agent-vm-logs
```

`start-opencode-agent-vm` initializes the VM state, starts `microvm@opencode-agent-vm.service`, and prints the local web URL. Hosts may also set `autostart = true` to start the VM at boot. On `lapdog`, the UI is available at:

```text
http://127.0.0.1:32860
```

`opencode-agent-vm-public-key` prints the VM outbound SSH public key. Add this key manually to GitHub, Forgejo, or other Git remotes when the agent needs SSH access. The private key stays under `/var/lib/opencode-agent-vm/ssh/guest/` and is mounted into the guest read-only.

`opencode-agent-vm-ssh` logs into the guest as `agent` using the host-to-VM maintenance key generated for this VM. Host key checking is disabled because this VM is expected to be replaceable.

Useful logs:

```sh
opencode-agent-vm-logs
journalctl -u dnsmasq -g 'query'
journalctl -k -g '[opencode-agent-vm]'
```

The first command follows the service units. The second shows DNS queries logged by the host `dnsmasq`. The third shows new outbound TCP/UDP connection logs from the VM bridge.

## Host And Guest Configuration

The shared module has two sides:

```nix
dog.services.opencode-agent-vm.enable = true;
dog.services.opencode-agent-vm.guest.enable = true;
```

The host side requires `microvm.nixosModules.host`. It creates the host systemd units, bridge, NAT, dnsmasq integration, helper commands, credentials, and `microvm.vms.<vmName>` declaration.

The guest side requires `microvm.nixosModules.microvm`. It configures the VM OS, user, static network, shared filesystems, SSH, Home Manager, and the `opencode serve` systemd service.

Each host flake that enables the service must expose a guest NixOS configuration named the same as `vmName`. On `lapdog` and `mini`, this is `nixosConfigurations.opencode-agent-vm`.

## State And Credentials

Persistent VM state lives under:

```text
/var/lib/opencode-agent-vm/
```

Important subdirectories:

```text
/var/lib/opencode-agent-vm/home/
/var/lib/opencode-agent-vm/ssh/host/
/var/lib/opencode-agent-vm/ssh/guest/
```

The init unit, `opencode-agent-vm-init.service`, creates this state before both `microvm@opencode-agent-vm.service` and `microvm-virtiofsd@opencode-agent-vm.service`. This ordering matters because virtiofsd needs the share source directories to exist before the VM starts.

Generated keypairs:

- `ssh/host/host_to_vm`: host maintenance key used by `opencode-agent-vm-ssh`.
- `ssh/guest/vm_outbound`: VM outbound key for Git remotes.
- `ssh/host/vm_outbound.pub`: host-readable copy used by `opencode-agent-vm-public-key`.

The module does not mount the host user's `~/.ssh`, SSH agent socket, `~/.config/opencode`, `~/.local/share/opencode`, `~/.claude`, or `.claude.json`.

## Networking

The VM uses TAP networking through a host bridge. The `lapdog` configuration uses:

```text
Bridge:       opencode-vm0
TAP:          opencode-agent0
Host IP:      10.0.101.1/24
Guest IP:     10.0.101.2/24
Network CIDR: 10.0.101.0/24
```

The host enables IPv4 forwarding and NAT for the VM network. Guest DNS points at the host bridge IP. The module merges the bridge listen address into the existing host `dnsmasq` service and logs queries there.

By default, the module also adds the VM bridge as an explicit dnsmasq `interface` and enables `bind-interfaces`. This is suitable for `lapdog`, where dnsmasq is dedicated to VM DNS.

On `mini`, `dnsmasqBindBridgeInterface = false` keeps dnsmasq available on the LAN, Tailscale, loopback, and the VM bridge by listen address without restricting it to only the VM bridge. `mini` also sets `dnsmasqBindInterfaces = false` because its Tailscale listen address may not exist yet when dnsmasq starts after boot; strict binding makes dnsmasq fail with `Cannot assign requested address` in that case.

`nftables` logs new outbound TCP/UDP connections from the VM bridge with the prefix `[opencode-agent-vm]`. This is audit visibility, not an outbound allowlist. The VM can still reach the LAN and internet unless additional firewall rules are added.

## Web UI Exposure

Inside the guest, OpenCode runs as:

```sh
opencode serve --hostname 10.0.101.2 --port 32859
```

The host runs a local-only `socat` proxy:

```text
127.0.0.1:32860 -> 10.0.101.2:32859
```

This keeps the local proxy reachable only from the host. On `mini`, Caddy exposes `https://opencode.local.doreto.com.br`, protects it with Authentik `forward_auth`, and autostarts the VM at boot before proxying to the host-local service.

## Filesystem Shares

The VM always mounts its VM-owned persistent home and VM SSH key material from `stateDir`. Additional host directory shares are explicit module configuration through `dog.services.opencode-agent-vm.shares`, and the list may be empty.

The current `lapdog` test configuration intentionally adds this extra host share:

```text
/home/dog/projects -> /home/agent/projects (read-write)
```

This broad share is an accepted initial testing tradeoff on `lapdog`. It gives the VM write access to project checkouts under `/home/dog/projects`, including this dotfiles checkout.

`mini` intentionally has no additional host directory shares. It starts OpenCode in `/home/agent`, which is backed by VM-owned persistent state under `/var/lib/opencode-agent-vm/home`. Because the service is exposed through Caddy, `mini` sets `autostart = true` so the host-local proxy is present after boot.

Always keep sensitive host state out of the share list unless that access is intentional.

## Guest Runtime

The guest user is:

```text
agent uid=1000 gid=1000
```

UID/GID `1000` keeps ownership compatible with the host user for shared project files. The username differs from the host user so shells, logs, and prompts make it clear when commands are running inside the VM.

The guest user is in `wheel`, and `security.sudo.wheelNeedsPassword = false`. This is intentional: the agent should be able to administer the guest freely. The boundary is the VM, network policy, VM-owned credentials, and explicit shares.

The guest home is a persistent virtiofs share from `/var/lib/opencode-agent-vm/home` mounted at `/home/agent`. The host `/nix/store` is mounted read-only at `/nix/.ro-store` and combined with `writableStoreOverlay = "/nix/.rw-store"` so the guest can use Nix without writing to the host store.

The guest includes the GitHub CLI (`gh`) and Forgejo CLI (`forgejo-cli`). Git is configured for the `agent` user through the shared `dog.programs.git` Home Manager module, so the VM gets the same default identity, aliases, ignores, and private include path as the rest of the dotfiles.

The `opencode serve` systemd service runs with `PATH` pointing at `/etc/profiles/per-user/agent/bin`, `/run/current-system/sw/bin`, and `/run/wrappers/bin`. This makes Home Manager tools such as `npm` available to commands launched by OpenCode, matching the tools visible in an SSH session more closely than systemd's default service environment.

## Lapdog Integration

`hosts/lapdog/flake.nix` defines the shared `opencodeAgentVm` settings and passes them through `specialArgs`. It also imports `../../modules/nixos` and adds the guest output:

```text
nixosConfigurations.opencode-agent-vm
```

`hosts/lapdog/configuration.nix` enables the host side with:

```nix
dog.services.opencode-agent-vm = opencodeAgentVm // {
  enable = true;
};
```

## Mini Integration

`hosts/mini/flake.nix` imports `../../modules/nixos`, adds `microvm.nix`, defines `opencodeAgentVm`, and exposes `nixosConfigurations.opencode-agent-vm` for the guest. `hosts/mini/configuration.nix` enables the host side.

The previous host-level `opencode-web` service has been removed. `opencode.local.doreto.com.br` is now owned by the VM module's Caddy integration and proxies through `127.0.0.1:32860`.

`mini` relies on the default `shares = [ ]`, so the VM does not mount `/home/dog/projects` or any other additional host directory.

`mini` already runs dnsmasq for LAN and local-domain DNS. Its dnsmasq `listen-address` values are list-shaped so the VM module can append `10.0.101.1`, while `dnsmasqBindBridgeInterface = false` and `dnsmasqBindInterfaces = false` preserve the host's non-strict binding behavior.

## Validation

Before switching a host generation, validate with:

```sh
nix flake check ./hosts/lapdog/
nix build ./hosts/lapdog#nixosConfigurations.lapdog.config.system.build.toplevel
nix build ./hosts/lapdog#nixosConfigurations.opencode-agent-vm.config.system.build.toplevel
nix flake check ./hosts/mini/
nix build ./hosts/mini#nixosConfigurations.dogdot.config.system.build.toplevel
nix build ./hosts/mini#nixosConfigurations.opencode-agent-vm.config.system.build.toplevel
```

Do not run activation commands from automation. The user switches generations manually.
