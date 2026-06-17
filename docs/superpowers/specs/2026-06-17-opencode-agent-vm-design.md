# OpenCode Agent MicroVM Design

## Goal

Create a reusable NixOS module for an isolated OpenCode agent environment. The environment runs inside a microVM, starts `opencode serve` automatically, and gives the agent broad freedom inside the guest while keeping host filesystem, credentials, and service exposure explicit.

The first consumer is `lapdog` for local testing. The design must also support a later `mini` deployment where the same VM service is reachable remotely through Caddy and Authentik.

## Non-Goals

- Do not replace the existing `lapdog-agent` microVM immediately.
- Do not share host OpenCode, Claude, SSH, or agent configuration into the VM.
- Do not expose the OpenCode web UI directly to the LAN or internet during the initial `lapdog` phase.
- Do not automate adding the VM outbound public key to GitHub, Forgejo, or other remotes.

## Architecture

Add a reusable NixOS module under `modules/nixos/services/opencode-agent-vm.nix`, exposed through a new shared NixOS module aggregator at `modules/nixos/default.nix`. Use the option namespace `dog.services.opencode-agent-vm`.

The module owns the common host-side machinery:

- `microvm.vms.<name>` declaration.
- State directory management under `/var/lib/opencode-agent-vm/`.
- VM-only SSH key generation.
- Bridge, TAP, NAT, DNS, and audit logging configuration.
- Host-local exposure for the guest `opencode serve` endpoint.
- Convenience commands for start, stop, SSH, logs, and public key display.

Each host flake that enables the service also defines a guest NixOS configuration output, similar to the current `lapdog-agent` output. The guest output imports `microvm.nixosModules.microvm` and the guest half of the shared module. The initial `lapdog` implementation should live alongside the existing `lapdog-agent` until the new setup is validated.

## Module Configuration

The module should expose host-level options for the values that differ between `lapdog` and `mini`:

- VM name, defaulting to `opencode-agent-vm`.
- State directory, defaulting to `/var/lib/opencode-agent-vm`.
- Host bridge name and TAP interface name.
- Host IP, guest IP, subnet/prefix, and guest gateway.
- Guest hostname.
- Guest vCPU and memory allocation.
- OpenCode guest listen port.
- Host-local listen address and port for local browser access.
- Explicit filesystem shares, including host path, guest path, and read-only/read-write mode.
- DNS upstreams and optional home-domain forwarding.
- Whether to enable outbound DNS and connection audit logging.
- Whether to configure a Caddy/Authentik protected virtual host later on `mini`.

The initial `lapdog` share configuration is:

- Host path: `/home/dog/projects`
- Guest path: `/workspace/projects`
- Mode: read-write

This share is not a module default. It is only the initial `lapdog` host configuration. `mini` must provide its own explicit share list later.

## Isolation Boundaries

The VM must not mount host personal state by default. In particular, it must not share:

- `~/.ssh`
- `~/.config/opencode`
- `~/.local/share/opencode`
- `~/.claude`
- `~/.claude.json`
- `.agents` or other host agent configuration
- Personal secret directories

Persistent VM-owned state lives under `/var/lib/opencode-agent-vm/`. The directory should be subdivided for clarity, for example:

- `home/` for the VM user's persistent home.
- `ssh/` for VM-only SSH identities.
- `runtime/` or service-specific state if needed.

The guest user is named `agent` and uses UID/GID `1000`. Matching UID/GID keeps ownership compatible with the host user's shared project files, while the distinct username makes VM prompts and logs visibly different from the host.

The guest user has passwordless sudo. The trust boundary is the VM, explicit shares, generated VM-only credentials, and network policy rather than repeated prompts inside the guest.

## Credential Model

The module generates VM-specific credentials at runtime if they are missing. Generated secrets must live outside the Nix store under `/var/lib/opencode-agent-vm/ssh/`.

Two keypairs are used:

- Host-to-VM SSH key: used by host convenience commands for maintenance access to the guest.
- VM outbound SSH key: used by the agent for GitHub, Forgejo, or other outbound Git/SSH workflows.

The VM outbound public key should be available through a convenience command such as `opencode-agent-vm-public-key`. Adding that key to GitHub, Forgejo, or other remotes remains manual.

No personal host SSH keys or host SSH agent socket should be mounted into the VM.

## Networking

Use the existing `lapdog-agent` bridge/TAP model, generalized behind module options.

For `lapdog`, use a network distinct from the existing `lapdog-agent` setup to avoid collisions, for example:

- Host bridge: configurable, for example `opencode-vm0`.
- TAP interface: configurable, for example `opencode-agent0`.
- Host IP: `10.0.101.1/24`.
- Guest IP: `10.0.101.2/24`.

The host provides NAT and IPv4 forwarding so the VM can reach the LAN and internet. The VM uses the host bridge IP as its gateway and DNS server.

Host `dnsmasq` listens on the VM bridge address, logs DNS queries, and forwards queries. Home domains can be forwarded to `mini`, following the existing `lapdog-agent` pattern. Host `nftables` logs new outbound TCP/UDP connections from the VM network.

This preserves agent freedom while giving the user visibility into network behavior. It does not prevent outbound LAN or internet access by itself; restrictions can be added later through host firewall rules if needed.

## Web Exposure

Inside the guest, `opencode serve` listens on the guest private IP and configured port. On `lapdog`, the host exposes that service only on a local interface such as `127.0.0.1:<hostPort>`. This makes the UI available to the local browser without exposing it to the LAN.

For the later `mini` deployment, expose the host-local service through Caddy with Authentik `forward_auth`. The OpenCode web server should not be treated as the only authentication boundary for a remotely reachable empowered agent.

## Guest Runtime

The guest OS is a minimal NixOS microVM. It includes:

- `microvm.nixosModules.microvm`.
- Home Manager for user `agent`.
- `opencode` from the existing `llm-agents` overlay.
- Git and OpenSSH client.
- Common CLI tools useful for agent work.
- Nix with `nix-command` and `flakes` enabled.

Avoid enabling the existing `dog.programs.opencode` Home Manager module in the guest because it assumes host dotfiles paths and bubblewrap-oriented state sharing. The VM should have separate OpenCode config and data under its own persistent home/state.

`opencode serve` runs as a systemd service as user `agent`, with `HOME=/home/agent` and default working directory `/workspace/projects` for the initial `lapdog` configuration.

## Operations

Add host-side convenience commands through the host module:

- `start-opencode-agent-vm`
- `stop-opencode-agent-vm`
- `opencode-agent-vm-ssh`
- `opencode-agent-vm-public-key`
- `opencode-agent-vm-logs`

Startup should ensure the state directory, persistent home, share staging if needed, and VM-only SSH keys exist before the microVM starts.

The start command should print:

- Local web URL.
- SSH command hint.
- DNS audit command.
- Connection audit command.
- Public key command for remote Git/SSH setup.

## Validation

Implementation validation should run:

```sh
nix flake check ./hosts/lapdog/
```

If evaluation succeeds, build the `lapdog` system toplevel. If needed, also build the guest NixOS configuration. Do not run activation commands such as `nixos-rebuild switch`, `nixos-rebuild boot`, or `home-manager switch`; the user performs deployment manually.

## Later Mini Integration

The later `mini` phase should:

- Add the `microvm.nix` input and host module to `hosts/mini/flake.nix`.
- Enable the shared `opencode-agent-vm` module from `mini`.
- Configure `mini`-specific explicit shares.
- Expose the service through Caddy.
- Protect the Caddy route with Authentik `forward_auth`.
- Keep VM-owned state under `/var/lib/opencode-agent-vm/` on `mini`.

This should be a small host configuration change rather than a redesign.
