# Agent VM — microvm isolation for coding agents

Provides hardware-level isolation for AI coding agents (claude-code, opencode,
copilot) via QEMU microvms.  Each VM is an ephemeral NixOS guest sharing only
specific host directories through virtiofs.  A router VM handles all egress:
NAT, DNS logging, and connection auditing.

---

## Architecture

```
                  host (non-NixOS, no system-level changes)
 ┌────────────────────────────────────────────────────────────────────┐
 │  vde_switch   ←→  agent-vm-net.sock  (userspace L2, no kernel iface)│
 │       ▲                   ▲                                          │
 │       │                   │                                          │
 │  ┌────┴───────┐    ┌───────┴──────────┐                              │
 │  │ agent-router│    │  agent-work      │  (one per vms/<name>/ dir)  │
 │  │ 10.0.0.1   │    │  10.0.0.2        │                              │
 │  │            │    │  default gw →    │                              │
 │  │ eth0: vde  │    │  10.0.0.1        │                              │
 │  │ eth1: SLIRP│    │  eth0: vde only  │                              │
 │  │            │    │  virtiofs shares │                              │
 │  │ dnsmasq    │    │  claude-code,    │                              │
 │  │ nftables   │    │  opencode, …     │                              │
 │  │ NAT+audit  │    └──────────────────┘                              │
 │  └────────────┘                                                      │
 │       │ SLIRP → host network → internet (VPN-aware)                  │
 └───────┼────────────────────────────────────────────────────────────── ┘
         ▼
      internet
```

Key properties:
- **No host bridge or TAP**: the vde switch is a Unix-socket-based userspace
  switch; QEMU opens it as a regular file.  No root required at runtime.
- **VPN pass-through**: SLIRP makes outbound connections via ordinary
  `socket()` calls from the host's network namespace, so VPN routes and
  DNS (including GlobalProtect corp DNS via `systemd-resolved`) apply
  automatically.  The router forwards DNS to `10.0.2.3` — QEMU SLIRP's
  alias for the host resolver.
- **SSH access via ProxyJump**: the router VM is reachable at
  `localhost:2200` via SLIRP hostfwd.  Agent VMs are accessed through the
  router: `ssh agent-work` → ProxyJump agent-router → 10.0.0.2:22.
- **Ephemeral root, persistent home**: the guest Nix store is a tmpfs overlay
  on top of the host's `/nix/store` (virtiofs read-only share).  The user's
  home (`/home/dog`) is a virtiofs-mounted host directory that survives
  restarts.
- **Audit trail** (inside the router VM):
  - DNS: `journalctl -u dnsmasq -g 'query'` — every name lookup
  - Connections: `journalctl -k -g '\[vm-agent\]'` — every new TCP/UDP

---

## Module files

| File | Role |
|------|------|
| `guest.nix` | Base NixOS module for any VM.  Options: hostname, user, uid, shares, networking. |
| `router.nix` | Extension for the router role: SLIRP NIC, NAT, audit nftables, dnsmasq. |
| `../../home-manager/programs/agent-vm.nix` | HM module: `agent-vm` CLI, user systemd units, SSH config. |

---

## VM directory convention

Each named VM lives under `vms/<name>/` in the repo root:

```
vms/
  work/
    meta.nix    ← flat attrset: net.address, net.mac, identityFile
    config.nix  ← NixOS module: dog.microvm-agent.* overrides + shares
    home.nix    ← home-manager config for the dog user inside the VM
  sandbox/      ← copy vms/work/ and adjust addresses/shares
    …
```

`flake.nix` calls `builtins.readDir ./vms` and generates one
`nixosConfigurations.agent-<name>` plus `packages.<system>.agent-<name>-runner`
per directory.  **Adding a new VM is: copy the directory, edit the three
files, run `home-manager switch`.**

### meta.nix (flat attrset, not a NixOS module)

```nix
{
  net.address  = "10.0.0.2";           # IP on the vde network (no CIDR)
  net.mac      = "02:00:00:00:01:01";  # must be unique across all VMs
  identityFile = "~/.ssh/id_agent_vm"; # SSH key used by the host
}
```

Consumed by `flake.nix` to wire up the router and generate SSH config.

### config.nix (NixOS module)

Sets `dog.microvm-agent.*` (hostname, sshAuthorizedKeys, net.address/mac,
virtiofs shares, microvm.vcpu/mem).  See `vms/work/config.nix` for the
annotated template.

### home.nix (home-manager module)

A full home-manager config that can use **any** `dog.programs.*` module
because `dotfiles-dog/modules/home-manager` is loaded as
`home-manager.sharedModules` in the nixosSystem.

```nix
# vms/sandbox/home.nix
{ pkgs, ... }: {
  home.username     = "dog";
  home.homeDirectory = "/home/dog";
  home.packages = with pkgs; [ llm-agents.opencode ];

  dog.programs = {
    cli-tools.enable = true;
    git.enable       = true;
    claude-code = { enable = true; bubblewrap.enable = false; };
  };
}
```

Note: disable bubblewrap inside VMs — the VM boundary is the isolation layer.

---

## Network addressing

| Host | IP | Notes |
|------|----|-------|
| Router VM (vde) | `10.0.0.1/24` | Gateway for all agent VMs |
| Router VM (SLIRP) | DHCP from QEMU | Internet uplink; 10.0.2.3 = host resolver |
| `vms/work/` | `10.0.0.2/24` | — |
| Next VM | `10.0.0.3/24` | Increment last octet |

MAC address scheme (locally administered, `02:...`):

| NIC | MAC |
|-----|-----|
| Router vde | `02:00:00:00:00:01` |
| Router SLIRP | `02:00:00:ff:ff:02` |
| work vde | `02:00:00:00:01:01` |
| sandbox vde | `02:00:00:00:02:01` |

---

## One-time host setup (non-NixOS)

These three commands are the **entire** system-level footprint.  Run once
after cloning the repo; `home-manager switch` handles everything else.

```bash
# 1. Load KVM on every boot
echo kvm-intel | sudo tee /etc/modules-load.d/kvm.conf
sudo modprobe kvm-intel

# 2. Grant KVM device access (log out + back in after this)
sudo usermod -aG kvm $USER

# 3. Keep user systemd session alive for long-running VMs
loginctl enable-linger $USER
```

Verify: `ls /dev/kvm` and `id -nG | grep -w kvm`.

### SSH keypair for VM access

```bash
ssh-keygen -t ed25519 -f ~/.ssh/id_agent_vm -C "agent-vm"
# Paste the PUBLIC key into vms/<name>/config.nix → sshAuthorizedKeys
cat ~/.ssh/id_agent_vm.pub
```

### Persistent home directories

Created automatically by `systemd-tmpfiles` after `home-manager switch`.
Force-create manually if needed:
```bash
mkdir -p ~/.local/share/agent-vm/work/home
```

---

## Day-to-day workflow

```bash
# Start the work VM (switch + router start automatically)
agent-vm start work

# SSH into the VM
ssh agent-work
# or: agent-vm ssh work

# Run an agent inside the VM
ssh agent-work claude

# Audit what the agent did
agent-vm logs --audit

# Stop everything
agent-vm stop --all

# Wipe the VM's persistent home for a clean slate
agent-vm nuke work
```

---

## Adding a second VM (e.g. "sandbox")

```bash
cp -r vms/work vms/sandbox
```

Edit `vms/sandbox/meta.nix`:
```nix
{ net.address = "10.0.0.3"; net.mac = "02:00:00:00:02:01"; identityFile = "~/.ssh/id_agent_vm"; }
```

Edit `vms/sandbox/config.nix`: change `hostname`, `net.address`, `net.mac`,
and the `agent-home` share source path to
`~/.local/share/agent-vm/sandbox/home`.

Add to your home-manager config:
```nix
dog.programs.agent-vm.vms.sandbox = { guestIp = "10.0.0.3"; };
```

Run `home-manager switch`.  The flake auto-discovers the new directory and
builds `agent-sandbox-runner`.

---

## Differences from the lapdog setup

| Aspect | lapdog (NixOS) | non-NixOS host (vde) |
|--------|----------------|----------------------|
| Host networking | TAP + kernel bridge `vm0` | vde userspace switch (no host iface) |
| NAT | host nftables | router VM nftables |
| DNS audit | host dnsmasq | router VM dnsmasq |
| Connection audit | host nftables log | router VM nftables log |
| SSH access | direct `ssh lapdog-agent` | ProxyJump via router |
| Auth overhead | none (kernel bridge) | one SSH hop overhead |
| `/etc` changes | bridge, nftables, dnsmasq | **none** |
| Root at runtime | yes (microvm@.service) | **no** |

The audit information is identical; it just lives inside the router VM rather
than the host journal.  Fetch it with `agent-vm logs --audit`.

---

## Phase 3: refactoring lapdog (future)

Once the vde setup is stable, lapdog can be refactored to use this shared
module by:

1. Replacing `hosts/lapdog/microvm-guest.nix` with:
   ```nix
   imports = [ ../../modules/nixos/microvm-agent/guest.nix ];
   dog.microvm-agent = {
     hostname = "lapdog-agent";
     net.backend = "tap";
     net.tapId   = "vm-agent0";
     net.mac     = "02:00:00:00:00:01";
     net.address = "10.0.100.2/24";
     net.gateway = "10.0.100.1";
     net.dns     = "10.0.100.1";
     sshAuthorizedKeys = [ "ssh-ed25519 AAAA..." ];
     shares = [ … existing shares … ];
   };
   ```
2. Keeping `hosts/lapdog/configuration.nix` unchanged — lapdog still manages
   the host-side bridge, NAT, and dnsmasq itself.

No lapdog behaviour change; only code deduplication.
