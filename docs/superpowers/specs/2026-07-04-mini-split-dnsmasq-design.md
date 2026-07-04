# Mini Split Dnsmasq Design

## Goal

Mini currently has one `dnsmasq` instance serving DNS for both the LAN and Tailscale addresses. That mixed setup can return LAN-only addresses to Tailscale clients, or Tailscale addresses to LAN-only clients, which breaks clients outside the matching network.

The new setup should serve the same local service names on both networks while returning the address that is reachable from the querying network:

- LAN clients get `*.local.doreto.com.br -> 192.168.0.2`.
- Tailscale clients get `*.local.doreto.com.br -> 100.117.142.110`.

## Current Context

The existing mini DNS configuration lives in `hosts/mini/networking.nix` under `services.dnsmasq`. It currently listens on loopback, the LAN static IP, the `chungus-proxy` dummy IP, and the Tailscale IP.

Relevant existing values:

- LAN static IP: `192.168.0.2`
- Tailscale IP: `100.117.142.110`
- Chungus proxy IP: `vars.chungusProxyIp`, currently `192.168.0.4`
- LAN interface: `wlo1`
- Upstream DNS servers: `1.1.1.1`, `1.0.0.1`

The NixOS `services.dnsmasq` module generates a key-value config, validates it with `dnsmasq --test -C <config>`, runs dnsmasq in foreground mode with `-k`, and uses the `dnsmasq` user and group. The custom split services should keep those parts where they still fit.

## Design

Replace the single NixOS `services.dnsmasq` instance on mini with two explicit systemd-managed dnsmasq instances:

- `dnsmasq-lan.service`
- `dnsmasq-tailscale.service`

Each service gets its own generated config file and its own process. The instances do not share a dnsmasq DBus name, because two `--enable-dbus` dnsmasq processes would contend for `uk.org.thekelleys.dnsmasq`.

## LAN Instance

The LAN instance listens only on addresses reachable from local LAN clients and local host processes:

- `127.0.0.1`
- `::1`
- `192.168.0.2`
- `vars.chungusProxyIp`

It answers:

- `/${config.networking.hostName}.home/192.168.0.2`
- `/local.doreto.com.br/192.168.0.2`
- `/chungus.home/192.168.0.3`
- `/chungus-proxy.home/${vars.chungusProxyIp}`

It keeps the current resolver behavior for local processes by adding `127.0.0.1` to `networking.nameservers` if the built-in dnsmasq module is no longer responsible for that.

## Tailscale Instance

The Tailscale instance listens only on mini's Tailscale address:

- `100.117.142.110`

It answers:

- `/local.doreto.com.br/100.117.142.110`

It should not answer LAN-only `.home` records unless there is a specific need later. This keeps Tailscale DNS focused on service names that are intentionally exposed through mini's Tailscale address.

The service should start after the Tailscale service and network setup. It should use `bind-dynamic` so startup tolerates the Tailscale address appearing after dnsmasq starts.

## Shared Dnsmasq Settings

Both instances should keep the safe shared settings from the current config:

- `bind-dynamic = true`
- `no-hosts = true`
- `no-resolv = true`
- `bogus-priv = true`
- `domain-needed = true`
- `server = [ "1.1.1.1" "1.0.0.1" ]`
- `log-queries = true`

Each service should run `dnsmasq --test -C <generated-config>` in `preStart` and then run dnsmasq with `-k --user=dnsmasq -C <generated-config>`.

## System Integration

The built-in `services.dnsmasq.enable` option should be disabled for mini to avoid a third process competing for port 53.

Because the built-in module creates the `dnsmasq` user and group only when enabled, the split implementation must define the same system user and group locally.

The firewall can keep allowing TCP and UDP port 53 as it does today. Network separation comes from the per-instance listener addresses, not from firewall rules.

## Error Handling

Each dnsmasq config is validated before service start. A bad config should fail the affected service before it begins answering DNS.

The services should use `Restart=always`, matching the current `alwaysKeepRunning = true` behavior. If one instance fails, the other instance can continue serving its network.

## Verification

Run `nix flake check ./hosts/mini/` after editing the Nix config.

After deployment, verify both service units are running and that DNS answers are network-specific:

- LAN query to `192.168.0.2`: `local.doreto.com.br` resolves to `192.168.0.2`.
- Tailscale query to `100.117.142.110`: `local.doreto.com.br` resolves to `100.117.142.110`.
- LAN query does not return the Tailscale address.
- Tailscale query does not return the LAN address.
