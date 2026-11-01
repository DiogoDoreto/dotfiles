# T3 Code on Chungus

T3 runs at boot as `dog` via the system service `t3code.service`, independently
of desktop login. State, pairing credentials, and sessions live in
`/home/dog/.t3`. The service restarts on failure.

## Access and pairing

The backend listens at **http://192.168.0.3:32862**. The firewall permits only
`192.168.0.0/24` clients arriving on `enp4s0`. There is no HTTPS proxy and no
change to router port forwarding. Native T3 authentication remains enabled;
plain HTTP does not protect traffic from observers on the LAN.

After manually deploying the NixOS configuration, run as `dog` on Chungus:

```sh
systemctl status t3code
journalctl -u t3code -f
# Generate a pairing link/token; treat its output as a credential.
t3 auth pairing create --base-url http://192.168.0.3:32862
```

Use T3's pairing flow to add this backend to the desktop app or a LAN browser.
The installed desktop has its app-managed local environment disabled during
Home Manager activation, so it does not launch a second backend. Other desktop
preferences are preserved. The persistent service is a separately paired
environment even when using the desktop on Chungus itself.

## Agents

T3's bundled provider packages are disabled. It uses explicitly installed agents
from `dog`'s login-shell PATH. Pi and pi-acp are installed by the shared
`dog.programs.pi` Home Manager module; existing OpenCode remains unchanged.
The pinned T3 version has a native Pi provider using the `pi` executable.
Configure/authenticate Pi as `dog` before using that provider in T3.

Pi uses bubblewrap by default, with writable access to `~/.pi/`, `~/.agents`,
and Chungus's `~/p/` project directory. Create these directories before use
(the wrapper only binds paths that already exist). The shared Pi module also
links the Nix-managed `pi-web-access` extension into
`~/.pi/agent/extensions/pi-web-access`. Do not additionally install a second copy
with npm or `pi install`. Provider credentials remain mutable in the home
folder, not in Nix configuration.

T3 itself runs directly as `dog`, not inside the Pi sandbox. Wrapping `pi`
does not establish an isolation boundary for every operation T3 can perform.

In the Agent VM, the same Pi Home Manager module is enabled with
`dog.programs.pi.sandbox.enable = false`, since the VM supplies isolation.
