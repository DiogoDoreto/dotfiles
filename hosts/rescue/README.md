# Rescue image

Build the ISO on an x86-64 NixOS machine:

```sh
nix build path:./hosts/rescue#isoImage
```

The ISO is in `result/iso/`. Copy it onto the Ventoy drive as a normal ISO
file and choose it from Ventoy's menu. Boot normally for KDE, or select
**Options → Disable display-manager** for a console when graphics fail.
The boot menu also offers Memtest86+. On machines with Secure Boot enabled,
disable it temporarily to boot this image.

The live account is `dog`, with passwordless sudo and key-only SSH from lapdog
or the iPhone. Use `ip -br addr` to find the address and
`ssh-keygen -lf /etc/ssh/ssh_host_ed25519_key.pub` to verify its temporary SSH
fingerprint. A new SSH host key is generated on each boot.

KDE disk mounts default to read-only, except for USB devices, which are writable
destinations. USB-attached recovery sources also fall under that exception:
mount them explicitly read-only before inspection. See `/etc/rescue-guide` in
the live system for the short operational guide. The session does not persist.

Test both the KDE and console entries on the Ventoy stick before relying on it
for a recovery session.
