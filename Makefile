.PHONY: remote-mini-switch
remote-mini-switch:
	nh os switch --target-host dogdot --build-host dogdot -H dogdot ./hosts/mini/

.PHONY: remote-mini-boot
remote-mini-boot:
	nh os boot --target-host dogdot --build-host dogdot -H dogdot ./hosts/mini/ && ssh dogdot reboot

.PHONY: hm-dog-chungus-switch
hm-dog-chungus-switch:
	home-manager --flake .#dog@chungus switch

.PHONY: nixos-chungus-boot
nixos-chungus-boot:
	sudo -A nixos-rebuild --flake .#chungus boot

.PHONY: nixos-chungus-switch
nixos-chungus-switch:
	sudo -A nixos-rebuild --flake .#chungus switch

.PHONY: gc
gc:
	nix-collect-garbage -d

.PHONY: nixos-delete-generations
nixos-delete-generations:
	sudo -A nix-env --delete-generations 40d --profile /nix/var/nix/profiles/system
	sudo -A nix-collect-garbage --delete-older-than 40d
	sudo -A /nix/var/nix/profiles/system/bin/switch-to-configuration switch
