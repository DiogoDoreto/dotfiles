.PHONY: hm-dog-chungus-switch
hm-dog-chungus-switch:
	home-manager --flake .#dog@chungus switch

.PHONY: nixos-chungus-boot
nixos-chungus-boot:
	sudo -A nixos-rebuild --flake .#chungus boot

.PHONY: nixos-chungus-switch
nixos-chungus-switch:
	sudo -A nixos-rebuild --flake .#chungus switch

.PHONY: hm-dog-mini-switch
hm-dog-mini-switch:
	home-manager --flake .#dog@mini switch

.PHONY: nixos-mini-boot
nixos-mini-boot:
	sudo -A nixos-rebuild --flake .#mini boot

.PHONY: nixos-mini-switch
nixos-mini-switch:
	sudo -A nixos-rebuild --flake .#mini switch

.PHONY: gc
gc:
	nix-collect-garbage -d

.PHONY: nixos-delete-generations
nixos-delete-generations:
	sudo -A nix-env --delete-generations 40d --profile /nix/var/nix/profiles/system
	sudo -A nix-collect-garbage --delete-older-than 40d
	sudo -A /nix/var/nix/profiles/system/bin/switch-to-configuration switch
