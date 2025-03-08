.PHONY: hm-dog-dogdot-switch
hm-dog-dogdot-switch:
	home-manager --flake .#dog@dogdot switch

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

.PHONY: clean
clean:
	nix-collect-garbage -d
