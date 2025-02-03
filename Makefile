.PHONY: hm-dog-dogdot-switch
hm-dog-dogdot-switch:
	home-manager --flake .#dog@dogdot switch

.PHONY: hm-dog-dogdot-switch
hm-dog-chungus-switch:
	home-manager --flake .#dog@chungus switch

.PHONY: nixos-chungus-boot
nixos-chungus-boot:
	sudo -A nixos-rebuild --flake .#chungus boot

.PHONY: nixos-chungus-switch
nixos-chungus-switch:
	sudo -A nixos-rebuild --flake .#chungus switch

.PHONY: clean
clean:
	nix-collect-garbage -d
