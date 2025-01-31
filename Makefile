.PHONY: dogdot-switch
dogdot-switch:
	home-manager switch --flake .#home

.PHONY: chungus-boot
chungus-boot:
	sudo -A nixos-rebuild --flake .#chungus boot

.PHONY: chungus-switch
chungus-switch:
	sudo -A nixos-rebuild --flake .#chungus switch

.PHONY: clean
clean:
	nix-collect-garbage -d
