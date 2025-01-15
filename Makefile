.PHONY: update-home
update-home:
	home-manager switch --flake .#home

.PHONY: wsl-switch
update-work:
	sudo nixos-rebuild --flake .#wsl switch

.PHONY: update-work
update-work:
	home-manager switch --flake .#work

.PHONY: clean
clean:
	nix-collect-garbage -d
