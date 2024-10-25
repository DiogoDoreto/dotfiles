.PHONY: update
update:
	home-manager switch --flake .#dog

.PHONY: update-backup
update-backup:
	home-manager switch -b backup --flake .#dog

.PHONY: clean
clean:
	nix-collect-garbage -d
