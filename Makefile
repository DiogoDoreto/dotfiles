.PHONY: lapdog-build
lapdog-build:
	./run-and-notify.sh --app-name=Nix -t 0 --icon=nix-snowflake "Lapdog build" -- \
	nh os build ./hosts/lapdog/

.PHONY: lapdog-boot
lapdog-boot:
	nh os boot ./hosts/lapdog/

.PHONY: lapdog-switch
lapdog-switch:
	nh os switch ./hosts/lapdog/

.PHONY: lapdog-flake-update
lapdog-flake-update:
	./run-and-notify.sh --app-name=Nix -t 0 --icon=nix-snowflake "Lapdog flake update" -- \
	nix flake update --flake ./hosts/lapdog/

.PHONY: test-org-notes-remote-mini-switch
test-org-notes-remote-mini-switch:
	cd hosts/mini/ && nix flake update nextcloud-org-notes
	./run-and-notify.sh --app-name=Nix -t 0 --icon=nix-snowflake "Mini switch" -- \
	'nh os switch --target-host dogdot -H dogdot --elevation-strategy passwordless ./hosts/mini/'

.PHONY: remote-mini-switch
remote-mini-switch:
	./run-and-notify.sh --app-name=Nix -t 0 --icon=nix-snowflake "Mini switch" -- \
	'nh os switch --target-host dogdot -H dogdot --elevation-strategy passwordless ./hosts/mini/'

.PHONY: remote-mini-boot
remote-mini-boot:
	./run-and-notify.sh --app-name=Nix -t 0 --icon=nix-snowflake "Mini boot" -- \
	'nh os boot --target-host dogdot -H dogdot --elevation-strategy passwordless ./hosts/mini/ && ssh dogdot reboot'

.PHONY: remote-chungus-switch
remote-chungus-switch:
	./run-and-notify.sh --app-name=Nix -t 0 --icon=nix-snowflake "Chungus switch" -- \
	'nh os switch --build-host chungus --target-host chungus -H chungus ./hosts/chungus/'

.PHONY: remote-chungus-boot
remote-chungus-boot:
	./run-and-notify.sh --app-name=Nix -t 0 --icon=nix-snowflake "Chungus boot" -- \
	'nh os boot --build-host chungus --target-host chungus -H chungus ./hosts/chungus/'

.PHONY: hm-dog-chungus-switch
hm-dog-chungus-switch:
	home-manager --flake .#dog@chungus switch

.PHONY: nixos-chungus-boot
nixos-chungus-boot:
	./run-and-notify.sh --app-name=Nix -t 0 --icon=nix-snowflake "Chungus boot" -- \
	sudo -A nixos-rebuild --flake ./hosts/chungus boot

.PHONY: nixos-chungus-build
nixos-chungus-build:
	./run-and-notify.sh --app-name=Nix -t 0 --icon=nix-snowflake "Chungus build" -- \
	nixos-rebuild --flake ./hosts/chungus build

.PHONY: nixos-chungus-switch
nixos-chungus-switch:
	./run-and-notify.sh --app-name=Nix -t 0 --icon=nix-snowflake "Chungus switch" -- \
	sudo -A nixos-rebuild --flake ./hosts/chungus switch

.PHONY: gc
gc:
	nix-collect-garbage -d

.PHONY: nixos-delete-generations
nixos-delete-generations:
	sudo -A nix-env --delete-generations 40d --profile /nix/var/nix/profiles/system
	sudo -A nix-collect-garbage --delete-older-than 40d
	sudo -A /nix/var/nix/profiles/system/bin/switch-to-configuration switch
	nix store optimise

.PHONY: format-nix
format-nix:
	git ls-files -z '*.nix' | xargs -0 -r nixfmt
