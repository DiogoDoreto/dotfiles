.PHONY: hm-dog-lapdog-switch
hm-dog-lapdog-switch:
	home-manager --flake . switch

.PHONY: hm-news
hm-news:
	home-manager --flake . news

.PHONY: nixos-lapdog-boot
nixos-lapdog-boot:
	sudo -A nixos-rebuild --flake . boot

.PHONY: nixos-lapdog-switch
nixos-lapdog-switch:
	sudo -A nixos-rebuild --flake . switch
