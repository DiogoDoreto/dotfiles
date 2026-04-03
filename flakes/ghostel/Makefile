# Makefile for flakes/ghostel
# Helper targets for maintaining the ghostel flake.
# Run `make help` to list available targets.

FLAKE_DIR := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

# Configurable: override on the command line
GHOSTEL_REPO ?= dakra/ghostel
GHOSTTY_REPO ?= ghostty-org/ghostty

.PHONY: help build lock show-ghostel-rev show-ghostty-rev ghostty-src-hash \
        show-ghostty-zon update-ghostty-deps fetch-dep-hash

## help: Show this help message
help:
	@echo "ghostel flake maintenance targets"
	@echo ""
	@sed -n 's/^## //p' $(MAKEFILE_LIST) | column -t -s ':' | sed -e 's/^/  /'

## build: Build the ghostel-package derivation (compiles libghostty-vt + module)
build:
	nix build $(FLAKE_DIR)#ghostel-package

## build-module: Build only the ghostel-module.so derivation
build-module:
	nix build $(FLAKE_DIR)#ghostel-module

## build-libvt: Build only libghostty-vt (useful for testing the Zig build chain)
build-libvt:
	nix build $(FLAKE_DIR)#libghostty-vt

## lock: Refresh flake.lock (update nixpkgs + flake-utils pins)
lock:
	nix flake update $(FLAKE_DIR)

## show-ghostel-rev: Show the latest commit on ghostel main branch
show-ghostel-rev:
	@echo "Latest ghostel commit:"
	@curl -s "https://api.github.com/repos/$(GHOSTEL_REPO)/commits/main" \
	  | grep '"sha"' | head -1 | sed 's/.*"\([0-9a-f]\{40\}\)".*/\1/'

## show-ghostty-rev: Show the ghostty submodule commit for a given ghostel rev
## Usage: make show-ghostty-rev GHOSTEL_REV=<commit>
show-ghostty-rev:
ifndef GHOSTEL_REV
	$(error GHOSTEL_REV is required. Usage: make show-ghostty-rev GHOSTEL_REV=<commit>)
endif
	@echo "ghostty submodule commit for ghostel $(GHOSTEL_REV):"
	@curl -s "https://api.github.com/repos/$(GHOSTEL_REPO)/contents/vendor/ghostty?ref=$(GHOSTEL_REV)" \
	  | grep '"sha"' | head -1 | sed 's/.*"\([0-9a-f]\{40\}\)".*/\1/'

## ghostty-src-hash: Get the nix hash for a ghostty source tarball
## Usage: make ghostty-src-hash GHOSTTY_REV=<commit>
ghostty-src-hash:
ifndef GHOSTTY_REV
	$(error GHOSTTY_REV is required. Usage: make ghostty-src-hash GHOSTTY_REV=<commit>)
endif
	@echo "Fetching nix hash for ghostty@$(GHOSTTY_REV)..."
	@nix flake prefetch "github:$(GHOSTTY_REPO)/$(GHOSTTY_REV)" 2>&1 | grep "hash '"

## ghostel-src-hash: Get the nix hash for a ghostel source tarball
## Usage: make ghostel-src-hash GHOSTEL_REV=<commit>
ghostel-src-hash:
ifndef GHOSTEL_REV
	$(error GHOSTEL_REV is required. Usage: make ghostel-src-hash GHOSTEL_REV=<commit>)
endif
	@echo "Fetching nix hash for ghostel@$(GHOSTEL_REV)..."
	@nix flake prefetch "github:$(GHOSTEL_REPO)/$(GHOSTEL_REV)" 2>&1 | grep "hash '"

## show-ghostty-zon: Show the external deps in a ghostty commit's build.zig.zon
## Usage: make show-ghostty-zon GHOSTTY_REV=<commit>
show-ghostty-zon:
ifndef GHOSTTY_REV
	$(error GHOSTTY_REV is required. Usage: make show-ghostty-zon GHOSTTY_REV=<commit>)
endif
	@echo "External deps in ghostty@$(GHOSTTY_REV) build.zig.zon (url entries only):"
	@curl -s "https://raw.githubusercontent.com/$(GHOSTTY_REPO)/$(GHOSTTY_REV)/build.zig.zon" \
	  | grep -v 'path\s*=' | grep 'url\s*=' | sed 's/.*"\(.*\)".*/  \1/'

## fetch-dep-hash: Fetch the nix sha256 hash for a dependency URL
## Usage: make fetch-dep-hash DEP_URL=<url>
fetch-dep-hash:
ifndef DEP_URL
	$(error DEP_URL is required. Usage: make fetch-dep-hash DEP_URL=<url>)
endif
	@nix store prefetch-file "$(DEP_URL)" --hash-type sha256 2>&1 | grep "hash '"

## update-ghostty-deps: Guided helper for updating ghostty-deps.nix entries
## Usage: make update-ghostty-deps GHOSTTY_REV=<new-ghostty-commit>
##
## This compares the new build.zig.zon against ghostty-deps.nix and prints
## hashes for any URLs that appear new or changed. Edit ghostty-deps.nix manually
## using the output.
update-ghostty-deps:
ifndef GHOSTTY_REV
	$(error GHOSTTY_REV is required. Usage: make update-ghostty-deps GHOSTTY_REV=<commit>)
endif
	@echo "==> Fetching build.zig.zon for ghostty@$(GHOSTTY_REV)..."
	@echo ""
	@echo "External URLs in new build.zig.zon:"
	@curl -s "https://raw.githubusercontent.com/$(GHOSTTY_REPO)/$(GHOSTTY_REV)/build.zig.zon" \
	  | grep -v 'path\s*=' | grep 'url\s*=' | sed 's/.*"\(.*\)".*/\1/' \
	  | while read url; do \
	      echo ""; \
	      echo "  URL: $$url"; \
	      nix store prefetch-file "$$url" --hash-type sha256 2>&1 | grep "hash '" | sed 's/.*hash /  nix hash:/'; \
	    done
	@echo ""
	@echo "==> Compare against current ghostty-deps.nix and update changed entries."
	@echo "    Remember to also update the linkFarm 'name' entries (zig hashes from .zon)."
