#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

for cmd in curl jq nix-prefetch-url npm; do
  if ! command -v "$cmd" >/dev/null 2>&1; then
    echo "ERROR: missing required command: $cmd" >&2
    exit 1
  fi
done

sed_inplace() {
  if sed --version >/dev/null 2>&1; then
    sed -i "$@"
  else
    sed -i '' "$@"
  fi
}

registry_url="https://registry.npmjs.org/@github%2Fcopilot"
version=$(curl -s "$registry_url" | jq -r '."dist-tags".latest')

if [[ -z $version || $version == "null" ]]; then
  echo "ERROR: unable to determine latest version" >&2
  exit 1
fi

tarball_url="https://registry.npmjs.org/@github/copilot/-/copilot-${version}.tgz"
raw_hash=$(nix-prefetch-url "$tarball_url")
source_hash=$(nix hash to-sri --type sha256 "$raw_hash")

sed_inplace "s/version = \".*\";/version = \"$version\";/" package.nix
sed_inplace "s|hash = \"sha256-[^\"]*\";|hash = \"$source_hash\";|" package.nix

echo "Updated copilot-cli to $version"
