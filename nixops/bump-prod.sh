#!/bin/env bash
set -euo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")"

[ -z "$(git status --porcelain)" ] || { echo "Push first"; exit 1; }

rev=$(git rev-parse HEAD)
sha256=$(nix-prefetch-git $(git config --get remote.origin.url) --rev $(git rev-parse HEAD) | jq .sha256 -r)
nixops set-args -d y --arg y-version "{ rev = \"$rev\"; sha256 = \"$sha256\"; }"
