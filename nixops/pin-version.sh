#!/bin/env bash

cd "$(dirname "${BASH_SOURCE[0]}")"

[ -z "$(git status --porcelain)" ] || { echo "Push first"; exit 1; }

rev=$(git rev-parse HEAD)
sha256=$(nix-prefetch-git $(git config --get remote.origin.url) --rev $(git rev-parse HEAD) | jq .sha256 -r)
echo "{ rev = \"$rev\"; sha256 = \"$sha256\"; }" > ./y-version.nix
