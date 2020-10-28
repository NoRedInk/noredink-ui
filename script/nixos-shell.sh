#!/usr/bin/env bash
set -euo pipefail

VOLUME_NAME="noredink-ui-nixos-shell-nix"

if ! docker volume ls | grep -q "$VOLUME_NAME"; then
  docker volume create "$VOLUME_NAME"
fi

docker run \
  --interactive \
  --tty \
  --mount "type=bind,source=$(pwd),target=/app" \
  --mount "type=volume,source=$VOLUME_NAME,target=/nix" \
  --workdir /app \
  lnl7/nix:latest \
  nix-shell
