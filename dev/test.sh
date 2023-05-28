#!/bin/sh

set -e

cd "$(dirname "$0")/.."

nix run github:NixOS/nixpkgs#elmPackages.elm-test -- "$@"