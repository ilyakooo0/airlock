#!/bin/sh

set -e

cd "$(dirname "$0")/../example"

nix run nixpkgs#elmPackages.elm-live -- src/Vanilla.elm --start-page=index.html -- --output=elm.js --debug "$@"
