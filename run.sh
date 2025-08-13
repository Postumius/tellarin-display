#! /usr/bin/env nix-shell
#! nix-shell -i bash -p bash

set -e

elm make src/Display.elm --output elm-compiled/display.js
elm make src/Controls.elm --output elm-compiled/controls.js
electron .
