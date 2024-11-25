#!/bin/bash

export VERSION=$(date +'%-y.%-m.%-d')
export COMMIT=$(git rev-parse --short HEAD)

# package.json
yarn files:set-version
PACKAGE=$(jq '.productName="Zoo Modeling App (Nightly)" | .name="zoo-modeling-app-nightly"' package.json --indent 2)
echo "$PACKAGE" > package.json

# electron-builder.yml
yq -i '.publish[0].url = "https://dl.zoo.dev/releases/modeling-app/nightly"' electron-builder.yml
yq -i '.appId = "dev.zoo.modeling-app-nightly"' electron-builder.yml

# Release notes
echo "Nightly build $VERSION (commit $COMMIT)" > release-notes.md

# icons
cp assets/icon-nightly.png assets/icon.png
cp assets/icon-nightly.ico assets/icon.ico
