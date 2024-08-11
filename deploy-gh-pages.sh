#!/bin/env bash
set -euxo pipefail
COMMIT=$(git rev-parse HEAD)
make build
rm -rf /tmp/yoneda-viz
cp -r dist/. /tmp/yoneda-viz
git checkout gh-pages
rm -rf ./*
cp -r /tmp/yoneda-viz/. .
git add .
git commit -m "Deploy ${COMMIT}"
