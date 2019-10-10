#!/bin/sh

set -ex

mkdir -p /build
cp -R /mnt/* /build
cd /build

stack --no-terminal install --flag "rpw:rpw_dist" --local-bin-path /mnt
