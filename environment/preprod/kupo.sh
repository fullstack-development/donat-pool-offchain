#!/bin/sh

# Kupo

ogmiosPort=1337
ogmiosHost=localhost
sincePoint=origin
workDir=./kupo_db

/nix/store/wg6wk7d7ficwx860cglassp1f3vlyyxd-kupo-exe-kupo-x86_64-unknown-linux-musl-2.2.0/bin/kupo \
  --ogmios-host $ogmiosHost \
  --ogmios-port $ogmiosPort \
  --since $sincePoint \
  --match "*" \
  --workdir $workDir

# Add --defer-db-indexes flag if it's the first time you run Kupo
