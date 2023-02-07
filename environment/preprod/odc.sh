#!/bin/sh

# Ogmios-datum-cache

dbPort=5432
dbHost=localhost
dbUser=user
dbPassword="1234"
dbName=odc
ogmiosAddress=localhost
ogmiosPort=1337

/nix/store/lkzcbbkva0c3d0n9k0cmjwcrhqwq2994-ogmios-datum-cache-0.1.0.0/bin/ogmios-datum-cache \
  --db-port $dbPort \
  --db-host $dbHost \
  --db-user $dbUser \
  --db-password $dbPassword \
  --db-name $dbName \
  --server-port 9999 \
  --server-api "" \
  --ogmios-address $ogmiosAddress \
  --ogmios-port $ogmiosPort \
  --from-tip --use-latest \
  --log-level debug
