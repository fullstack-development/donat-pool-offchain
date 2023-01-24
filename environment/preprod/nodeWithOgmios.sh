#!/bin/sh

# Cardano-node with Ogmios bundle

docker run -it -d \
  --name cardano-node-ogmios \
  -p 1337:1337 \
  -v cardano-node-ogmios-db:/db \
  cardanosolutions/cardano-node-ogmios:latest-preprod
