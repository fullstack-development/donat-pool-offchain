# DonatPool Offchain project based on [Cardano-Transaction-Lib](https://github.com/Plutonomicon/cardano-transaction-lib/tree/develop/doc)

To start working run `nix develop` (make sure to use Nix v2.8 or later) and then build the project with `spago build`.

Please also see the CTL related sources:

- [Documentation](https://github.com/Plutonomicon/cardano-transaction-lib/tree/develop/doc)

- [Generated docs](https://plutonomicon.github.io/cardano-transaction-lib/)

- [Discord server](https://discord.gg/JhbexnV9Pc)


## DonationPool Project environment 

DonationPool is dependent on the services listed below, so for working in blockchain you need them all installed and run on you host-machine. The services you need:

- Cardano node
- Ogmios
- Ogmios-Datum-Cache
- Kupo 

### Running in Pre-Production testnet 

1. Cardano-node with Ogmios bundle

For simplicity you may use Cardano node with Ogmios bundle for the pre-production testnet for docker. Call the following command to download image from dockerhub and run services:
```
docker run -it -d \
  --name cardano-node-ogmios \
  -p 1337:1337 \
  -v cardano-node-ogmios-db:/db \
  cardanosolutions/cardano-node-ogmios:v5.6.0_1.35.5-preprod
  ```
You may call the same command with `bash ./environment/preprod/nodeWithOgmios.sh`.

2. Ogmios-Datum-Cache

After building project dependencies you have built binary file for Ogmios-Datum-Cache. You may find in in the nix/store/.. folder. Alternatively you may clone the [Ogmios-Datum-Cache](https://github.com/mlabs-haskell/ogmios-datum-cache) repo and build the binary file by yourself with the `cabal install` command (make sure to call `nix-shell` or `nix develop` first). After you have the binary for Ogmios-Datum-Cache you may run the server with the following command:

```
ogmios-datum-cache \
  --db-port 5432 \
  --db-host localhost \
  --db-user username \
  --db-password "password" \
  --db-name dbname \
  --server-port 9999 \
  --server-api "" \
  --ogmios-address localhost \
  --ogmios-port 1337 \
  --from-tip --use-latest \
  --log-level debug
```

Notes: 
- Before starting the service make sure that you have created a Database for it (use PostgreSQL). Also there may be different values for ogmios-address and ogmios-port.
- Alternatively may call `bash ./environment/preprod/odc.sh` from the project root to run the ODC service for current project configuration.

3. Kupo

After building project dependencies you have built binary file for Kupo as well. You may either use the binary file from /nix/store/.. folder or build the [service](https://github.com/CardanoSolutions/kupo) by yourself.
Run the service with the following command:
```
kupo \
  --ogmios-host localhost \
  --ogmios-port 1337 \
  --since origin \
  --match "*" \
  --workdir pathToKupoDb
```

Notes: 
- Alternatively may run `bash ./environment/preprod/kupo.sh` from the project root to run the Kupo service for current project configuration.
- If it's the first time you run Kupo, you also better to add `--defer-db-indexes` flag to speed up the initial indexer synchronization.
- You may specify the `since` argument with different value (please see the Kupo [manual](https://cardanosolutions.github.io/kupo/)) 
- Instead of connecting via Ogmios you may also connect to Cardano-Node directly. In this case replace `--ogmios-host` and `--ogmios-port` arguments with

```
  --node-socket some-folder/cardano-node/node.socket \
  --node-config some-folder/cardano-node/config.json \
```
