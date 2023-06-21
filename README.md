# DonatPool Offchain project based on [Cardano-Transaction-Lib](https://github.com/Plutonomicon/cardano-transaction-lib/tree/develop/doc)

The repo contains the offchain part for the [DonatPool onchain](https://github.com/fullstack-development/donat-pool-onchain) project. Here you may find transaction builders (including all the offchain checks), simplified frontend (buttons to call the endpoints) and a bunch of unit tests and e2e plutip tests.

You may find the current project working in the PreProduction testnet on the [DonatPool website](https://testnet.donat-pool.io/).

To start working clone the repository, go to the project root and run `nix develop` (make sure to use Nix v2.8 or later) and then build the project with `spago build`.


## DonatPool Project environment 

DonationPool is dependent on the services listed below, so for working in blockchain you need them all installed and run on you host-machine. The services you need:

- Cardano node
- Ogmios
- Kupo 

Also before starting make sure to have files with plutus scripts in the scripts/ folder.

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

2. Kupo

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

- To run kupo using Docker container:

```
docker pull cardanosolutions/kupo:v2.4.0

docker run --network host -d \
  cardanosolutions/kupo:v2.4.0 \
  --ogmios-host 0.0.0.0 \
  --ogmios-port 1337 \
  --since origin \
  --match "*" \
  --workdir . 
```

3. Create new dist for frontend:

Run `sh build.sh` from project root

4. Run server

Before running the server open terminal from the project root and type `npm install` to create node modules (make sure to do it not under `nix develop`). Then run the environment in different terminals, build the project with `spago build` (under `nix develop`) and then create new dists (`sh build.sh` under `nix develop`). You may run the server on port 4008 with `npm run serve` command (under `nix develop` as well). Then go to `localhost:4008` with the Chrome browser and explore.

Note that the current offchain version functionality is available for Chrome browser with installed Nami wallet extension only. To test it with Nami wallet make sure to switch on the PreProduction testnet network (Nami -> Settings -> Network -> Preprod), you also have to lock 5 Ada as collateral (Nami -> Collateral -> Confirm).

### Tests

To run Unit tests type `npm run unit-tests` from the project root (under `nix develop`)
To run Plutip tests type `npm run plutip-tests` from the project root (under `nix develop`)

### Code formatting

```
make format
```