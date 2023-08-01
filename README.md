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

1. The project runtime dependencies are settled in the docker-compose file, so to run Cardano node, Ogmios and Kupo just type 

```
docker-compose up -d
```
in terminal opened from the project root

2. Create new dist for frontend:

Run `sh build.sh` from project root

3. Run server

First make sure to run the runtime dependencies. Than open terminal from the project root and type `npm install` to create node modules (make sure to do it not under `nix develop`). Than build the project with `spago build` (under `nix develop`) and then create new dists (`sh build.sh` under `nix develop`). You may run the server on port 4008 with `npm run serve` command (under `nix develop` as well). Then go to `localhost:4008` with the Chrome browser and explore.

Note that the current PreProduction testnet offchain version functionality is available for Chrome browser with installed Nami, Lode, Eternl or Flint light wallet extensions. To test it make sure to switch on the PreProduction testnet network in light wallet extension (i.e. for Nami: Nami -> Settings -> Network -> Preprod), you also have to lock 5 Ada as collateral (i.e. for Nami: Nami -> Collateral -> Confirm).

### Tests

To run Unit tests type `npm run unit-tests` from the project root (under `nix develop`)
To run Plutip tests type `npm run plutip-tests` from the project root (under `nix develop`)

### Code formatting

```
make format
```


### TO run from CLI:

1. Start `ogmios + cardano node` docker container and `kupo`.

2. Enter `cardano-node-ogmios` container and make payment and stake keys:

```
cardano-cli address key-gen  --verification-key-file payment.vkey --signing-key-file payment.skey
cardano-cli stake-address key-gen --verification-key-file stake.vkey --signing-key-file stake.skey
```

3. Copy private keys to `wallet` directory in the project. On the first start contract will fail, because no utxo on the new wallet. Use wallet address from logs to receive TADA from faucet or another wallet.

4. Deploy automatic scripts:
- Remove old `node-modules` and run `npm i`
- Run `nix develop`
- Run `sh bundle-apps.sh`, it will create `.js` files in `dist` directory
- To start Protocol run: `npm run start-protocol`. In result of this script, protocol token will be written to the `dist/protocol.local.conf` file.
- To update Protocol edit `conf/config.local.conf` with new values, then run: `npm run update-protocol`.
- To close Protocol run: `npm run close-protocol`
