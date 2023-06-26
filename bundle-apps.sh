#!/bin/sh

spago bundle-app --main CLI.StartProtocol.Main --to dist/start-protocol.js
spago bundle-app --main CLI.UpdateProtocol.Main --to dist/update-protocol.js
spago bundle-app --main CLI.CloseProtocol.Main --to dist/close-protocol.js
