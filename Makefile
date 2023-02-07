SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c

ps-sources := $$(fd -epurs)
ps-entrypoint := Scaffold.Main
e2e-entrypoint := Scaffold.Test.E2E.Serve
ps-bundle = spago bundle-module -m ${ps-entrypoint} --to output/index.js

run-dev:
	@${ps-bundle} && BROWSER_RUNTIME=1 webpack serve --config webpack.offchain.config.js --progress

e2e-serve:
	spago bundle-module -m ${e2e-entrypoint} --to output.js
	BROWSER_RUNTIME=1 webpack serve --config webpack.offchain.config.js --progress

run-build:
	@${ps-bundle} && BROWSER_RUNTIME=1 webpack --config webpack.offchain.config.js --mode=production --progress

check-format:
	@purs-tidy check ${ps-sources}

format:
	@purs-tidy format-in-place ${ps-sources}
