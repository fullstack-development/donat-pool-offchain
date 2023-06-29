#!/bin/sh

spago bundle-module -m Scaffold.Main --to output.js
npm run build
