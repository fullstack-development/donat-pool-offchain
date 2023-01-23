let script;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  // script = require("Scripts/alwaysMintsPolicy.plutus");
  script = require("Scripts/nftPolicy.plutus");
} else {
  const fs = require("fs");
  const path = require("path");
  script = fs.readFileSync(
    // path.resolve(__dirname, "../../scripts/alwaysMintsPolicy.plutus"),
    path.resolve(__dirname, "../../scripts/nftPolicy.plutus"),
    "utf8"
  );
}

exports.nftPolicy = script;