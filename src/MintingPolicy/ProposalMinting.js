let script;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  script = require("Scripts/proposalPolicy.plutus");
} else {
  const fs = require("fs");
  const path = require("path");
  script = fs.readFileSync(
    path.resolve("scripts/proposalPolicy.plutus"),
    "utf8"
  );
}

exports.proposalPolicy = script;
