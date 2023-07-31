let script;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  script = require("Scripts/governancePolicy.plutus");
} else {
  const fs = require("fs");
  const path = require("path");
  script = fs.readFileSync(
    path.resolve("scripts/governancePolicy.plutus"),
    "utf8"
  );
}

exports.governancePolicy = script;