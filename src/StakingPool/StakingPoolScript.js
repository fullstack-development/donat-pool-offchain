let script;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  script = require("Scripts/stakingPool.plutus");
} else {
  const fs = require("fs");
  const path = require("path");
  script = fs.readFileSync(
    path.resolve(__dirname, "../../scripts/stakingPool.plutus"),
    "utf8"
  );
}

exports.stakingPoolValidator = script;