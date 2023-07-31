module CLI.MintGovernanceTokens.Main (main) where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Governance.MintGovernanceTokens (runMintGovernanceTokens)

main :: Effect Unit
main = launchAff_ runMintGovernanceTokens
