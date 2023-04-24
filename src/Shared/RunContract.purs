module Shared.RunContract where

import Contract.Prelude
import Effect.Exception (Error, message)
import Effect.Aff (runAff_)
import Shared.TestnetConfig (mkTestnetNamiConfig)
import Contract.Monad (Contract, runContract)

runContractWithUnitResult :: (Unit -> Effect Unit) -> (String -> Effect Unit) -> Contract Unit -> Effect Unit
runContractWithUnitResult onComplete onError contract = do
  testnetNamiConfig <- mkTestnetNamiConfig
  runAff_ handler $ runContract testnetNamiConfig contract
  where
  handler :: Either Error Unit -> Effect Unit
  handler (Right _) = onComplete unit
  handler (Left err) = onError $ message err

