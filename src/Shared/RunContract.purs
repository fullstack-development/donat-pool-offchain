module Shared.RunContract where

import Contract.Prelude
import Data.Maybe (maybe)
import Effect.Aff (runAff_)
import Effect.Exception (Error, message, throw)
import Contract.Monad (Contract, runContract)
import Shared.TestnetConfig (mkNetworkWalletConfig)
import Shared.NetworkData (NetworkParams, networkParamsToNetworkWallet)

runContractWithResult :: forall a. (a -> Effect Unit) -> (String -> Effect Unit) -> NetworkParams -> Contract a -> Effect Unit
runContractWithResult onComplete onError networkParams contract = do
  networkWallet <- maybe (throw "Impossible to parse Wallet type") pure $ networkParamsToNetworkWallet networkParams
  networkWalletConfig <- mkNetworkWalletConfig networkWallet
  runAff_ handler $ runContract networkWalletConfig contract
  where
  handler :: Either Error a -> Effect Unit
  handler (Right res) = onComplete res
  handler (Left err) = onError $ message err
