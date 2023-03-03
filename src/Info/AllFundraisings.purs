module Info.AllFundraisings where

import Contract.Prelude

import Contract.Address (validatorHashBaseAddress)
import Contract.Config (testnetNamiConfig, NetworkId(TestnetId))
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, runContract)
import Contract.Utxos (utxosAt)
import Data.Map as Map
import Effect.Aff (runAff_)
import Effect.Exception (Error, message)
import Fundraising.FundraisingScript (getFundraisingValidatorHash)
import Fundraising.Models (Fundraising(..))
import MintingPolicy.VerTokenMinting as VerToken
import Protocol.Models (Protocol)
import Shared.Helpers as Helpers
import Info.UserData (FundraisingInfo, mapToFundraisingInfo)
import Data.Array (mapMaybe)

runGetAllFundraisings :: (Array FundraisingInfo -> Effect Unit) -> (String -> Effect Unit) -> Protocol -> Effect Unit
runGetAllFundraisings onComplete onError protocol = runAff_ handler $
  runContract testnetNamiConfig (getAllFundraisings protocol)
  where
  handler :: Either Error (Array FundraisingInfo) -> Effect Unit
  handler (Right response) = onComplete response
  handler (Left err) = onError $ message err

getAllFundraisings :: Protocol -> Contract () (Array FundraisingInfo)
getAllFundraisings protocol = do
  _ /\ verTokenCs <- Helpers.mkCurrencySymbol (VerToken.mintingPolicy protocol)
  verTn <- VerToken.verTokenName
  let
    fundraising = Fundraising
      { protocol: protocol
      , verTokenCurrency: verTokenCs
      , verTokenName: verTn
      }

  frValidatorHash <- getFundraisingValidatorHash fundraising

  frAddress <- liftContractM "Impossible to get Fundraising script address" $ validatorHashBaseAddress TestnetId frValidatorHash

  fundraisings <- utxosAt frAddress
  let frInfos = mapMaybe mapToFundraisingInfo (Map.toUnfoldable fundraisings)
  logInfo' $ "Found UTxOs" <> show frInfos
  pure frInfos
