module Info.AllFundraisings where

import Contract.Prelude

import Contract.Address (validatorHashBaseAddress)
import Contract.Config (NetworkId(TestnetId))
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, runContract)
import Contract.Utxos (utxosAt)
import Data.Array (mapMaybe)
import Data.Map as Map
import Effect.Aff (runAff_)
import Effect.Exception (Error, message)
import Fundraising.FundraisingScript (getFundraisingValidatorHash)
import Fundraising.Models (Fundraising(..))
import Info.UserData (FundraisingInfo, mapToFundraisingInfo)
import MintingPolicy.VerTokenMinting as VerToken
import Protocol.UserData (ProtocolData, dataToProtocol)
import Shared.Helpers as Helpers
import Shared.TestnetConfig (mkTestnetNamiConfig)

runGetAllFundraisings :: (Array FundraisingInfo -> Effect Unit) -> (String -> Effect Unit) -> ProtocolData -> Effect Unit
runGetAllFundraisings onComplete onError protocolData = do
  testnetNamiConfig <- mkTestnetNamiConfig
  runAff_ handler $ runContract testnetNamiConfig (getAllFundraisings protocolData)
  where
  handler :: Either Error (Array FundraisingInfo) -> Effect Unit
  handler (Right response) = onComplete response
  handler (Left err) = onError $ message err

getAllFundraisings :: ProtocolData -> Contract (Array FundraisingInfo)
getAllFundraisings protocolData = do
  protocol <- dataToProtocol protocolData
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
