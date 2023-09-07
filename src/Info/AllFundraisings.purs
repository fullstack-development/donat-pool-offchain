module Info.AllFundraisings where

import Contract.Prelude

import Contract.Address (getNetworkId, validatorHashBaseAddress)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.Utxos (utxosAt)
import Data.Map as Map
import Data.Traversable (traverse)
import Ext.Contract.Value (mkCurrencySymbol)
import Fundraising.FundraisingScript (getFundraisingValidatorHash)
import Fundraising.Models (Fundraising(..))
import Info.UserData (FundraisingInfo, mapToFundraisingInfo)
import MintingPolicy.VerTokenMinting as VerToken
import Protocol.UserData (ProtocolData, dataToProtocol)
import Shared.NetworkData (NetworkParams)
import Shared.RunContract (runContractWithResult)
import Shared.Utxo (filterByToken)

runGetAllFundraisings :: (Array FundraisingInfo -> Effect Unit) -> (String -> Effect Unit) -> ProtocolData -> NetworkParams -> Effect Unit
runGetAllFundraisings onComplete onError protocolData networkParams =
  runContractWithResult onComplete onError networkParams (getAllFundraisings protocolData)

getAllFundraisings :: ProtocolData -> Contract (Array FundraisingInfo)
getAllFundraisings protocolData = do
  protocol <- dataToProtocol protocolData
  _ /\ verTokenCs <- mkCurrencySymbol (VerToken.mintingPolicy protocol)
  verTn <- VerToken.verTokenName
  let
    fundraising = Fundraising
      { protocol: protocol
      , verTokenCurrency: verTokenCs
      , verTokenName: verTn
      }

  frValidatorHash <- getFundraisingValidatorHash fundraising
  networkId <- getNetworkId
  frAddress <- liftContractM "Impossible to get Fundraising script address" $ validatorHashBaseAddress networkId frValidatorHash

  fundraisings <- utxosAt frAddress
  frInfos <- traverse mapToFundraisingInfo <<< filterByToken (verTokenCs /\ verTn) $ Map.toUnfoldable fundraisings
  logInfo' $ "Found UTxOs" <> show frInfos
  pure frInfos
