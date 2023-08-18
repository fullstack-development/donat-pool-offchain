module Governance.GovernanceScriptInfo
  ( GovernanceScriptInfo(..)
  , getGovernanceScriptInfo
  )
  where

import Contract.Prelude

import Contract.Address (getNetworkId, validatorHashBaseAddress)
import Contract.Monad (Contract, liftContractM)
import Contract.Transaction (ScriptRef(..), TransactionInput, TransactionOutputWithRefScript, mkTxUnspentOut)
import Contract.TxConstraints (InputWithScriptRef)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Ctl.Internal.Plutus.Types.Address (Address)
import Ctl.Internal.Types.Scripts (Validator, ValidatorHash)
import Ctl.Internal.Types.TokenName (TokenName)
import Data.Map (Map)
import Governance.Datum (GovernanceDatum)
import Governance.GovernanceScript (getGovernanceValidatorHash, governanceTokenName, governanceValidatorScript)
import Protocol.Models (Protocol)
import Shared.Utxo (extractDatumFromUTxO, extractValueFromUTxO, getUtxoByNFT, getUtxoByScriptRef)

data GovernanceScriptInfo = GovernanceScriptInfo {
    govTokenName :: TokenName,
    govValidator :: Validator,
    govValidatorHash :: ValidatorHash,
    govAddress :: Address,
    govUtxos :: Map TransactionInput TransactionOutputWithRefScript,
    govUtxo :: Tuple TransactionInput TransactionOutputWithRefScript,
    govDatum :: GovernanceDatum,
    govValue :: Value.Value,
    govRefScriptUtxo ::  Tuple TransactionInput TransactionOutputWithRefScript,
    govRefScriptInput :: InputWithScriptRef
}

getGovernanceScriptInfo ∷ Protocol → Contract GovernanceScriptInfo
getGovernanceScriptInfo protocol = do
    networkId <- getNetworkId
    govTn <- governanceTokenName
    govValidator <- governanceValidatorScript protocol
    govValidatorHash <- getGovernanceValidatorHash protocol
    govAddress <-
        liftContractM "Impossible to get Governance script address" $ validatorHashBaseAddress networkId govValidatorHash
    govUtxos <- utxosAt govAddress
    govUtxo <- getUtxoByNFT "Governance" ((unwrap protocol).protocolCurrency /\  govTn) govUtxos
    govDatum <- liftContractM "Impossible to get Governance Datum" $ extractDatumFromUTxO govUtxo
    let govValue = extractValueFromUTxO govUtxo
    let govScriptRef = PlutusScriptRef (unwrap govValidator)
    govRefScriptUtxo <- getUtxoByScriptRef "Governance" govScriptRef govUtxos
    let govRefScriptInput = Constraints.RefInput $ mkTxUnspentOut (fst govRefScriptUtxo) (snd govRefScriptUtxo)
    pure $ GovernanceScriptInfo {
        govTokenName: govTn,
        govValidator: govValidator,
        govValidatorHash: govValidatorHash,
        govAddress: govAddress,
        govUtxos: govUtxos,
        govUtxo: govUtxo,
        govDatum: govDatum,
        govValue: govValue,
        govRefScriptUtxo: govRefScriptUtxo,
        govRefScriptInput: govRefScriptInput
    }