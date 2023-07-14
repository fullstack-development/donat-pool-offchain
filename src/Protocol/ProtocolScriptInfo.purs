module Protocol.ProtocolScriptInfo where

import Contract.Prelude

import Contract.Address (Address, getNetworkId, validatorHashBaseAddress)
import Contract.Monad (Contract, liftContractM)
import Contract.Transaction (ScriptRef(..), TransactionInput, TransactionOutputWithRefScript, mkTxUnspentOut)
import Contract.TxConstraints (InputWithScriptRef)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Ctl.Internal.Types.Scripts (Validator, ValidatorHash)
import Data.Map (Map)
import Info.AppInfo (getProtocolUtxo)
import Protocol.Datum (PProtocolDatum)
import Protocol.Models (Protocol)
import Protocol.ProtocolScript (getProtocolValidatorHash, protocolValidatorScript)
import Shared.Utxo (extractDatumFromUTxO, extractValueFromUTxO, getUtxoByScriptRef)

newtype ProtocolScriptInfo = ProtocolScriptInfo
  { pValidator :: Validator
  , pValidatorHash :: ValidatorHash
  , pAddress :: Address
  , pUtxos :: Map TransactionInput TransactionOutputWithRefScript
  , pUtxo :: (Tuple TransactionInput TransactionOutputWithRefScript)
  , pDatum :: PProtocolDatum
  , pValue :: Value.Value
  , pScriptRef :: (Tuple TransactionInput TransactionOutputWithRefScript)
  , pRefScriptInput :: InputWithScriptRef
  }

getProtocolScriptInfo :: Protocol -> Contract ProtocolScriptInfo
getProtocolScriptInfo protocol = do
  protocolValidator <- protocolValidatorScript protocol
  protocolValidatorHash <- getProtocolValidatorHash protocol
  networkId <- getNetworkId
  protocolAddress <-
    liftContractM "Impossible to get Protocol script address" $ validatorHashBaseAddress networkId protocolValidatorHash
  utxos <- utxosAt protocolAddress
  protocolUtxo <- getProtocolUtxo protocol utxos
  currentDatum <- liftContractM "Impossible to get Protocol Datum" $ extractDatumFromUTxO protocolUtxo
  let value = extractValueFromUTxO protocolUtxo
  
  let scriptRef = PlutusScriptRef (unwrap protocolValidator)
  refScriptUtxo <- getUtxoByScriptRef "Protocol" scriptRef utxos
  let refScriptInput = Constraints.RefInput $ mkTxUnspentOut (fst refScriptUtxo) (snd refScriptUtxo)

  pure $ ProtocolScriptInfo
    { pValidator: protocolValidator
    , pValidatorHash: protocolValidatorHash
    , pAddress: protocolAddress
    , pUtxos: utxos
    , pUtxo: protocolUtxo
    , pDatum: currentDatum
    , pValue: value
    , pScriptRef: refScriptUtxo
    , pRefScriptInput: refScriptInput
    }
