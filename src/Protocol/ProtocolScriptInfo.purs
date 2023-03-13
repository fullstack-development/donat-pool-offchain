module Protocol.ProtocolScriptInfo where

import Contract.Prelude

import Contract.Address (Address, validatorHashBaseAddress)
import Contract.Config (NetworkId(TestnetId))
import Contract.Monad (Contract, liftContractM)
import Contract.Transaction (TransactionInput, TransactionOutputWithRefScript)
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Ctl.Internal.Types.Scripts (Validator, ValidatorHash)
import Data.Map (Map)
import Info.Protocol (getProtocolUtxo)
import Protocol.Datum (PProtocolDatum)
import Protocol.Models (Protocol)
import Protocol.ProtocolScript (getProtocolValidatorHash, protocolValidatorScript)
import Shared.Helpers (extractDatumFromUTxO, extractValueFromUTxO)

newtype ProtocolScriptInfo = ProtocolScriptInfo
  { pValidator :: Validator
  , pValidatorHash :: ValidatorHash
  , pAddress :: Address
  , pUtxos :: Map TransactionInput TransactionOutputWithRefScript
  , pUtxo :: (Tuple TransactionInput TransactionOutputWithRefScript)
  , pDatum :: PProtocolDatum
  , pValue :: Value.Value
  }

getProtocolScriptInfo :: Protocol -> Contract () ProtocolScriptInfo
getProtocolScriptInfo protocol = do
  protocolValidator <- protocolValidatorScript protocol
  protocolValidatorHash <- getProtocolValidatorHash protocol
  protocolAddress <-
    liftContractM "Impossible to get Protocol script address" $ validatorHashBaseAddress TestnetId protocolValidatorHash
  utxos <- utxosAt protocolAddress
  protocolUtxo <- getProtocolUtxo protocol utxos
  currentDatum <- liftContractM "Impossible to get Protocol Datum" $ extractDatumFromUTxO protocolUtxo
  let value = extractValueFromUTxO protocolUtxo
  pure $ ProtocolScriptInfo
    { pValidator: protocolValidator
    , pValidatorHash: protocolValidatorHash
    , pAddress: protocolAddress
    , pUtxos: utxos
    , pUtxo: protocolUtxo
    , pDatum: currentDatum
    , pValue: value
    }
