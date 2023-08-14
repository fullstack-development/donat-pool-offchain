module Protocol.ProtocolScriptInfo where

import Contract.Prelude

import Contract.Address (Address, getNetworkId, validatorHashBaseAddress)
import Contract.Monad (Contract, liftContractM)
import Contract.Scripts (MintingPolicy(..))
import Contract.Transaction (ScriptRef(..), TransactionInput, TransactionOutputWithRefScript, mkTxUnspentOut)
import Contract.TxConstraints (InputWithScriptRef)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Ctl.Internal.Types.Scripts (Validator, ValidatorHash)
import Data.Map (Map)
import Effect.Exception (throw)
import Info.AppInfo (getProtocolUtxo)
import MintingPolicy.VerTokenMinting as VerToken
import Protocol.Datum (PProtocolDatum)
import Protocol.Models (Protocol)
import Protocol.ProtocolScript (getProtocolValidatorHash, protocolValidatorScript)
import Shared.Utxo (extractDatumFromUTxO, extractValueFromUTxO, getUtxoByScriptRef)

type References =
  { pScriptRef :: Tuple TransactionInput TransactionOutputWithRefScript
  , pRefScriptInput :: InputWithScriptRef
  , verTokenRef :: Tuple TransactionInput TransactionOutputWithRefScript
  , verTokenInput :: InputWithScriptRef
  }

newtype ProtocolScriptInfo = ProtocolScriptInfo
  { pValidator :: Validator
  , pValidatorHash :: ValidatorHash
  , pAddress :: Address
  , pUtxos :: Map TransactionInput TransactionOutputWithRefScript
  , pUtxo :: Tuple TransactionInput TransactionOutputWithRefScript
  , pDatum :: PProtocolDatum
  , pValue :: Value.Value
  , references :: References
  }

getProtocolScriptInfo :: Protocol -> Contract ProtocolScriptInfo
getProtocolScriptInfo protocol = do
  protocolValidator <- protocolValidatorScript protocol
  protocolValidatorHash <- getProtocolValidatorHash protocol
  networkId <- getNetworkId
  protocolAddress <-
    liftContractM "Impossible to get Protocol script address" $ validatorHashBaseAddress networkId protocolValidatorHash
  protocolUtxos <- utxosAt protocolAddress
  protocolUtxo <- getProtocolUtxo protocol protocolUtxos
  currentDatum <- liftContractM "Impossible to get Protocol Datum" $ extractDatumFromUTxO protocolUtxo
  let value = extractValueFromUTxO protocolUtxo

  let scriptRef = PlutusScriptRef (unwrap protocolValidator)
  refScriptUtxo <- getUtxoByScriptRef "Protocol" scriptRef protocolUtxos
  let refScriptInput = Constraints.RefInput $ mkTxUnspentOut (fst refScriptUtxo) (snd refScriptUtxo)

  verTokenMpWrapped <- VerToken.mintingPolicy protocol
  policyRef <- case verTokenMpWrapped of
    PlutusMintingPolicy policy -> pure $ PlutusScriptRef policy
    _ -> liftEffect $ throw "Unexpected Minting Policy script type"
  policyRefUtxo <- getUtxoByScriptRef "VerTokenPolicy" policyRef protocolUtxos
  let policyRefInput = Constraints.RefInput $ mkTxUnspentOut (fst policyRefUtxo) (snd policyRefUtxo)
  let
    refs =
      { pScriptRef: refScriptUtxo
      , pRefScriptInput: refScriptInput
      , verTokenRef: policyRefUtxo
      , verTokenInput: policyRefInput
      }
  pure $ ProtocolScriptInfo
    { pValidator: protocolValidator
    , pValidatorHash: protocolValidatorHash
    , pAddress: protocolAddress
    , pUtxos: protocolUtxos
    , pUtxo: protocolUtxo
    , pDatum: currentDatum
    , pValue: value
    , references: refs
    }
