module Protocol.UpdateProtocol where

import Contract.Prelude
import Contract.Monad (Contract, liftContractM, liftedE, liftedM)
import Contract.Prelude (Maybe(..), Tuple(..), Unit, bind, discard, mempty, pure, unwrap, ($), (<$>), (<>), (>>=), (>>>))
import Contract.Transaction (TransactionHash, TransactionUnspentOutput, balanceTxWithConstraints, signTransaction
  , submit, _input, awaitTxConfirmed, lookupTxHash, submitTxFromConstraints)
import Protocol.Models (PProtocol)
import Contract.Address
import Contract.Log (logInfo')
import Contract.PlutusData (PlutusData, Redeemer(Redeemer), fromData, toData)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (ValidatorHash)
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Ctl.Internal.Plutus.Types.Transaction (UtxoMap)
import Ctl.Internal.Plutus.Types.Value (Value)
import Ctl.Internal.Types.Datum (Datum(..))
import Data.Array (head)
import Data.Lens (view)
import Protocol.Datum (PProtocolConfig, PProtocolDatum(..), _protocolConstants)
import Protocol.ProtocolScript (protocolValidatorScript, getProtocolValidatorHash)
import Protocol.Redeemer (PProtocolRedeemer(..))
import Contract.Credential (Credential(ScriptCredential))
import Data.Array (head) as Array
import Contract.BalanceTxConstraints
  ( BalanceTxConstraintsBuilder
  , mustSendChangeToAddress
  )
import Ctl.Internal.Types.Transaction
import Shared.Helpers
-- main :: Effect Unit
-- main = example testnetNamiConfig

contract :: PProtocol -> PProtocolConfig -> Contract () Unit
contract protocol protocolConfig = do
  logInfo' "Running Examples.AlwaysSucceeds"
  validator <- protocolValidatorScript protocol
  vhash <- getProtocolValidatorHash protocol
  let scriptAddress = scriptHashAddress vhash Nothing
  utxos <- utxosAt scriptAddress
  protocolUtxo <- getProtocolUtxo protocol utxos

  ownAddress <- liftedM "Failed to get own address" $ Array.head <$> getWalletAddresses
  currentDatum <- liftContractM "Impossible to get Protocol Datum" $ extractDatumFromUTxO protocolUtxo
  let value = extractValueFromUTxO  protocolUtxo
  
  let newDatum = Datum $ toData $ makeDatum currentDatum protocolConfig
  let updateProtocolRedeemer = Redeemer $ toData $ PUpdateProtocolConfig protocolConfig

  let 
      constraints :: Constraints.TxConstraints Void Void
      constraints = 
        Constraints.mustPayToScriptAddress vhash (ScriptCredential vhash) newDatum Constraints.DatumInline value
        <> Constraints.mustSpendScriptOutput (fst protocolUtxo) updateProtocolRedeemer
  
  let 
      lookups :: Lookups.ScriptLookups Void
      lookups = 
        Lookups.validator validator
        <> Lookups.unspentOutputs utxos

  unbalancedTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  let
    addressWithNetworkTag =
      AddressWithNetworkTag
        { address: ownAddress
        , networkId: TestnetId
        }
    balanceTxConstraints :: BalanceTxConstraintsBuilder
    balanceTxConstraints = mustSendChangeToAddress addressWithNetworkTag
  balancedTx <- liftedE $ balanceTxWithConstraints unbalancedTx balanceTxConstraints
  balancedSignedTx <- signTransaction balancedTx
  txId <- submit balancedSignedTx
  awaitTxConfirmed txId

makeDatum ∷ PProtocolDatum -> PProtocolConfig → PProtocolDatum
makeDatum currentDatum protocolConfig  =
  let protocolConstants = view _protocolConstants currentDatum
  in PProtocolDatum { protocolConstants, protocolConfig }

getProtocolUtxo :: PProtocol -> UtxoMap -> Contract () UtxoTuple
getProtocolUtxo protocol utxos = 
  let p = unwrap protocol
  in getUtxoByThreadToken (Tuple (_.protocolCurrency p) (_.protocolTokenName p)) utxos
