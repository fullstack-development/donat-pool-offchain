module Protocol.UpdateProtocol where

import Contract.Address (scriptHashAddress)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData (PlutusData, Redeemer(Redeemer), fromData, toData)
import Contract.Prelude (Maybe(..), Unit, bind, discard, ($), (<$>), (<>))
import Contract.Scripts (ValidatorHash)
import Ctl.Internal.Types.Datum (Datum (..))
import Protocol.Datum (PProtocolConfig, PProtocolDatum(..), _protocolConstants)
import Protocol.Models
import Protocol.Redeemer (PProtocolRedeemer(..))
import Protocol.ProtocolScript (protocolValidatorScript)

import Contract.ScriptLookups as Lookups
import Contract.Transaction (TransactionHash, _input, _output, awaitTxConfirmed, lookupTxHash, outputDatumDatum, submitTxFromConstraints)
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Ctl.Internal.Plutus.Types.Transaction (_datum)
import Data.Array (head)
import Data.Lens (view)
import Ctl.Internal.Plutus.Types.Transaction as Tx
-- main :: Effect Unit
-- main = example testnetNamiConfig

-- contract :: Contract () Unit
-- contract = do
--   logInfo' "Running Examples.AlwaysSucceeds"
--   validator <- alwaysSucceedsScript
--   let vhash = validatorHash validator
--   logInfo' "Attempt to lock value"
--   txId <- payToAlwaysSucceeds vhash
--   awaitTxConfirmed txId
--   logInfo' "Tx submitted successfully, Try to spend locked values"
--   spendFromAlwaysSucceeds vhash validator txId

-- example :: ConfigParams () -> Effect Unit
-- example cfg = launchAff_ do
--   runContract cfg contract

-- payToAlwaysSucceeds :: ValidatorHash -> Contract () TransactionHash
-- payToAlwaysSucceeds vhash = do
--   -- Send to own stake credential. This is used to test mustPayToScriptAddress.
--   mbScriptPaymentKeyHash <- join <<< head <$> ownPaymentPubKeyHash
--   let
--     constraints :: TxConstraints Unit Unit
--     constraints =
--       case mbScriptPaymentKeyHash of
--         Nothing ->
--           Constraints.mustPayToScript vhash unitDatum
--             Constraints.DatumWitness
--             $ Value.lovelaceValueOf
--             $ BigInt.fromInt 2_000_000
--         Just stakeKeyHash ->
--           Constraints.mustPayToScriptAddress vhash
--             (PubKeyCredential $ unwrap stakeKeyHash)
--             unitDatum
--             Constraints.DatumWitness
--             $ Value.lovelaceValueOf
--             $ BigInt.fromInt 2_000_000

--     lookups :: Lookups.ScriptLookups PlutusData
--     lookups = mempty

--   submitTxFromConstraints lookups constraints

spendProtocol
  :: PProtocol 
  -> PProtocolConfig
  -> ValidatorHash
  -> TransactionHash
  -> Contract () Unit
spendProtocol protocol protocolConfig vhash txId = do
  let scriptAddress = scriptHashAddress vhash Nothing
  utxos <- utxosAt scriptAddress
  txInput <- liftContractM "no locked output at address"
    (view _input <$> head (lookupTxHash txId utxos))
  txOutputWithRefScript <- liftContractM "no locked output at address"
    (view _output <$> head (lookupTxHash txId utxos))
  Datum pdata <- liftContractM "no datum in protocol utxo"  
    (outputDatumDatum $ view _datum $ view Tx._output txOutputWithRefScript)
  (currentDatum :: PProtocolDatum) <- liftContractM "can't decode datum" $ fromData pdata
  let protocolConstants = view _protocolConstants currentDatum
  let newDatum = PProtocolDatum { protocolConstants, protocolConfig } 
  let uddateProtocolRedeemer = Redeemer $ toData $ PUpdateProtocolConfig protocolConfig
  let newPlutusDatum = Datum $ toData newDatum
  protocolValidator <- protocolValidatorScript protocol
  let
    lookups :: Lookups.ScriptLookups PlutusData
    lookups = Lookups.validator protocolValidator
      <> Lookups.unspentOutputs utxos

    constraints :: TxConstraints Unit Unit
    constraints =
      Constraints.mustSpendScriptOutput txInput uddateProtocolRedeemer
        <> Constraints.mustIncludeDatum newPlutusDatum
  spendTxId <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed spendTxId
  logInfo' "Successfully spent locked values."
