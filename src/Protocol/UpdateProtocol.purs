module Protocol.UpdateProtocol where

import Contract.Monad
import Contract.Prelude
import Contract.Transaction
import Ctl.Internal.Plutus.Types.CurrencySymbol
import Protocol.Models
import Contract.Address (scriptHashAddress)
import Contract.Log (logInfo')
import Contract.PlutusData (PlutusData, Redeemer(Redeemer), fromData, toData)
import Contract.Prelude (Maybe(..), Unit, bind, discard, ($), (<$>), (<>), (<<<))
import Contract.ScriptLookups as Lookups
import Contract.Scripts (ValidatorHash, validatorHash)
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Ctl.Internal.Plutus.Types.Transaction (UtxoMap)
import Ctl.Internal.Plutus.Types.Transaction (_datum, _amount, _output)
import Ctl.Internal.Plutus.Types.Transaction as Tx
import Ctl.Internal.Plutus.Types.Value (Value, valueOf)
import Ctl.Internal.Types.Datum (Datum(..))
import Ctl.Internal.Types.TokenName (TokenName, adaToken, mkTokenName)
import Data.Array (filter)
import Data.Array (head)
import Data.BigInt as BigInt
import Data.Lens (view)
import Data.Map (Map, toUnfoldable)
import Protocol.Datum (PProtocolConfig, PProtocolDatum(..), _protocolConstants)
import Protocol.ProtocolScript (protocolValidatorScript)
import Protocol.Redeemer (PProtocolRedeemer(..))
import Ctl.Internal.Plutus.Types.Transaction as Tx
-- main :: Effect Unit
-- main = example testnetNamiConfig

-- contract :: PProtocol -> Contract () Unit
-- contract protocol = do
--   logInfo' "Running Examples.AlwaysSucceeds"
--   validator <- protocolValidatorScript protocol
--   let vhash = validatorHash validator

--   let scriptAddress = scriptHashAddress vhash Nothing
--   utxos <- utxosAt scriptAddress
--   -- TODO: can't construct datum if no txId 

--   logInfo' "Attempt to lock value"
--   txId <- payToAlwaysSucceeds vhash
--   awaitTxConfirmed txId
--   logInfo' "Tx submitted successfully, Try to spend locked values"
--   spendFromAlwaysSucceeds vhash validator txId

-- example :: ConfigParams () -> Effect Unit
-- example cfg = launchAff_ do
--   runContract cfg contract

payToProtocol :: ValidatorHash -> Tuple PProtocolDatum Value -> Contract () TransactionHash
payToProtocol vhash (Tuple newProtocolDatum value) =
  let
    newPlutusDatum = Datum $ toData newProtocolDatum
    constraints :: TxConstraints Unit Unit
    constraints =
      ( Constraints.mustPayToScript vhash newPlutusDatum Constraints.DatumWitness value
      )
        <> Constraints.mustIncludeDatum newPlutusDatum

    lookups :: Lookups.ScriptLookups PlutusData
    lookups = mempty
  in
    submitTxFromConstraints lookups constraints


makeDatum ∷ PProtocolConfig → UtxoMap → TransactionHash → Contract () PProtocolDatum
makeDatum protocolConfig utxos txId  = do
  Tuple pdata _ <- getDatumAndValue utxos txId
  (currentDatum :: PProtocolDatum) <- liftContractM "can't decode datum" $ fromData pdata
  let protocolConstants = view _protocolConstants currentDatum
  pure $ PProtocolDatum { protocolConstants, protocolConfig }

-- TODO: move to helpers file
getDatumAndValue ∷ UtxoMap → TransactionHash → Contract () (Tuple PlutusData Value)
getDatumAndValue utxos txId= do
  txOutputWithRefScript <- liftContractM "no locked output at address"
    (view _output <$> head (lookupTxHash txId utxos))
  let txOutput = view Tx._output txOutputWithRefScript
  Datum pdata <- liftContractM "no datum in protocol utxo"  
    (outputDatumDatum $ view _datum txOutput)
  let value = view _amount txOutput
  pure $ Tuple pdata value


filterUtxosByThreadToken
  :: CurrencySymbol -> TokenName -> UtxoMap -> Array TransactionUnspentOutput
filterUtxosByThreadToken currency tokenName utxos =
  let getValue = snd >>> unwrap >>>  _.output >>> unwrap >>> _.amount
      getTokenAmount v = valueOf v currency tokenName
  in map (\(input /\ output) -> TransactionUnspentOutput { input, output })
    $ filter (getValue >>> getTokenAmount >>> eq (BigInt.fromInt 1))
    $
      toUnfoldable utxos

getOnlyOneUtxo :: Array TransactionUnspentOutput -> Contract () TransactionUnspentOutput
getOnlyOneUtxo [] = throwContractError "no utxos with given thread token on script"
getOnlyOneUtxo [x] = pure x
getOnlyOneUtxo _ = throwContractError "many utxos with given thread token on script"

spendProtocol
  :: PProtocol 
  -> PProtocolDatum
  -> PProtocolConfig
  -> ValidatorHash
  -> TransactionHash
  -> Contract () Unit
spendProtocol protocol newProtocolDatum protocolConfig vhash txId = do
  let scriptAddress = scriptHashAddress vhash Nothing
  utxos <- utxosAt scriptAddress
  txInput <- liftContractM "no locked output at address"
    (view _input <$> head (lookupTxHash txId utxos))
  let uddateProtocolRedeemer = Redeemer $ toData $ PUpdateProtocolConfig protocolConfig
  let newPlutusDatum = Datum $ toData newProtocolDatum
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
