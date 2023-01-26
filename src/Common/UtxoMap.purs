module Common.UtxoMap where

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
import Ctl.Internal.Plutus.Types.Transaction (_datum, _amount)
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

filterUtxosByThreadToken
  :: CurrencySymbol -> TokenName -> Tx.UtxoMap -> Array TransactionUnspentOutput
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

getOneUtxoByThreadToken :: CurrencySymbol -> TokenName -> Tx.UtxoMap -> Contract () TransactionUnspentOutput
getOneUtxoByThreadToken currency tokenName utxos = getOnlyOneUtxo $ filterUtxosByThreadToken currency tokenName utxos

getDatumAndValue ∷ TransactionUnspentOutput -> Contract () (Tuple PlutusData Value)
getDatumAndValue txUnspentOutput = do
  let txOutput = view Tx._output $ view _output $ txUnspentOutput
  Datum pdata <- liftContractM "no datum in protocol utxo"  
    (outputDatumDatum $ view _datum txOutput)
  let value = view _amount txOutput
  pure $ Tuple pdata value
