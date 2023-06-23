module Shared.Utxo where

import Contract.Prelude

import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData (Datum(..), fromData, class FromData)
import Contract.Transaction (TransactionInput, TransactionOutputWithRefScript, OutputDatum(OutputDatum))
import Contract.Value as Value
import Ctl.Internal.Plutus.Types.Transaction (UtxoMap, _amount, _datum, _output)
import Data.Array (filter, head) as Array
import Data.BigInt (fromInt)
import Data.Lens.Getter ((^.))
import Data.Map as Map

type TokenTuple = Tuple Value.CurrencySymbol Value.TokenName
type UtxoTuple = Tuple TransactionInput TransactionOutputWithRefScript

-- NOTE: Nami wallet doesn't allow to spend the collateral UTxO - a special UTxO with 5 Ada balance.
-- As this UTxO doesn't have any special flags, we get the first UTxO with balance non equal to 5 Ada
adaCollateralValue :: Value.Value
adaCollateralValue = Value.singleton Value.adaSymbol Value.adaToken (fromInt 5000000)

checkNonCollateral :: UtxoTuple -> Boolean
checkNonCollateral (Tuple _ txOutWithRef) =
  let
    utxoValue = (txOutWithRef ^. _output) ^. _amount
  in
    utxoValue /= adaCollateralValue

filterNonCollateral :: Array UtxoTuple -> Array UtxoTuple
filterNonCollateral = Array.filter checkNonCollateral

getNonCollateralUtxo :: UtxoMap -> Contract UtxoMap
getNonCollateralUtxo utxos = do
  let nonCollateralArray = filterNonCollateral (Map.toUnfoldable utxos)
  (Tuple walletTxInput walletTxOutputWitRef) <- liftContractM "Failed to get non collateral utxo" $ Array.head nonCollateralArray
  pure $ Map.singleton walletTxInput walletTxOutputWitRef

checkTokenInUTxO :: TokenTuple -> UtxoTuple -> Boolean
checkTokenInUTxO (Tuple cs tn) (Tuple _ txOutWithRef) =
  let
    utxoValue = (txOutWithRef ^. _output) ^. _amount
  in
    Value.valueOf utxoValue cs tn == one

filterByToken :: TokenTuple -> Array UtxoTuple -> Array UtxoTuple
filterByToken token = Array.filter (checkTokenInUTxO token)

getUtxoByNFT :: String -> TokenTuple -> UtxoMap -> Contract UtxoTuple
getUtxoByNFT scriptName nft utxos =
  liftContractM (scriptName <> " UTxO with given nft not found")
    (Array.head (filterByToken nft $ Map.toUnfoldable utxos))

extractDatumFromUTxO
  :: forall (datum :: Type). FromData datum => UtxoTuple -> Maybe datum
extractDatumFromUTxO (Tuple _ txOutWithRef) =
  case (txOutWithRef ^. _output) ^. _datum of
    OutputDatum (Datum datumData) -> fromData datumData
    _ -> Nothing

extractValueFromUTxO :: UtxoTuple -> Value.Value
extractValueFromUTxO (Tuple _ txOutWithRef) = (txOutWithRef ^. _output) ^. _amount
