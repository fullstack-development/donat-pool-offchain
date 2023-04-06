module Shared.Helpers where

import Contract.Prelude

import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData (Datum(..), fromData, class FromData)
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.Scripts (MintingPolicy)
import Contract.Time (POSIXTime(..))
import Contract.Transaction (TransactionInput, TransactionOutputWithRefScript, OutputDatum(OutputDatum))
import Contract.Value as Value
import Ctl.Internal.Plutus.Types.Transaction (UtxoMap, _amount, _datum, _output)
import Data.Array (filter, head) as Array
import Data.BigInt (fromInt, BigInt)
import Data.BigInt as BigInt
import Data.Lens.Getter ((^.))
import Data.Map as Map
import Data.Rational ((%), Ratio, numerator, denominator)
import Math as Math

type TokenTuple = Tuple Value.CurrencySymbol Value.TokenName
type UtxoTuple = Tuple TransactionInput TransactionOutputWithRefScript

mkTokenName :: String -> Maybe Value.TokenName
mkTokenName = Value.mkTokenName <=< byteArrayFromAscii

runMkTokenName :: forall (r :: Row Type). String -> Contract Value.TokenName
runMkTokenName = liftContractM "Cannot make token name" <<< mkTokenName

mkCurrencySymbol :: forall (r :: Row Type). Contract MintingPolicy -> Contract (MintingPolicy /\ Value.CurrencySymbol)
mkCurrencySymbol policy = do
  mp <- policy
  cs <- liftContractM "Cannot get cs" $ Value.scriptCurrencySymbol mp
  pure (mp /\ cs)

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

getCurrencyByTokenName :: Value.Value -> Value.TokenName -> Maybe Value.CurrencySymbol
getCurrencyByTokenName val tokenName =
  let
    tokens = Value.flattenNonAdaAssets val
  in
    case filterByName tokens of
      [ cs /\ _ /\ _ ] -> Just cs
      _ -> Nothing
  where
  filterByName = Array.filter (\(_ /\ tn /\ _) -> tn == tokenName)

mkRational :: Tuple Int Int -> Maybe (Ratio BigInt)
mkRational (Tuple num den) =
  if den == 0 then Nothing
  else Just (fromInt num % fromInt den)

mkBigIntRational :: Tuple BigInt BigInt -> Maybe (Ratio BigInt)
mkBigIntRational (Tuple num den) =
  if den == fromInt 0 then Nothing
  else Just (num % den)

roundBigIntRatio :: Ratio BigInt -> Maybe BigInt
roundBigIntRatio frac = Math.round >>> BigInt.fromNumber $ bigIntRatioToNumber frac

bigIntRatioToNumber :: Ratio BigInt -> Number
bigIntRatioToNumber x = BigInt.toNumber (numerator x) / BigInt.toNumber (denominator x)

addTimes :: POSIXTime -> POSIXTime -> POSIXTime
addTimes (POSIXTime time1) (POSIXTime time2) = POSIXTime (time1 + time2)
