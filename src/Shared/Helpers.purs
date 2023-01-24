module Shared.Helpers where

import Contract.Prelude

import Contract.Monad (Contract, liftContractM)
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.Scripts (MintingPolicy)
import Contract.Transaction
  ( TransactionInput
  , TransactionOutputWithRefScript
  )
import Ctl.Internal.Plutus.Types.Transaction (_amount, _output)
import Contract.Value as Value
import Data.Array (filter) as Array
import Data.BigInt (fromInt, BigInt)
import Data.Lens.Getter ((^.))
import Data.Rational ((%), Ratio)

mkTokenName :: forall (r :: Row Type). String -> Contract r Value.TokenName
mkTokenName =
  liftContractM "Cannot make token name"
    <<< (Value.mkTokenName <=< byteArrayFromAscii)

mkCurrencySymbol :: forall (r :: Row Type). Contract r MintingPolicy -> Contract r (MintingPolicy /\ Value.CurrencySymbol)
mkCurrencySymbol policy = do
  mp <- policy
  cs <- liftContractM "Cannot get cs" $ Value.scriptCurrencySymbol mp
  pure (mp /\ cs)

-- NOTE: Nami wallet doesn't allow to spend the collateral UTxO - a special UTxO with 5 Ada balance.
-- As this UTxO doesn't have any special flags, we get the first UTxO with balance non equal to 5 Ada
adaCollateralValue :: Value.Value
adaCollateralValue = Value.singleton Value.adaSymbol Value.adaToken (fromInt 5000000)

checkNonCollateral :: Tuple TransactionInput TransactionOutputWithRefScript -> Boolean
checkNonCollateral (Tuple _ txOutWithRef) =
  let
    utxoValue = (txOutWithRef ^. _output) ^. _amount
  in
    utxoValue /= adaCollateralValue

filterNonCollateral
  :: Array (Tuple TransactionInput TransactionOutputWithRefScript)
  -> Array (Tuple TransactionInput TransactionOutputWithRefScript)
filterNonCollateral = Array.filter checkNonCollateral

mkRational :: Tuple Int Int -> Maybe (Ratio BigInt)
mkRational (Tuple num den) =
  if den == 0 then Nothing
  else Just (fromInt num % fromInt den)
