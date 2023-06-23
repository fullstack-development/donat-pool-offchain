module Ext.Contract.Value where

import Contract.Prelude

import Contract.Monad (Contract, liftContractM)
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.Scripts (MintingPolicy)
import Contract.Value as Value
import Ctl.Internal.Types.ByteArray (byteArrayToHex, ByteArray(..))
import Data.Array (filter) as Array
import Data.TextDecoder (decodeUtf8)

mkTokenName :: String -> Maybe Value.TokenName
mkTokenName = Value.mkTokenName <=< byteArrayFromAscii

runMkTokenName :: forall (r :: Row Type). String -> Contract Value.TokenName
runMkTokenName = liftContractM "Cannot make token name" <<< mkTokenName

mkCurrencySymbol :: forall (r :: Row Type). Contract MintingPolicy -> Contract (MintingPolicy /\ Value.CurrencySymbol)
mkCurrencySymbol policy = do
  mp <- policy
  cs <- liftContractM "Cannot get cs" $ Value.scriptCurrencySymbol mp
  pure (mp /\ cs)

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

currencySymbolToString :: Value.CurrencySymbol -> String
currencySymbolToString = byteArrayToHex <<< Value.getCurrencySymbol

tokenNameToString :: Value.TokenName -> Maybe String
tokenNameToString tn =
  let
    (ByteArray tnBytes) = Value.getTokenName tn
  in
    either (const Nothing) Just $ decodeUtf8 tnBytes
