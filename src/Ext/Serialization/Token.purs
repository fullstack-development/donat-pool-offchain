module Ext.Serialization.Token where

import Contract.Prelude
import Contract.Monad (Contract, liftContractM)
import Contract.Value as Value
import Ctl.Internal.Plutus.Types.CurrencySymbol (CurrencySymbol, mkCurrencySymbol)
import Ctl.Internal.Types.ByteArray (byteArrayFromAscii, hexToByteArray)

deserializeCurrency ∷ String → Contract CurrencySymbol
deserializeCurrency currencyString =
  liftContractM "Impossible to get protocol currency symbol" $
    (hexToByteArray currencyString >>= mkCurrencySymbol)

deserializeTokenName :: String -> Contract Value.TokenName
deserializeTokenName tokenName =
  liftContractM "Impossible to get protocol token name" $
    (byteArrayFromAscii tokenName >>= Value.mkTokenName)