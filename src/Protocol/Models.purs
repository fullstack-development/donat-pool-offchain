module Protocol.Models where

import Contract.Prelude

import Contract.Value (CurrencySymbol, TokenName)
import Contract.Address (PaymentPubKeyHash)

newtype PProtocol = PProtocol
  { managerPkh :: PaymentPubKeyHash
  , protocolCurrency :: CurrencySymbol
  , protocolTokenName :: TokenName
  }

derive newtype instance Show PProtocol
